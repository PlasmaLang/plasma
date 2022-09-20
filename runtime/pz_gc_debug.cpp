/*
 * Plasma garbage collector - validation checks & dumping code.
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2018-2019, 2021-2022 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#include "pz_common.h"

#include <stdio.h>

#include "pz_gc.h"
#include "pz_gc.impl.h"
#include "pz_gc_layout.h"
#include "pz_gc_layout.impl.h"

namespace pz {

void Heap::check_heap() const
{
    assert(m_chunk_bop.is_mapped());

    const_cast<ChunkBOP*>(m_chunk_bop.ptr())->check();
    const_cast<ChunkFit*>(m_chunk_fit.ptr())->check();
}

void ChunkBOP::check()
{
    assert(m_wilderness < GC_Block_Per_Chunk);

    for (unsigned i = 0; i < m_wilderness; i++) {
        m_blocks[i].check();
    }
}

void Block::check()
{
    if (!is_in_use()) return;

    assert(size() >= GC_Min_Cell_Size);
    assert(size() <= Block::Max_Cell_Size);
    assert(num_cells() <= GC_Cells_Per_Block);

    unsigned num_free_ = 0;
    for (unsigned i = 0; i < num_cells(); i++) {
        CellPtrBOP cell(this, i);

        if (!cell.is_allocated()) {
            assert(!cell.is_marked());

            // This is quadratic and should be replaced with an extra bit in the
            // cell header and using that to pass over the cells and the
            // free list once each.
            // https://github.com/PlasmaLang/plasma/issues/202
            assert(is_in_free_list(cell));

            num_free_++;
        } else {
            assert(!is_in_free_list(cell));
        }
    }

    assert(num_free() == num_free_);
    assert(num_cells() == num_free_ + num_allocated());
}

bool Block::is_in_free_list(CellPtrBOP & search)
{
    int cur = m_header.free_list;

    while (cur != Header::Empty_Free_List) {
        assert(cur >= 0);
        CellPtrBOP cell(this, unsigned(cur));
        if (search.index() == cell.index()) {
            return true;
        }
        cur = cell.next_in_list();
    }

    return false;
}

unsigned Block::num_free()
{
    int      cur = m_header.free_list;
    unsigned num = 0;

    while (cur != Header::Empty_Free_List) {
        num++;
        assert(cur >= 0);
        CellPtrBOP cell(this, unsigned(cur));
        cur = cell.next_in_list();
    }

    return num;
}

void ChunkFit::check()
{
    // Check the free list.
    bool free_list_valid = m_header.free_list.is_valid();
    if (free_list_valid) {
        // Right now the free list isn't really a list.
        assert(!m_header.free_list.next_in_list().is_valid());
    }

    CellPtrFit cell = first_cell();
    while (cell.is_valid()) {
        assert(contains_pointer(cell.pointer()));
        cell.check();
        if (!cell.is_allocated()) {
            assert(free_list_valid);
        }

        cell = cell.next_in_chunk();
    }
}

void CellPtrFit::check()
{
    assert(size() < ChunkFit::Payload_Bytes);

    switch (info_ptr()->state) {
        case CS_FREE:
        case CS_ALLOCATED:
        case CS_MARKED:
            break;
        default:
            fprintf(stderr, "Invalid cell state\n");
            abort();
    }
}

/****************************************************************************/

void Heap::print_usage_stats(size_t initial_usage) const
{
    printf("\nHeap usage report\n=================\n");
    printf("Usage: %ldKB -> %ldKB\n", initial_usage / 1024, usage() / 1024);
    m_chunk_bop->print_usage_stats();
    m_chunk_fit->print_usage_stats();
    printf("\n");
}

void ChunkBOP::print_usage_stats() const
{
    printf("\nChunkBOP\n--------\n");
    printf("Num blocks: %d/%ld, %ldKB\n",
           m_wilderness,
           GC_Block_Per_Chunk,
           m_wilderness * GC_Block_Size / 1024);
    for (unsigned i = 0; i < m_wilderness; i++) {
        m_blocks[i].print_usage_stats();
    }
}

void Block::print_usage_stats() const
{
    if (is_in_use()) {
        unsigned cells_used = 0;
        for (unsigned i = 0; i < num_cells(); i++) {
            CellPtrBOP cell(const_cast<Block *>(this), i);
            if (cell.is_allocated()) {
                cells_used++;
            }
        }
        printf("Block for %ld-word objects: %d/%d cells\n",
               size(),
               cells_used,
               num_cells());
    } else {
        printf("Block out of use\n");
    }
}

void ChunkFit::print_usage_stats() const
{
    printf("\nChunkFit\n--------\n");

    unsigned num_allocated = 0;
    unsigned num_cells     = 0;
    size_t   allocated     = 0;

    CellPtrFit cell = const_cast<ChunkFit*>(this)->first_cell();

    while (cell.is_valid()) {
        if (cell.is_allocated()) {
            printf("Cell Allocated %ld\n", cell.size());
            num_allocated++;
            allocated += cell.size();
        } else {
            printf("Cell Free      %ld\n", cell.size());
        }
        num_cells++;
        cell = cell.next_in_chunk();
    }

    printf("%d/%d cells, %ld/%ld words allocated\n",
           num_allocated,
           num_cells,
           allocated,
           Payload_Bytes / WORDSIZE_BYTES);
}

/****************************************************************************/

inline const char * bool_string(bool value)
{
    return value ? "true" : "false";
}

void Heap::print_addr_info(void * addr) const
{
    CellPtrBOP cell_bop = ptr_to_bop_cell(addr);
    if (cell_bop.is_valid()) {
        fprintf(stderr, "Debug: %p is a BOP cell\n", addr);
    } else {
        cell_bop = ptr_to_bop_cell_interior(addr);
        if (cell_bop.is_valid()) {
            std::ptrdiff_t diff =
		    (uint8_t *)cell_bop.pointer() - (uint8_t *)addr;
            fprintf(stderr,
                    "Debug: %p is an interior pointer 0x%lx bytes into a "
                    "BOP cell at %p",
                    addr,
                    diff,
                    cell_bop.pointer());
        }
    }
    if (cell_bop.is_valid()) {
        fprintf(stderr,
                "Debug: Cell is index %d in block %p, for size %ld\n",
                cell_bop.index(),
                cell_bop.block(),
                cell_bop.block()->size());
        fprintf(stderr,
                "Debug: Allocated: %s, Marked: %s\n",
                bool_string(cell_bop.is_allocated()),
                bool_string(cell_bop.is_marked()));
        return;
    }

    CellPtrFit cell_fit = ptr_to_fit_cell(addr);
    if (cell_fit.is_valid()) {
        fprintf(stderr, "Debug: %p is a Fit cell\n", addr);
    } else {
        cell_fit = ptr_to_fit_cell_interior(addr);
        if (cell_fit.is_valid()) {
            std::ptrdiff_t diff =
                    (uint8_t *)cell_fit.pointer() - (uint8_t *)addr;
            fprintf(stderr,
                    "Debug: %p is an interior pointer (0x%lx bytes) to a "
                    "Fit cell at %p",
                    addr,
                    diff,
                    cell_fit.pointer());
        }
    }
    if (cell_fit.is_valid()) {
        fprintf(stderr,
                "Debug: Size %ld, Allocated: %s, Marked: %s\n",
                cell_fit.size(),
                bool_string(cell_fit.is_allocated()),
                bool_string(cell_fit.is_marked()));
        if (*cell_fit.meta()) {
            fprintf(stderr, "Debug: Has meta info at %p\n", *cell_fit.meta());
        }
        return;
    }

    fprintf(stderr, "Debug: %p is not a current GC cell\n", addr);
}

}  // namespace pz
