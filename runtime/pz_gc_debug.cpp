/*
 * Plasma garbage collector - validation checks & dumping code.
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2018-2019 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#include "pz_common.h"

#include <stdio.h>

#include "pz_gc.h"
#include "pz_gc.impl.h"
#include "pz_gc_layout.h"
#include "pz_gc_layout.impl.h"

namespace pz {

void
Heap::check_heap() const
{
    assert(s_page_size != 0);
    assert(m_chunk_bop != NULL);
    assert(m_max_size >= s_page_size);
    assert(m_max_size % s_page_size == 0);
    assert(m_max_size % GC_Block_Size == 0);

    m_chunk_bop->check();
}

void
ChunkBOP::check()
{
    assert(m_wilderness < GC_Block_Per_Chunk);

    for (unsigned i = 0; i < m_wilderness; i++) {
        m_blocks[i].check();
    }
}

void
Block::check()
{
    if (!is_in_use()) return;

    assert(size() >= GC_Min_Cell_Size);
    assert(size() <= Block::Max_Cell_Size);
    assert(num_cells() <= GC_Cells_Per_Block);

    unsigned num_free_ = 0;
    for (unsigned i = 0; i < num_cells(); i++) {
        CellPtrBOP cell(this, i);

        if (!is_allocated(cell)) {
            assert(!is_marked(cell));

            assert(is_in_free_list(cell));

            num_free_++;
        } else {
            assert(!is_in_free_list(cell));
        }
    }

    assert(num_free() == num_free_);
}

bool
Block::is_in_free_list(CellPtrBOP &search)
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

unsigned
Block::num_free()
{
    int cur = m_header.free_list;
    unsigned num = 0;

    while (cur != Header::Empty_Free_List) {
        num++;
        assert(cur >= 0);
        CellPtrBOP cell(this, unsigned(cur));
        cur = cell.next_in_list();
    }

    return num;
}

/****************************************************************************/

void
Heap::print_usage_stats() const
{
    m_chunk_bop->print_usage_stats();
}

void
ChunkBOP::print_usage_stats() const
{
    printf("\nChunkBOP\n--------\n");
    printf("Num blocks: %d/%ld, %ldKB\n",
        m_wilderness, GC_Block_Per_Chunk,
        m_wilderness * GC_Block_Size / 1024);
    for (unsigned i = 0; i < m_wilderness; i++) {
        m_blocks[i].print_usage_stats();
    }
}

void
Block::print_usage_stats() const
{
    if (is_in_use()) {
        unsigned cells_used = 0;
        for (unsigned i = 0; i < num_cells(); i++) {
            CellPtrBOP cell(const_cast<Block*>(this), i);
            if (is_allocated(cell)) {
                cells_used++;
            }
        }
        printf("Block for %ld-word objects: %d/%d cells\n",
            size(), cells_used, num_cells());
    } else {
        printf("Block out of use\n");
    }
}

} // namespace pz
