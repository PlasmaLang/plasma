/*
 * Plasma garbage collector collection procedures
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2019 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#include "pz_common.h"

#include <string.h>

#include "pz_util.h"

#include "pz_gc.h"
#include "pz_gc_util.h"

#include "pz_gc.impl.h"
#include "pz_gc_layout.h"
#include "pz_gc_layout.impl.h"

namespace pz {

void *
Heap::alloc(size_t size_in_words, GCCapability &gc_cap)
{
    assert(size_in_words > 0);

    void *cell;
#ifdef PZ_DEV
    assert(m_in_no_gc_scope == !gc_cap.can_gc());
    if (m_options.gc_zealous() &&
        gc_cap.can_gc() &&
        !is_empty())
    {
        // Force a collect before each allocation in this mode.
        cell = NULL;
    } else
#endif
    {
        cell = try_allocate(size_in_words);
    }
    if (cell == NULL && gc_cap.can_gc()) {
        collect(&gc_cap.tracer());
        cell = try_allocate(size_in_words);
    }

    if (cell == NULL) {
        gc_cap.oom(size_in_words * WORDSIZE_BYTES);
    }

    return cell;
}

void *
Heap::alloc_bytes(size_t size_in_bytes, GCCapability &gc_cap) {
    size_t size_in_words = AlignUp(size_in_bytes, WORDSIZE_BYTES) /
        WORDSIZE_BYTES;

    return alloc(size_in_words, gc_cap);
}

void *
Heap::try_allocate(size_t size_in_words)
{
    static_assert(64 <= Block::Max_Cell_Size);
    if (size_in_words <= 64) {
        return try_small_allocate(size_in_words);
    } else {
        return try_medium_allocate(size_in_words);
    }
}

void *
Heap::try_small_allocate(size_t size_in_words)
{
    if (size_in_words < GC_Min_Cell_Size) {
        size_in_words = GC_Min_Cell_Size;
    } else if (size_in_words <= 16) {
        size_in_words = RoundUp(size_in_words, size_t(2));
    } else {
        size_in_words = RoundUp(size_in_words, size_t(4));
    }

    /*
     * Try the free list
     */
    Block *block = get_block_for_allocation(size_in_words);
    if (!block) {
        block = allocate_block(size_in_words);
        if (!block) {
            #ifdef PZ_DEV
            if (m_options.gc_trace2()) {
                fprintf(stderr, "Heap full for allocation of %ld words\n",
                        size_in_words);
            }
            #endif
            return nullptr;
        }
    }

    CellPtrBOP cell = block->allocate_cell();

    if (!cell.is_valid()) return nullptr;
    
    #ifdef PZ_DEV
    if (m_options.gc_poison()) {
        memset(cell.pointer(), Poison_Byte, block->size() * WORDSIZE_BYTES);
    }

    if (m_options.gc_trace2()) {
        fprintf(stderr, "Allocated %p from free list\n", cell.pointer());
    }
    #endif

    m_usage += block->size() * WORDSIZE_BYTES;

    return cell.pointer();
}

Block *
Heap::get_block_for_allocation(size_t size_in_words)
{
    return m_chunk_bop->get_block_for_allocation(size_in_words);
}

Block *
ChunkBOP::get_block_for_allocation(size_t size_in_words)
{
    for (unsigned i = 0; i < m_wilderness; i++) {
        Block *block = &(m_blocks[i]);

        if (block->is_in_use() && block->size() == size_in_words &&
                !block->is_full())
        {
            return block;
        }
    }

    return nullptr;
}

Block *
Heap::allocate_block(size_t size_in_words)
{
    Block *block;

    if (m_chunk_bop->usage() >= m_max_size)
        return nullptr;

    block = m_chunk_bop->allocate_block();
    if (!block) return nullptr;

    #ifdef PZ_DEV
    if (m_options.gc_trace()) {
        fprintf(stderr, "Allocated new block for %ld cells\n", size_in_words);
    }
    #endif

    new(block) Block(m_options, size_in_words);

    return block;
}

Block*
ChunkBOP::allocate_block()
{
    for (unsigned i = 0; i < m_wilderness; i++) {
        if (!m_blocks[i].is_in_use()) {
#ifdef PZ_DEV
            fprintf(stderr,
                "Running previously-unused code path, "
                "see https://github.com/PlasmaLang/plasma/issues/191\n");
#endif
            return &m_blocks[i];
        }
    }

    if (m_wilderness >= GC_Block_Per_Chunk)
        return nullptr;

    return &m_blocks[m_wilderness++];
}

CellPtrBOP
Block::allocate_cell()
{
    assert(is_in_use());

    if (m_header.free_list < 0)
        return CellPtrBOP::Invalid();

    CellPtrBOP cell(this, m_header.free_list);
    assert(!is_allocated(cell));
    m_header.free_list = cell.next_in_list();
    assert(m_header.free_list == Header::Empty_Free_List ||
            (m_header.free_list < static_cast<int>(num_cells()) &&
            m_header.free_list >= 0));
    allocate(cell);
    return cell;
}

void *
Heap::try_medium_allocate(size_t size_in_words)
{
    CellPtrFit cell = m_chunk_fit->allocate_cell(size_in_words);

#ifdef PZ_DEV
    if (cell.is_valid() && m_options.gc_poison()) {
        memset(cell.pointer(), Poison_Byte, cell.size() * WORDSIZE_BYTES);
    }
#endif

    m_usage += cell.size()*WORDSIZE_BYTES + CellPtrFit::CellInfoOffset;

    return cell.pointer();
}

constexpr size_t CellSplitThreshold = Block::Max_Cell_Size +
    CellPtrFit::CellInfoOffset;

CellPtrFit
ChunkFit::allocate_cell(size_t size_in_words)
{
    CellPtrFit cell = m_header.free_list;

    while (cell.is_valid()) {
        if (cell.size() >= size_in_words) {
            m_header.free_list = cell.next_in_list();

            // Should we split the cell?
            if (cell.size() >= size_in_words + CellSplitThreshold) {
                CellPtrFit new_cell = cell.split(size_in_words);
                new_cell.set_next_in_list(m_header.free_list);
                m_header.free_list = new_cell;
            }

            cell.set_allocated();
            return cell;
        }

        cell = cell.next_in_list();
    }

    return CellPtrFit::Invalid();
}

ChunkFit::ChunkFit() : Chunk(CT_FIT)
{
    CellPtrFit singleCell = first_cell();
    singleCell.set_size((Payload_Bytes - CellPtrFit::CellInfoOffset)
        / WORDSIZE_BYTES);
    singleCell.clear_next_in_list();
    m_header.free_list = singleCell;
}

CellPtrFit
CellPtrFit::split(size_t new_size)
{
    assert(size() >= 1 + CellPtrFit::CellInfoOffset + new_size);
#ifdef PZ_DEV
    void *end_of_cell = next_by_size(size());
#endif

    CellPtrFit new_cell(m_chunk, next_by_size(new_size));
    size_t rem_size = size() - new_size -
        CellPtrFit::CellInfoOffset/WORDSIZE_BYTES;
    set_size(new_size);
    new_cell.set_size(rem_size);

#ifdef PZ_DEV
    assert(new_cell.pointer() == next_in_chunk().pointer());
    assert(end_of_cell == new_cell.next_by_size(new_cell.size()));
#endif

    return new_cell;
}

} // namespace pz
