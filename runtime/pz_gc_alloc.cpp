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

namespace pz {

void *
Heap::alloc(size_t size_in_words, const GCCapability &gc_cap)
{
    assert(size_in_words > 0);

    void *cell;
#ifdef PZ_DEV
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
        if (cell == NULL) {
            fprintf(stderr, "Out of memory, tried to allocate %lu bytes.\n",
                        size_in_words * WORDSIZE_BYTES);
            abort();
        }
    }

    return cell;
}

void *
Heap::alloc_bytes(size_t size_in_bytes, const GCCapability &gc_cap) {
    size_t size_in_words = AlignUp(size_in_bytes, WORDSIZE_BYTES) /
        WORDSIZE_BYTES;

    return alloc(size_in_words, gc_cap);
}

void *
Heap::try_allocate(size_t size_in_words)
{
    if (size_in_words < GC_Min_Cell_Size) {
        size_in_words = GC_Min_Cell_Size;
    } else if (size_in_words <= 16) {
        size_in_words = RoundUp(size_in_words, size_t(2));
    } else {
        size_in_words = RoundUp(size_in_words, size_t(4));
    }

    if (size_in_words > LBlock::Max_Cell_Size) {
        fprintf(stderr, "Allocation %ld too big for GC\n", size_in_words);
        abort();
    }

    /*
     * Try the free list
     */
    LBlock *block = get_lblock_for_allocation(size_in_words);
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

    CellPtr cell = block->allocate_cell();

    if (!cell.is_valid()) return nullptr;

    #ifdef PZ_DEV
    if (m_options.gc_trace2()) {
        fprintf(stderr, "Allocated %p from free list\n", cell.pointer());
    }
    #endif

    return cell.pointer();
}

LBlock *
Heap::get_lblock_for_allocation(size_t size_in_words)
{
    return m_bblock->get_lblock_for_allocation(size_in_words);
}

LBlock *
BBlock::get_lblock_for_allocation(size_t size_in_words)
{
    for (unsigned i = 0; i < m_wilderness; i++) {
        LBlock *lblock = &(m_blocks[i]);

        if (lblock->is_in_use() && lblock->size() == size_in_words &&
                !lblock->is_full())
        {
            return lblock;
        }
    }

    return nullptr;
}

LBlock *
Heap::allocate_block(size_t size_in_words)
{
    LBlock *block;

    if (m_bblock->size() >= m_max_size)
        return nullptr;

    block = m_bblock->allocate_block();
    if (!block) return nullptr;

    #ifdef PZ_DEV
    if (m_options.gc_trace()) {
        fprintf(stderr, "Allocated new block for %ld cells\n", size_in_words);
    }
    #endif

    new(block) LBlock(m_options, size_in_words);

    return block;
}

LBlock*
BBlock::allocate_block()
{
    for (unsigned i = 0; i < m_wilderness; i++) {
        if (!m_blocks[i].is_in_use()) {
            fprintf(stderr,
                "Running previously-unused code path, " 
                "see https://github.com/PlasmaLang/plasma/issues/191\n");
            return &m_blocks[i];
        }
    }

    if (m_wilderness >= GC_LBlock_Per_BBlock)
        return nullptr;

    return &m_blocks[m_wilderness++];
}

CellPtr
LBlock::allocate_cell()
{
    assert(is_in_use());

    if (m_header.free_list < 0)
        return CellPtr::Invalid();

    CellPtr cell(this, m_header.free_list);
    assert(!is_allocated(cell));
    m_header.free_list = cell.next_in_list();
    assert(m_header.free_list == Header::Empty_Free_List ||
            (m_header.free_list < static_cast<int>(num_cells()) &&
            m_header.free_list >= 0));
    allocate(cell);
    return cell;
}

} // namespace pz
