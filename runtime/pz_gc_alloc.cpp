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
    /*
     * Try the free list
     */
    size_in_words = size_in_words < GC_MIN_CELL_SIZE ? GC_MIN_CELL_SIZE :
        size_in_words;
    LBlock *block = get_free_list(size_in_words);
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

    if (!cell.isValid()) return nullptr;

    #ifdef PZ_DEV
    if (m_options.gc_trace2()) {
        fprintf(stderr, "Allocated %p from free list\n", cell.pointer());
    }
    #endif

    return cell.pointer();
}

LBlock *
Heap::get_free_list(size_t size_in_words)
{
    for (unsigned i = 0; i < GC_LBLOCK_PER_BBLOCK; i++) {
        LBlock *lblock = &(m_bblock->m_blocks[i]);

        // TODO: Appropiriate size?
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

    if (m_bblock->m_wilderness * GC_LBLOCK_SIZE >= m_heap_size)
        return nullptr;

    block = m_bblock->next_block();
    if (!block) return nullptr;

    #ifdef PZ_DEV
    if (m_options.gc_trace()) {
        fprintf(stderr, "Allocated new block for %ld cells\n", size_in_words);
    }
    #endif

    new(block) LBlock(size_in_words);

    return block;
}

LBlock*
BBlock::next_block()
{
    if (m_wilderness >= GC_LBLOCK_PER_BBLOCK)
        return nullptr;

    return &m_blocks[m_wilderness++];
}

CellPtr
LBlock::allocate_cell()
{
    assert(is_in_use());

    for (unsigned i = 0; i < num_cells(); i++) {
        if (0 == (*cell_bits(i) & GC_BITS_ALLOCATED)) {
            assert(*cell_bits(i) == 0);
            *cell_bits(i) = GC_BITS_ALLOCATED;
            return CellPtr(this, i);
        }
    }

    return CellPtr::Invalid();
}

} // namespace pz
