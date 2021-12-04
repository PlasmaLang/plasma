/*
 * Plasma garbage collector collection procedures
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2019-2021 Plasma Team
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

void * Heap::alloc(size_t size_in_words, GCCapability & gc_cap, AllocOpts opts)
{
    assert(size_in_words > 0);

    bool should_collect = false;

#ifdef PZ_DEV
    if (m_options.gc_zealous() && gc_cap.can_gc() && !is_empty()) {
        // Force a collect before each allocation in this mode.
        should_collect = true;
    }
#endif

    if (gc_cap.can_gc() &&
        m_usage + size_in_words * WORDSIZE_BYTES > m_threshold)
    {
        should_collect = true;
    }

    if (should_collect) {
        collect(&gc_cap.tracer());
    }

    void * cell = try_allocate(size_in_words, opts);
    if (cell) {
        return cell;
    }

    if (gc_cap.can_gc() && !should_collect) {
        collect(&gc_cap.tracer());
        cell = try_allocate(size_in_words, opts);
        if (cell) {
            return cell;
        }
    }

    gc_cap.oom(size_in_words * WORDSIZE_BYTES);
    return nullptr;
}

void * Heap::alloc_bytes(size_t size_in_bytes, GCCapability & gc_cap,
                         AllocOpts opts)
{
    size_t size_in_words =
        AlignUp(size_in_bytes, WORDSIZE_BYTES) / WORDSIZE_BYTES;

    return alloc(size_in_words, gc_cap, opts);
}

void * Heap::try_allocate(size_t size_in_words, AllocOpts opts)
{
    switch (opts) {
        case NORMAL:
            if (size_in_words <= GC_Small_Alloc_Threshold) {
                return try_small_allocate(size_in_words);
            } else {
                return try_medium_allocate(size_in_words, opts);
            }
        case META:
        case TRACE: {
            return try_medium_allocate(size_in_words, opts);
        }
        default:
            fprintf(stderr, "Allocation options is invalid\n");
            abort();
    }
}

void * Heap::try_small_allocate(size_t size_in_words)
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
    Block * block = get_block_for_allocation(size_in_words);
    if (!block) {
        block = allocate_block(size_in_words);
        if (!block) {
#ifdef PZ_DEV
            if (m_options.gc_trace2()) {
                fprintf(stderr,
                        "Heap full for allocation of %ld words\n",
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

Block * Heap::get_block_for_allocation(size_t size_in_words)
{
    return m_chunk_bop->get_block_for_allocation(size_in_words);
}

Block * ChunkBOP::get_block_for_allocation(size_t size_in_words)
{
    for (unsigned i = 0; i < m_wilderness; i++) {
        Block * block = &(m_blocks[i]);

        if (block->is_in_use() && block->size() == size_in_words &&
                !block->is_full())
        {
            return block;
        }
    }

    return nullptr;
}

Block * Heap::allocate_block(size_t size_in_words)
{
    Block * block;

    block = m_chunk_bop->allocate_block();
    if (!block) return nullptr;

#ifdef PZ_DEV
    if (m_options.gc_trace()) {
        fprintf(stderr, "Allocated new block for %ld cells\n", size_in_words);
    }
#endif

    new (block) Block(m_options, size_in_words);

    return block;
}

Block * ChunkBOP::allocate_block()
{
    for (unsigned i = 0; i < m_wilderness; i++) {
        if (!m_blocks[i].is_in_use()) {
            // TODO https://github.com/PlasmaLang/plasma/issues/191
            return &m_blocks[i];
        }
    }

    if (m_wilderness >= GC_Block_Per_Chunk) return nullptr;

    return &m_blocks[m_wilderness++];
}

CellPtrBOP Block::allocate_cell()
{
    assert(is_in_use());

    if (m_header.free_list < 0) return CellPtrBOP::Invalid();

    CellPtrBOP cell(this, m_header.free_list);
    assert(!cell.is_allocated());
    m_header.free_list = cell.next_in_list();
    assert(m_header.free_list == Header::Empty_Free_List ||
           (m_header.free_list < static_cast<int>(num_cells()) &&
            m_header.free_list >= 0));
    cell.allocate();
    return cell;
}

void * Heap::try_medium_allocate(size_t size_in_words, AllocOpts opts)
{
    CellPtrFit cell = m_chunk_fit->allocate_cell(size_in_words);

#ifdef PZ_DEV
    if (cell.is_valid() && m_options.gc_poison()) {
        memset(cell.pointer(), Poison_Byte, cell.size() * WORDSIZE_BYTES);
    }
#endif

    /*
     * TODO: we could allow both meta and trace at the same time, there's
     * currently no limitation for that since we're using C++ virtual
     * methods to find the trace code and finaliser.
     */
    *cell.meta() = nullptr;
    switch (opts) {
        case NORMAL:
        case META:
            break;
        case TRACE:
            cell.set_flags(CellPtrFit::CF_TRACE_AND_FINALISE);
            break;
    }

    m_usage += cell.size() * WORDSIZE_BYTES + CellPtrFit::CellInfoOffset;

    return cell.pointer();
}

constexpr size_t CellSplitThreshold =
    Block::Max_Cell_Size + CellPtrFit::CellInfoOffset;

CellPtrFit ChunkFit::allocate_cell(size_t size_in_words)
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

ChunkFit::ChunkFit(Heap * heap) : Chunk(heap, CT_FIT)
{
    CellPtrFit singleCell = first_cell();
    singleCell.init((Payload_Bytes - CellPtrFit::CellInfoOffset) /
                    WORDSIZE_BYTES);
    m_header.free_list = singleCell;
}

CellPtrFit CellPtrFit::split(size_t new_size)
{
    assert(size() >= 1 + CellPtrFit::CellInfoOffset + new_size);
#ifdef PZ_DEV
    void * end_of_cell = next_by_size(size());
#endif

    CellPtrFit new_cell(m_chunk, next_by_size(new_size));
    size_t     rem_size =
        size() - new_size - CellPtrFit::CellInfoOffset / WORDSIZE_BYTES;
    set_size(new_size);
    new_cell.init(rem_size);

#ifdef PZ_DEV
    assert(new_cell.pointer() == next_in_chunk().pointer());
    assert(end_of_cell == new_cell.next_by_size(new_cell.size()));

    check();
    new_cell.check();
#endif

    return new_cell;
}

}  // namespace pz
