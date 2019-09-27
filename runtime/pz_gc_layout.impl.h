/*
 * Plasma garbage collector memory layout
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2019 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#ifndef PZ_GC_LAYOUT_IMPL_H
#define PZ_GC_LAYOUT_IMPL_H

#include "pz_gc_layout.h"

namespace pz {

/*
 * Definitions for some inline functions that must be defined here after
 * the class definitions.
 */

inline Block *
ptr_to_block(void *ptr)
{
    return reinterpret_cast<Block*>(
        reinterpret_cast<uintptr_t>(ptr) & GC_Block_Mask);
}

CellPtrBOP::CellPtrBOP(Block *block, unsigned index, void *ptr) :
    CellPtr(ptr, CT_BOP),
    m_block(block), m_index(index) { }

CellPtrBOP::CellPtrBOP(Block *block, unsigned index) :
    CellPtr(block->index_to_pointer(index), CT_BOP),
    m_block(block), m_index(index) { }

CellPtrFit::CellPtrFit(ChunkFit *chunk, void *ptr) :
    CellPtr(reinterpret_cast<void**>(ptr), CT_FIT),
    m_chunk(chunk)
{
    assert(chunk->contains_pointer(ptr));
}

bool
Heap::is_heap_address(void *ptr) const
{
    if (m_chunk_bop->contains_pointer(ptr)) {
        Block *block = m_chunk_bop->ptr_to_block(ptr);
        if (!block) return false;
        if (!block->is_in_use()) return false;
        return block->is_in_payload(ptr);
    } else if (m_chunk_fit->contains_pointer(ptr)) {
        return true;
    } else {
        return false;
    }
}

Block *
ChunkBOP::ptr_to_block(void *ptr)
{
    if (ptr >= &m_blocks[0] && ptr < &m_blocks[m_wilderness]) {
        return pz::ptr_to_block(ptr);
    } else {
        return nullptr;
    }
}

CellPtrBOP
Heap::ptr_to_bop_cell(void *ptr) const
{
    if (m_chunk_bop->contains_pointer(ptr)) {
        Block *block = m_chunk_bop->ptr_to_block(ptr);
        if (block && block->is_in_use() && block->is_valid_address(ptr)) {
            return CellPtrBOP(block, block->index_of(ptr), ptr);
        } else {
            return CellPtrBOP::Invalid();
        }
    } else {
        return CellPtrBOP::Invalid();
    }
}

CellPtrBOP
Heap::ptr_to_bop_cell_interior(void *ptr) const
{
    if (m_chunk_bop->contains_pointer(ptr)) {
        Block *block = m_chunk_bop->ptr_to_block(ptr);
        if (block && block->is_in_use()) {
            // Compute index then re-compute pointer to find the true
            // beginning of the cell.
            unsigned index = block->index_of(ptr);
            ptr = block->index_to_pointer(index);
            return CellPtrBOP(block, index, ptr);
        } else {
            return CellPtrBOP::Invalid();
        }
    } else {
        return CellPtrBOP::Invalid();
    }
}

CellPtrFit
Heap::ptr_to_fit_cell(void *ptr) const
{
    if (m_chunk_fit->contains_pointer(ptr)) {
        // TODO Speed up this search with a crossing-map.
        for (CellPtrFit cell = m_chunk_fit->first_cell(); cell.is_valid();
                cell = cell.next_in_chunk())
        {
            if (cell.pointer() == ptr) {
                return cell;
            } else if (cell.pointer() > ptr) {
                // The pointer points into the middle of a cell.
                return CellPtrFit::Invalid();
            }
        }
        return CellPtrFit::Invalid();
    } else {
        return CellPtrFit::Invalid();
    }
}

CellPtrFit
Heap::ptr_to_fit_cell_interior(void *ptr) const
{
    if (m_chunk_fit->contains_pointer(ptr)) {
        // TODO Speed up this search with a crossing-map.
        CellPtrFit prev = CellPtrFit::Invalid();
        for (CellPtrFit cell = m_chunk_fit->first_cell(); cell.is_valid();
                cell = cell.next_in_chunk())
        {
            if (cell.pointer() == ptr) {
                return cell;
            } else if (cell.pointer() > ptr) {
                if (prev.is_valid()) {
                    return prev;
                } else {
                    return CellPtrFit::Invalid();
                }
            }

            prev = cell;
        }
        return CellPtrFit::Invalid();
    } else {
        return CellPtrFit::Invalid();
    }
}

void*
CellPtrFit::next_by_size(size_t size) {
    return reinterpret_cast<void*>(pointer()) + size*WORDSIZE_BYTES +
        CellInfoOffset;
}

CellPtrFit
CellPtrFit::next_in_chunk() {
    assert(size() > 0);
    void *next = next_by_size(size());
    if (m_chunk->contains_pointer(next)) { 
        return CellPtrFit(m_chunk, next);
    } else {
        return CellPtrFit::Invalid();
    }
}

} // namespace pz

#endif // ! PZ_GC_LAYOUT_IMPL_H

