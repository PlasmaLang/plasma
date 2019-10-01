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

/**************************************************************************/

CellPtrBOP::CellPtrBOP(Block *block, unsigned index, void *ptr) :
    CellPtr(ptr, CT_BOP),
    m_block(block), m_index(index) { }

CellPtrBOP::CellPtrBOP(Block *block, unsigned index) :
    CellPtr(block->index_to_pointer(index), CT_BOP),
    m_block(block), m_index(index) { }

bool
Block::is_valid_address(const void *ptr) const
{
    assert(is_in_use());

    return is_in_payload(ptr) &&
        ((reinterpret_cast<size_t>(ptr) -
                reinterpret_cast<size_t>(m_bytes)) %
            (size() * WORDSIZE_BYTES)) == 0;
}

unsigned
Block::index_of(const void *ptr) const
{
    return (reinterpret_cast<size_t>(ptr) -
            reinterpret_cast<size_t>(m_bytes)) /
        (size() * WORDSIZE_BYTES);
}

void **
Block::index_to_pointer(unsigned index)
{
    assert(index < num_cells());

    unsigned offset = index * size() * WORDSIZE_BYTES;
    assert(offset + size() <= Payload_Bytes);

    return reinterpret_cast<void**>(&m_bytes[offset]);
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

/**************************************************************************/

CellPtrFit::CellPtrFit(ChunkFit *chunk, void *ptr) :
    CellPtr(reinterpret_cast<void**>(ptr), CT_FIT),
    m_chunk(chunk)
{
    assert(chunk->contains_pointer(ptr));
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

bool
CellPtrFit::is_valid()
{
    bool res = CellPtr::is_valid();
    if (res) {
        assert(size() > 0);
        // TODO also check flags.
    }
    return res;
}

CellPtrFit
CellPtrFit::next_in_list()
{
    if (*pointer()) {
        return CellPtrFit(m_chunk, *pointer());
    } else {
        return CellPtrFit::Invalid();
    }
}

} // namespace pz

#endif // ! PZ_GC_LAYOUT_IMPL_H

