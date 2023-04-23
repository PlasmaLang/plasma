/*
 * Plasma garbage collector
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#include "pz_common.h"

#include <stdio.h>
#include <string.h>
#include <sys/mman.h>
#include <unistd.h>

#include "pz_util.h"

#include "pz_gc.h"
#include "pz_gc_util.h"

#include "pz_gc.impl.h"
#include "pz_gc_layout.h"
#include "pz_gc_layout.impl.h"

/*
 * Plasma GC
 * ---------
 *
 * We want a GC that provides enough features to meet some MVP-ish goals.  It
 * only needs to be good enough to ensure we recover memory.  It is
 * currently a little bit better than that.
 *
 *  * Mark/Sweep
 *  * Non-moving
 *  * Conservative
 *  * Interior pointers (up to 7 byte offset)
 *  * Block based, each block contains cells of a particular size, a marking
 *    bitmap and free list pointer (the free list is made of unused cell
 *    contents.
 *  * Blocks are allocated from Chunks.  We allocate chunks from the OS.
 *
 * This GC is fairly simple.  There are a few changes we could make to
 * improve it in the medium term:
 *
 *  https://github.com/PlasmaLang/plasma/labels/component%3A%20gc
 *
 * In the slightly longer term we should:
 *
 *  * Use accurate pointer information and test it by adding compaction.
 *
 * In the long term, and with much tweaking, this GC will become the
 * tenured and maybe the tenured/mutable part of a larger GC with more
 * features and improvements.
 */

namespace pz {

/***************************************************************************
 *
 * These procedures will likely move somewhere else, but maybe after some
 * refactoring.
 */

size_t heap_get_usage(const Heap * heap)
{
    return heap->usage();
}

unsigned heap_get_collections(const Heap * heap)
{
    return heap->collections();
}

void heap_set_meta_info(Heap * heap, void * obj, void * meta)
{
    heap->set_meta_info(obj, meta);
}

void * heap_interior_ptr_to_ptr(const Heap * heap, void * ptr)
{
    return heap->interior_ptr_to_ptr(ptr);
}

void * Heap::interior_ptr_to_ptr(void * iptr) const
{
    CellPtrBOP cellb = ptr_to_bop_cell_interior(iptr);
    if (cellb.is_valid()) {
        return cellb.pointer();
    } else {
        CellPtrFit cellf = ptr_to_fit_cell_interior(iptr);
        if (cellf.is_valid()) {
            return cellf.pointer();
        }
    }

    return nullptr;
}

void * heap_meta_info(const Heap * heap, void * obj)
{
    return heap->meta_info(obj);
}

bool ChunkBOP::is_empty() const
{
    for (unsigned i = 0; i < m_wilderness; i++) {
        if (m_blocks[i].is_in_use()) return false;
    }
    return true;
}

bool ChunkFit::is_empty()
{
    CellPtrFit cell = first_cell();
    return !cell.is_allocated() &&
           cell.size() ==
               ((Payload_Bytes - CellPtrFit::CellInfoOffset) / WORDSIZE_BYTES);
}

/***************************************************************************/

Heap::Heap(const Options & options)
    : m_options(options)
    , m_chunk_bop("GC BOP")
    , m_chunk_fit("GC fit")
    , m_threshold(GC_Initial_Threshold)
{}

Heap::~Heap()
{
    assert(!m_chunk_bop.is_mapped());
    assert(!m_chunk_fit.is_mapped());
}

bool Heap::init()
{
    assert(!m_chunk_bop.is_mapped());
    if (m_chunk_bop.allocate(GC_Chunk_Size)) {
        new (m_chunk_bop.ptr()) ChunkBOP(this);
    } else {
        return false;
    }

    assert(!m_chunk_fit.is_mapped());
    if (m_chunk_fit.allocate(GC_Chunk_Size)) {
        new (m_chunk_fit.ptr()) ChunkFit(this);
    } else {
        return false;
    }

    return true;
}

bool Heap::finalise(bool fast)
{
    if (fast) {
        m_chunk_bop.forget();
        m_chunk_fit.forget();
        return true;
    }

    bool result = true;

    if (m_chunk_bop.is_mapped()) {
        if (!m_chunk_bop.release()) {
            result = false;
        }
    }

    if (m_chunk_fit.is_mapped()) {
        // sweeping first ensures we run finalisers.
        m_chunk_fit->sweep(m_options);
        if (!m_chunk_fit.release()) {
            result = false;
        }
    }

    return result;
}

/***************************************************************************/

Block::Block(const Options & options, size_t cell_size_) : m_header(cell_size_)
{
    assert(cell_size_ >= GC_Min_Cell_Size);
    memset(m_header.bitmap, 0, GC_Cells_Per_Block * sizeof(uint8_t));

#if PZ_DEV
    if (options.gc_poison()) {
        memset(m_bytes, Poison_Byte, Payload_Bytes);
    }
#endif

    sweep(options);
}

/***************************************************************************/

size_t Block::usage()
{
    return num_allocated() * size() * WORDSIZE_BYTES;
}

unsigned Block::num_allocated()
{
    unsigned count = 0;

    for (unsigned i = 0; i < num_cells(); i++) {
        CellPtrBOP cell(this, i);
        if (cell.is_allocated()) {
            count++;
        }
    }

    return count;
}

size_t ChunkBOP::usage()
{
    size_t usage = 0;

    for (unsigned i = 0; i < m_wilderness; i++) {
        if (m_blocks[i].is_in_use()) {
            usage += m_blocks[i].usage();
        }
    }

    return usage;
}

size_t ChunkFit::usage()
{
    size_t size = 0;

    CellPtrFit cell = first_cell();
    while (cell.is_valid()) {
        if (cell.is_allocated()) {
            size += cell.size() * WORDSIZE_BYTES + CellPtrFit::CellInfoOffset;
        }
        cell = cell.next_in_chunk();
    }
    return size;
}

/***************************************************************************/

void Heap::set_meta_info(void * obj, void * meta)
{
    CellPtrFit cell = ptr_to_fit_cell(obj);
    assert(cell.is_valid());
    *cell.meta() = meta;
}

void * Heap::meta_info(void * obj) const
{
    CellPtrFit cell = ptr_to_fit_cell(obj);
    assert(cell.is_valid());
    return *cell.meta();
}

}  // namespace pz

/***************************************************************************
 *
 * Check arhitecture assumptions
 */

// 8 bits per byte
static_assert(WORDSIZE_BYTES * 8 == WORDSIZE_BITS, "8 bits in a byte");

// 32 or 64 bit.
static_assert(WORDSIZE_BITS == 64 || WORDSIZE_BITS == 32,
              "Either 32 or 64bit wordsize");
