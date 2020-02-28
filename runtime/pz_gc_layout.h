/*
 * Plasma garbage collector memory layout
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2019 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#ifndef PZ_GC_LAYOUT_H
#define PZ_GC_LAYOUT_H

#include "pz_gc.h"
#include "pz_gc.impl.h"

namespace pz {

constexpr uint8_t Poison_Byte = 0x77;

/*
 * These must be a power-of-two and mmap must align to them. 4K is the
 * default.
 */
static const unsigned GC_Block_Log = 13;
static const size_t GC_Block_Size = 1 << (GC_Block_Log - 1);
static const size_t GC_Block_Mask = ~(GC_Block_Size - 1);
static const unsigned GC_Min_Cell_Size = 2;
static const unsigned GC_Cells_Per_Block = GC_Block_Size /
    (GC_Min_Cell_Size * WORDSIZE_BYTES);

/*
 * GC_Chunk_Size is also a power of two and is therefore a multiple of
 * GC_Block_Size.  4MB is the default.
 */
static const unsigned GC_Chunk_Log = 23;
static const size_t GC_Chunk_Size = 1 << (GC_Chunk_Log - 1);
static const size_t GC_Block_Per_Chunk =
        (GC_Chunk_Size / GC_Block_Size) - 1;

#if PZ_DEV
// Set this low for testing.
static const size_t GC_Initial_Threshold = 8 * GC_Block_Size;
#else
static const size_t GC_Initial_Threshold = 64 * GC_Block_Size;
#endif
static const float GC_Threshold_Factor = 1.5f;

// The threshold for small allocations in words.  Allocations of less than
// this many words are small allocations.
static const size_t GC_Small_Alloc_Threshold = 64;

static_assert(GC_Chunk_Size > GC_Block_Size,
        "Chunks must be larger than blocks");

/*
 * The heap is made out of blocks and chunks.  A chunk contains multiple
 * blocks, which each contain multiple cells.
 */

enum CellType {
    // Used for Invalid cells or unallocated chunks.
    CT_INVALID,

    CT_BOP,
    CT_FIT
};

/*
 * This class should be used by-value as a reference to a cell.
 */
class CellPtr {
  protected:
    void**      m_ptr;
    CellType    m_type;

    constexpr CellPtr() : m_ptr(nullptr), m_type(CT_INVALID) { }

  public:
    constexpr explicit CellPtr(void* ptr, CellType type) :
        m_ptr(static_cast<void**>(ptr)), m_type(type) { }

    void** pointer() { return m_ptr; }

    bool is_valid() const { return m_ptr != nullptr; }
    bool is_bop_cell() const { return m_type == CT_BOP; }
    bool is_fit_cell() const { return m_type == CT_FIT; }
};

/*
 * Chunks
 */
class Chunk {
  private:
    Chunk(const Chunk&) = delete;
    void operator=(const Chunk&) = delete;

    Chunk() : m_type(CT_INVALID) { }

  protected:
    // Currently unused, so make it protected to avoid a compiler warning.
    CellType m_type;
    Chunk(CellType type) : m_type(type) { }

  public:
    static Chunk* new_chunk();
    bool destroy();

    ChunkBOP* initalise_as_bop();
    ChunkFit* initalise_as_fit();

    /*
     * True if this pointer lies within the allocated part of this chunk.
     */
    bool contains_pointer(void *ptr) const {
        return ptr >= this &&
            ptr < (reinterpret_cast<const uint8_t*>(this) + GC_Chunk_Size);
    };
};

} // namespace pz

#include "pz_gc_layout_bop.h"
#include "pz_gc_layout_fit.h"

namespace pz {

static_assert(GC_Small_Alloc_Threshold <= Block::Max_Cell_Size,
        "The small alloc threshold must be less than the maximum cell size");

} // namespace pz

#endif // ! PZ_GC_LAYOUT_H
