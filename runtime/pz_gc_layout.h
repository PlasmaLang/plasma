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
 * The heap is made out of little blocks and big blocks.  A big block
 * contains multiple little blocks, which each contain multiple cells.
 */

/*
 * This class should be used by-value as a reference to a cell.
 */
class CellPtr {
  private:
    void**      m_ptr;
    LBlock*     m_block;
    unsigned    m_index;

    constexpr CellPtr() : m_ptr(nullptr), m_block(nullptr), m_index(0) { }

    int* free_list_data() {
        return reinterpret_cast<int*>(m_ptr);
    }

  public:
    inline explicit CellPtr(LBlock* block, unsigned index);
    inline explicit CellPtr(void* ptr);

    bool is_valid() const { return m_ptr != nullptr; }
    LBlock* lblock() const { return m_block; }
    unsigned index() const { return m_index; }
    void** pointer() { return m_ptr; }

    void set_next_in_list(int next) {
        *free_list_data() = next;
    }
    int next_in_list() {
        return *free_list_data();
    }

    static constexpr CellPtr Invalid() { return CellPtr(); }
};

/*
 * Little blocks - LBlock
 *
 * These must be a power-of-two and mmap must align to them. 4K is the
 * default.
 */
static const unsigned GC_LBlock_Log = 13;
static const size_t GC_LBlock_Size = 1 << (GC_LBlock_Log - 1);
static const size_t GC_LBlock_Mask = ~(GC_LBlock_Size - 1);
static const unsigned GC_Min_Cell_Size = 2;
static const unsigned GC_Cells_Per_LBlock = GC_LBlock_Size /
    (GC_Min_Cell_Size * WORDSIZE_BYTES);

class LBlock {
  private:
    struct Header {
        const static size_t Block_Empty = 0;
        size_t    block_type_or_size;

        const static int Empty_Free_List = -1;
        int       free_list;

        // Really a bytemap.
        uint8_t   bitmap[GC_Cells_Per_LBlock];

        explicit Header(size_t cell_size_) :
            block_type_or_size(cell_size_),
            free_list(Empty_Free_List)
        {
            assert(cell_size_ >= GC_Min_Cell_Size);
        }
        Header() {}
    };

    Header m_header;

  public:
    static constexpr size_t Header_Bytes =
        RoundUp<size_t>(sizeof(m_header), WORDSIZE_BYTES);
    static constexpr size_t Payload_Bytes =
        GC_LBlock_Size - Header_Bytes;
    static constexpr size_t Max_Cell_Size =
        Payload_Bytes / WORDSIZE_BYTES;

  private:
    alignas(WORDSIZE_BYTES)
    uint8_t     m_bytes[Payload_Bytes];

  public:
    explicit LBlock(const Options &options, size_t cell_size_);

    // This constructor won't touch any memory and can be used to construct
    // uninitialised LBlocks within BBlocks.
    LBlock() {}

    LBlock(const LBlock&) = delete;
    void operator=(const LBlock&) = delete;

    // Size in words.
    size_t size() const {
        assert(is_in_use());
        return m_header.block_type_or_size;
    }

    unsigned num_cells() const {
        unsigned num = Payload_Bytes / (size() * WORDSIZE_BYTES);
        assert(num <= GC_Cells_Per_LBlock);
        return num;
    }

    bool is_in_payload(const void *ptr) const {
        return ptr >= m_bytes && ptr < &m_bytes[Payload_Bytes];
    }

    bool is_valid_address(const void *ptr) const {
        assert(is_in_use());

        return is_in_payload(ptr) &&
            ((reinterpret_cast<size_t>(ptr) -
                    reinterpret_cast<size_t>(m_bytes)) %
                (size() * WORDSIZE_BYTES)) == 0;
    }

    unsigned index_of(const void *ptr) const {
        assert(is_valid_address(ptr));

        return (reinterpret_cast<size_t>(ptr) -
                reinterpret_cast<size_t>(m_bytes)) /
            (size() * WORDSIZE_BYTES);
    }

    void ** index_to_pointer(unsigned index) {
        assert(index < num_cells());

        unsigned offset = index * size() * WORDSIZE_BYTES;
        assert(offset + size() <= Payload_Bytes);

        return reinterpret_cast<void**>(&m_bytes[offset]);
    }

  private:
    /*
     * TODO: Can the const and non-const versions somehow share an
     * implementation?  Would that actually save any code lines?
     */
    const uint8_t * cell_bits(const CellPtr &cell) const {
        assert(cell.is_valid() && cell.lblock() == this);
        return cell_bits(cell.index());
    }

    uint8_t * cell_bits(const CellPtr &cell) {
        assert(cell.is_valid() && cell.lblock() == this);
        return cell_bits(cell.index());
    }

    const uint8_t * cell_bits(unsigned index) const {
        assert(index < num_cells());
        return &(m_header.bitmap[index]);
    }

    uint8_t * cell_bits(unsigned index) {
        assert(index < num_cells());
        return &(m_header.bitmap[index]);
    }

    constexpr static uintptr_t GC_Bits_Allocated = 0x01;
    constexpr static uintptr_t GC_Bits_Marked    = 0x02;

  public:
    bool is_allocated(CellPtr &cell) const {
        return *cell_bits(cell) & GC_Bits_Allocated;
    }

    bool is_marked(CellPtr &cell) const {
        return *cell_bits(cell) & GC_Bits_Marked;
    }

    void allocate(CellPtr &cell) {
        assert(*cell_bits(cell) == 0);
        *cell_bits(cell) = GC_Bits_Allocated;
    }

    void unallocate(CellPtr &cell) {
        assert(!is_marked(cell));
        *cell_bits(cell) = 0;
    }

    void mark(CellPtr &cell) {
        assert(is_allocated(cell));
        *cell_bits(cell) = GC_Bits_Allocated | GC_Bits_Marked;
    }

    void unmark(CellPtr &cell) {
        assert(is_allocated(cell));
        *cell_bits(cell) = GC_Bits_Allocated;
    }

    bool is_full() const {
        assert(is_in_use());
        return m_header.free_list == Header::Empty_Free_List;
    }

    bool is_in_use() const {
        return m_header.block_type_or_size != Header::Block_Empty;
    }

    // Returns true if the entire block is empty and may be reclaimed.
    bool sweep(const Options &options);

    void make_unused();

    CellPtr allocate_cell();

#ifdef PZ_DEV
    void print_usage_stats() const;

    void check();

  private:
    bool is_in_free_list(CellPtr &cell);

    // Calculate the number of free cells via the free list length.
    unsigned num_free();
#endif
};

static_assert(sizeof(LBlock) == GC_LBlock_Size);

/*
 * Big blocks - BBlocks
 *
 * GC_BBlock_Size is also a power of two and is therefore a multiple of
 * GC_LBlock_Size.  4MB is the default.
 */
static const unsigned GC_BBlock_Log = 23;
static const size_t GC_BBlock_Size = 1 << (GC_BBlock_Log - 1);
static const size_t GC_LBlock_Per_BBlock =
        (GC_BBlock_Size / GC_LBlock_Size) - 1;

class BBlock {
  private:
    uint32_t    m_wilderness;

    alignas(GC_LBlock_Size)
    LBlock      m_blocks[GC_LBlock_Per_BBlock];

    BBlock() : m_wilderness(0) { }

    BBlock(const BBlock&) = delete;
    void operator=(const BBlock&) = delete;

  public:
    static BBlock* new_bblock();

    // Get an unused block.
    LBlock* free_block();

    /*
     * The size of the allocated portion of this BBlock.
     */
    size_t size() const;

    bool is_empty() const;

    /*
     * True if this pointer lies within the allocated part of this bblock.
     */
    bool contains_pointer(void *ptr) const {
        return ptr >= &m_blocks[0] && ptr < &m_blocks[m_wilderness];
    };

    LBlock * get_free_list(size_t size_in_words);

    void sweep(const Options &options);

#ifdef PZ_DEV
    void print_usage_stats() const;

    void check();
#endif
};

static_assert(sizeof(BBlock) == GC_BBlock_Size);

static const size_t GC_Max_Heap_Size = GC_BBlock_Size;
static const size_t GC_Heap_Size = 64*GC_LBlock_Size;

static_assert(GC_BBlock_Size > GC_LBlock_Size);
static_assert(GC_Max_Heap_Size >= GC_BBlock_Size);
static_assert(GC_Max_Heap_Size >= GC_Heap_Size);

/*
 * Definitions for some inline functions that must be defined here after
 * the class definitions.
 */

inline LBlock *
ptr_to_lblock(void *ptr)
{
    return reinterpret_cast<LBlock*>(
        reinterpret_cast<uintptr_t>(ptr) & GC_LBlock_Mask);
}

CellPtr::CellPtr(LBlock *block, unsigned index) :
    m_block(block), m_index(index)
{
    m_ptr = block->index_to_pointer(index);
}

CellPtr::CellPtr(void* ptr) :
    m_ptr(reinterpret_cast<void**>(ptr))
{
    m_block = ptr_to_lblock(ptr);
    m_index = m_block->index_of(ptr);
}

bool
Heap::is_heap_address(void *ptr) const
{
    if (!m_bblock->contains_pointer(ptr)) return false;

    LBlock *lblock = ptr_to_lblock(ptr);

    if (!lblock->is_in_use()) return false;
    return lblock->is_in_payload(ptr);
}

bool
Heap::is_valid_cell(void *ptr) const
{
    if (!is_heap_address(ptr)) return false;

    LBlock *lblock = ptr_to_lblock(ptr);

    if (!lblock->is_in_use()) return false;
    return lblock->is_valid_address(ptr);
}

CellPtr
Heap::ptr_to_cell(void *ptr) const
{
    assert(is_valid_cell(ptr));

    return CellPtr(ptr);
}

} // namespace pz

#endif // ! PZ_GC_LAYOUT_H
