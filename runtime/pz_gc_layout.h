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
        m_ptr(reinterpret_cast<void**>(ptr)), m_type(type) { }

    void** pointer() { return m_ptr; }

    bool is_valid() const { return m_ptr != nullptr; }
    bool is_bop_cell() const { return m_type == CT_BOP; }
    bool is_fit_cell() const { return m_type == CT_FIT; }
};

/*
 * A cell in the "bag of pages" storage class.
 */
class CellPtrBOP : public CellPtr {
  private:
    Block*      m_block;
    unsigned    m_index;

    constexpr CellPtrBOP() : m_block(nullptr), m_index(0) { }

    int* free_list_data() {
        return reinterpret_cast<int*>(m_ptr);
    }

  public:
    inline explicit CellPtrBOP(Block* block, unsigned index, void* ptr);
    inline explicit CellPtrBOP(Block* block, unsigned index);
    inline explicit CellPtrBOP(void* ptr);

    Block* block() const { return m_block; }
    unsigned index() const { return m_index; }

    void set_next_in_list(int next) {
        *free_list_data() = next;
    }
    int next_in_list() {
        return *free_list_data();
    }

    static constexpr CellPtrBOP Invalid() { return CellPtrBOP(); }
};

/*
 * A Fit-allocated cell
 */
class CellPtrFit : public CellPtr {
  private:
    ChunkFit *m_chunk;

    constexpr CellPtrFit() : CellPtr(nullptr, CT_INVALID),
        m_chunk(nullptr) { }

    /*
     * We could pack size and flags into the same value, but that's a later
     * optimisation because it's tricky to do portably and still keep using
     * size_t which we use elsewhere (avoid losing data when casting).
     */
    struct CellInfo {
        size_t      size;
        uint8_t     flags;
    };
    uint8_t    CI_FLAG_ALLOCATED = 0x01;
    uint8_t    CI_FLAG_MARKED = 0x02;

  public:
    static constexpr size_t CellInfoOffset =
        AlignUp(sizeof(CellInfo), WORDSIZE_BYTES);

  private:
    /*
     * The memory word before a cell contains the size and two flags in the
     * highest bits.
     */
    CellInfo* info_ptr() {
        return reinterpret_cast<CellInfo*>(
                reinterpret_cast<void*>(pointer())-CellInfoOffset);
    }

  public:
    inline explicit CellPtrFit(ChunkFit *chunk, void *ptr);

    constexpr static CellPtrFit Invalid() { return CellPtrFit(); }

    size_t size() { return info_ptr()->size; }
    void set_size(size_t new_size) { info_ptr()->size = new_size; }

    bool is_allocated() {
        return info_ptr()->flags & CI_FLAG_ALLOCATED;
    }
    bool is_marked() {
        return info_ptr()->flags & CI_FLAG_MARKED;
    }
    void mark() {
        assert(is_allocated());
        info_ptr()->flags = CI_FLAG_ALLOCATED | CI_FLAG_MARKED;
    }
    void set_allocated() {
        assert(!is_allocated());
        assert(!is_marked());
        info_ptr()->flags = CI_FLAG_ALLOCATED;
    }

    CellPtrFit next_in_list() {
        if (*pointer()) {
            return CellPtrFit(m_chunk, *pointer());
        } else {
            return CellPtrFit::Invalid();
        }
    }
    void set_next_in_list(CellPtrFit &next) {
        *pointer() = next.pointer();
    }
    void clear_next_in_list() { *pointer() = nullptr; }

    inline CellPtrFit next_in_chunk();

    CellPtrFit split(size_t new_size);
};

/*
 * Blocks
 *
 * These must be a power-of-two and mmap must align to them. 4K is the
 * default.
 */
static const unsigned GC_Block_Log = 13;
static const size_t GC_Block_Size = 1 << (GC_Block_Log - 1);
static const size_t GC_Block_Mask = ~(GC_Block_Size - 1);
static const unsigned GC_Min_Cell_Size = 2;
static const unsigned GC_Cells_Per_Block = GC_Block_Size /
    (GC_Min_Cell_Size * WORDSIZE_BYTES);

class Block {
  private:
    struct Header {
        const static size_t Block_Empty = 0;
        size_t    block_type_or_size;

        const static int Empty_Free_List = -1;
        int       free_list;

        // Really a bytemap.
        uint8_t   bitmap[GC_Cells_Per_Block];

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
        GC_Block_Size - Header_Bytes;
    static constexpr size_t Max_Cell_Size =
        Payload_Bytes / WORDSIZE_BYTES;

  private:
    alignas(WORDSIZE_BYTES)
    uint8_t     m_bytes[Payload_Bytes];

  public:
    explicit Block(const Options &options, size_t cell_size_);

    // This constructor won't touch any memory and can be used to construct
    // uninitialised Blocks within Chunks.
    Block() {}

    Block(const Block&) = delete;
    void operator=(const Block&) = delete;

    // Size in words.
    size_t size() const {
        assert(is_in_use());
        return m_header.block_type_or_size;
    }

    unsigned num_cells() const {
        unsigned num = Payload_Bytes / (size() * WORDSIZE_BYTES);
        assert(num <= GC_Cells_Per_Block);
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

    /*
     * Must also work for interior pointers.
     */
    unsigned index_of(const void *ptr) const {
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
    const uint8_t * cell_bits(const CellPtrBOP &cell) const {
        assert(cell.is_valid() && cell.block() == this);
        return cell_bits(cell.index());
    }

    uint8_t * cell_bits(const CellPtrBOP &cell) {
        assert(cell.is_valid() && cell.block() == this);
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
    bool is_allocated(CellPtrBOP &cell) const {
        return *cell_bits(cell) & GC_Bits_Allocated;
    }

    bool is_marked(CellPtrBOP &cell) const {
        return *cell_bits(cell) & GC_Bits_Marked;
    }

    void allocate(CellPtrBOP &cell) {
        assert(*cell_bits(cell) == 0);
        *cell_bits(cell) = GC_Bits_Allocated;
    }

    void unallocate(CellPtrBOP &cell) {
        assert(!is_marked(cell));
        *cell_bits(cell) = 0;
    }

    void mark(CellPtrBOP &cell) {
        assert(is_allocated(cell));
        *cell_bits(cell) = GC_Bits_Allocated | GC_Bits_Marked;
    }

    void unmark(CellPtrBOP &cell) {
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

    CellPtrBOP allocate_cell();

#ifdef PZ_DEV
    void print_usage_stats() const;

    void check();

  private:
    bool is_in_free_list(CellPtrBOP &cell);

    // Calculate the number of free cells via the free list length.
    unsigned num_free();
#endif
};

static_assert(sizeof(Block) == GC_Block_Size);

/*
 * Chunks
 *
 * GC_Chunk_Size is also a power of two and is therefore a multiple of
 * GC_Block_Size.  4MB is the default.
 */
static const unsigned GC_Chunk_Log = 23;
static const size_t GC_Chunk_Size = 1 << (GC_Chunk_Log - 1);
static const size_t GC_Block_Per_Chunk =
        (GC_Chunk_Size / GC_Block_Size) - 1;

class Chunk {
  private:
    CellType m_type;

    Chunk(const Chunk&) = delete;
    void operator=(const Chunk&) = delete;

    Chunk() : m_type(CT_INVALID) { }

  protected:
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
            ptr < (reinterpret_cast<const void*>(this) + GC_Chunk_Size);
    };
};

/*
 * ChunkBOP is a chunk containing BIBOP style blocks of cells.
 */
class ChunkBOP : public Chunk {
  private:
    uint32_t    m_wilderness;

    alignas(GC_Block_Size)
    Block       m_blocks[GC_Block_Per_Chunk];

    ChunkBOP() : Chunk(CT_BOP), m_wilderness(0) { }
    friend ChunkBOP* Chunk::initalise_as_bop();

  public:
    /*
     * Get an unused block.
     *
     * The caller must initalise the block, this is require to ensure that
     * it is properly marked as allocated.
     */
    Block* allocate_block();

    /*
     * The size of the allocated portion of this Chunk.
     */
    size_t size() const;

    bool is_empty() const;

    /*
     * If this pointer lies within the allocated part of this chunk then
     * return its block.
     */
    inline Block * ptr_to_block(void *ptr);

    /*
     * Get an block for the given size that is not full (we want to
     * allocate a cell of this size).
     */
    Block * get_block_for_allocation(size_t size_in_words);

    void sweep(const Options &options);

#ifdef PZ_DEV
    void print_usage_stats() const;

    void check();
#endif
};

/*
 * ChunkFit is a chunk for allocation of larger cells using best-fit with
 * cell splitting.
 */
class ChunkFit : public Chunk {
  private:
    struct Header {
        CellPtrFit free_list;

        Header() : free_list(CellPtrFit::Invalid()) { }
    };

  public:
    static constexpr size_t Header_Bytes =
        RoundUp<size_t>(sizeof(Chunk) + sizeof(Header), WORDSIZE_BYTES);
    static constexpr size_t Payload_Bytes =
        GC_Chunk_Size - Header_Bytes;

  private:
    Header  m_header;

    alignas(WORDSIZE_BYTES)
    char    m_bytes[Payload_Bytes];

    ChunkFit();
    friend ChunkFit* Chunk::initalise_as_fit();

  public:
    CellPtrFit allocate_cell(size_t size_in_words);

    CellPtrFit first_cell() {
        return CellPtrFit(this, reinterpret_cast<void*>(m_bytes) +
                 CellPtrFit::CellInfoOffset);
    }
};

static_assert(sizeof(ChunkBOP) == GC_Chunk_Size);
static_assert(sizeof(ChunkFit) == GC_Chunk_Size);

static const size_t GC_Max_Heap_Size = GC_Chunk_Size;
static const size_t GC_Heap_Size = 64*GC_Block_Size;

static_assert(GC_Chunk_Size > GC_Block_Size);
static_assert(GC_Max_Heap_Size >= GC_Chunk_Size);
static_assert(GC_Max_Heap_Size >= GC_Heap_Size);

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

CellPtrBOP::CellPtrBOP(void* ptr) :
    CellPtr(reinterpret_cast<void**>(ptr), CT_BOP)
{
    m_block = ptr_to_block(ptr);
    m_index = m_block->index_of(ptr);
}

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

CellPtrFit
CellPtrFit::next_in_chunk() {
    void **next = reinterpret_cast<void**>(pointer()) + size() + 1;
    if (m_chunk->contains_pointer(next)) { 
        return CellPtrFit(m_chunk, next);
    } else {
        return CellPtrFit::Invalid();
    }
}

} // namespace pz

#endif // ! PZ_GC_LAYOUT_H
