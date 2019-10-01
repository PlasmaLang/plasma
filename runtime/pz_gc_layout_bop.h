/*
 * Plasma garbage collector memory layout - bop allocation.
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2019 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#ifndef PZ_GC_LAYOUT_BOP_H
#define PZ_GC_LAYOUT_BOP_H

namespace pz {

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
 * Blocks
 */
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

    inline bool is_valid_address(const void *ptr) const;

    /*
     * Must also work for interior pointers.
     */
    inline unsigned index_of(const void *ptr) const;

    inline void ** index_to_pointer(unsigned index);

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

static_assert(sizeof(ChunkBOP) == GC_Chunk_Size);

} // namespace pz

#endif // ! PZ_GC_LAYOUT_BOP_H
