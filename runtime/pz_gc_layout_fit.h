/*
 * Plasma garbage collector memory layout - fit allocation.
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2019 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#ifndef PZ_GC_LAYOUT_FIT_H
#define PZ_GC_LAYOUT_FIT_H

namespace pz {

/*
 * A Fit-allocated cell
 */
class CellPtrFit : public CellPtr {
  private:
    ChunkFit *m_chunk;

    constexpr CellPtrFit() : CellPtr(nullptr, CT_INVALID),
        m_chunk(nullptr) { }

    enum CellState {
        CS_FREE,
        CS_ALLOCATED,
        CS_MARKED
    };

    /*
     * We could pack size and flags into the same value, but that's a later
     * optimisation because it's tricky to do portably and still keep using
     * size_t which we use elsewhere (avoid losing data when casting).
     */
    struct CellInfo {
        size_t      size;
        CellState   state;
        void       *meta;
    };

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

    void set_size(size_t new_size) {
        assert(new_size >= 1 && new_size < GC_Chunk_Size);
        info_ptr()->size = new_size;
    }

  public:
    inline explicit CellPtrFit(ChunkFit *chunk, void *ptr);

    constexpr static CellPtrFit Invalid() { return CellPtrFit(); }

    void init(size_t size) {
        info_ptr()->state = CS_FREE;
        set_size(size);
        clear_next_in_list();
    }

    // This non-virtual override exists only to oppitunitistically provide an
    // assertion.
    inline bool is_valid();

    size_t size() { return info_ptr()->size; }

    bool is_allocated() {
        return info_ptr()->state != CS_FREE;
    }
    bool is_marked() {
        return info_ptr()->state == CS_MARKED;
    }
    void mark() {
        assert(info_ptr()->state != CS_FREE);
        info_ptr()->state = CS_MARKED;
    }
    void unmark() {
        // TODO: This state change should be illegal.  But it needs to wait
        // for https://github.com/PlasmaLang/plasma/issues/196
        assert(is_marked());
        info_ptr()->state = CS_ALLOCATED;
    }
    void set_allocated() {
        assert(info_ptr()->state == CS_FREE);
        info_ptr()->state = CS_ALLOCATED;
    }
    
    void ** meta() {
        return &(info_ptr()->meta);
    }

    inline CellPtrFit next_in_list();
    void set_next_in_list(CellPtrFit &next) {
        *pointer() = next.pointer();
    }
    void clear_next_in_list() { *pointer() = nullptr; }

    inline void* next_by_size(size_t size);
    inline CellPtrFit next_in_chunk();

    CellPtrFit split(size_t new_size);

#ifdef PZ_DEV
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
    /*
     * Bytes used in this chunk, including cell headers.
     */
    size_t usage();

    bool is_empty();

    CellPtrFit allocate_cell(size_t size_in_words);

    CellPtrFit first_cell() {
        return CellPtrFit(this, reinterpret_cast<void*>(m_bytes) +
                 CellPtrFit::CellInfoOffset);
    }

    void sweep(const Options &options);

#ifdef PZ_DEV
    void check();

    void print_usage_stats();
#endif
};

static_assert(sizeof(ChunkFit) == GC_Chunk_Size);

} // namespace pz

#endif // ! PZ_GC_LAYOUT_FIT_H⎋
