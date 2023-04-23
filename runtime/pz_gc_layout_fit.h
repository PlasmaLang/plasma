/*
 * Plasma garbage collector memory layout - fit allocation.
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#ifndef PZ_GC_LAYOUT_FIT_H
#define PZ_GC_LAYOUT_FIT_H

namespace pz {

/*
 * A Fit-allocated cell
 */
class CellPtrFit : public CellPtr
{
   private:
    ChunkFit * m_chunk;

    constexpr CellPtrFit() : CellPtr(nullptr, CT_INVALID), m_chunk(nullptr) {}

    enum CellState : uint8_t { CS_FREE, CS_ALLOCATED, CS_MARKED };

   public:
    enum CellFlags : uint8_t { CF_NONE = 0x00, CF_TRACE_AND_FINALISE = 0x01 };

   private:
    /*
     * We could pack size and flags into the same value, but that's a later
     * optimisation because it's tricky to do portably and still keep using
     * size_t which we use elsewhere (avoid losing data when casting).
     */
    struct CellInfo {
        size_t    size;
        CellState state;
        CellFlags flags;
        void *    meta;
    };

   public:
    static constexpr size_t CellInfoOffset =
        AlignUp(sizeof(CellInfo), WORDSIZE_BYTES);

   private:
    /*
     * The memory word before a cell contains the size and two flags in the
     * highest bits.
     */
    CellInfo * info_ptr()
    {
        return reinterpret_cast<CellInfo *>(
            reinterpret_cast<uint8_t *>(pointer()) - CellInfoOffset);
    }

    void set_size(size_t new_size)
    {
        assert(new_size >= 1 && new_size < GC_Chunk_Size);
        info_ptr()->size = new_size;
    }

   public:
    inline explicit CellPtrFit(ChunkFit * chunk, void * ptr);

    constexpr static CellPtrFit Invalid()
    {
        return CellPtrFit();
    }

    void init(size_t size)
    {
        info_ptr()->state = CS_FREE;
        set_size(size);
        clear_next_in_list();
    }

    // This non-virtual override exists only to oppitunitistically provide an
    // assertion.
    inline bool is_valid();

    size_t size()
    {
        return info_ptr()->size;
    }

    bool is_allocated()
    {
        return info_ptr()->state != CS_FREE;
    }
    bool is_marked()
    {
        return info_ptr()->state == CS_MARKED;
    }
    void mark()
    {
        assert(info_ptr()->state != CS_FREE);
        info_ptr()->state = CS_MARKED;
    }
    void unmark()
    {
        assert(is_marked());
        info_ptr()->state = CS_ALLOCATED;
    }
    void set_allocated()
    {
        assert(info_ptr()->state == CS_FREE);
        info_ptr()->state = CS_ALLOCATED;
        info_ptr()->flags = CF_NONE;
    }
    void set_free()
    {
        assert(info_ptr()->state == CS_ALLOCATED);
        info_ptr()->state = CS_FREE;
    }

    void ** meta()
    {
        return &(info_ptr()->meta);
    }

    CellFlags flags()
    {
        return info_ptr()->flags;
    }
    void set_flags(CellFlags flags)
    {
        info_ptr()->flags = flags;
    }

    inline CellPtrFit next_in_list();
    void              set_next_in_list(CellPtrFit & next)
    {
        *pointer() = next.pointer();
    }
    void clear_next_in_list()
    {
        *pointer() = nullptr;
    }

    inline void *     next_by_size(size_t size);
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
class ChunkFit : public Chunk
{
   private:
    struct Header {
        CellPtrFit free_list;

        Header() : free_list(CellPtrFit::Invalid()) {}
    };

   public:
    static constexpr size_t Header_Bytes =
        RoundUp<size_t>(sizeof(Chunk) + sizeof(Header), WORDSIZE_BYTES);
    static constexpr size_t Payload_Bytes = GC_Chunk_Size - Header_Bytes;

   private:
    Header m_header;

    alignas(WORDSIZE_BYTES) char m_bytes[Payload_Bytes];

   public:
    ChunkFit(Heap * heap);

    /*
     * Bytes used in this chunk, including cell headers.
     */
    size_t usage();

    bool is_empty();

    CellPtrFit allocate_cell(size_t size_in_words);

    CellPtrFit first_cell()
    {
        return CellPtrFit(
            this,
            reinterpret_cast<uint8_t *>(m_bytes) + CellPtrFit::CellInfoOffset);
    }

    void sweep(const Options & options);

#ifdef PZ_DEV
    void check();

    void print_usage_stats() const;
#endif
};

static_assert(sizeof(ChunkFit) == GC_Chunk_Size,
              "sizeof(ChunkFit) must match specified chunk size");

}  // namespace pz

#endif  // ! PZ_GC_LAYOUT_FIT_H⎋
