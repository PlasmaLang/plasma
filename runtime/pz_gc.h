/*
 * Plasma garbage collector
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#ifndef PZ_GC_H
#define PZ_GC_H

#include "pz_option.h"
#include "pz_util.h"

namespace pz {

/*
 * The GC recognised pointers even with one high tag bit (for strings) and 2
 * or 3 low tag bits.  The implementation uses these values to remove tags
 * from pointers, but users may also depend on them to remove tags.
 */
constexpr uintptr_t HIGH_TAG_SHIFT = WORDSIZE_BITS-1;
constexpr uintptr_t HIGH_TAG_MASK = static_cast<uintptr_t>(1)<<HIGH_TAG_SHIFT;
constexpr uintptr_t LOW_TAG_BITS = WORDSIZE_BYTES - 1;
constexpr uintptr_t TAG_BITS = HIGH_TAG_MASK | LOW_TAG_BITS;

class AbstractGCTracer;

class Heap;

enum AllocOpts {
    NORMAL,
    META,   // The caller can associate meta information with the object.
    TRACE,  // The caller provides trace and finalisation methods because it
            // is a GCNewTrace object
};

/*
 * Get current heap usage.
 */
size_t heap_get_usage(const Heap * heap);

/*
 * The number of times the GC has run.
 */
unsigned heap_get_collections(const Heap * heap);

/*
 * Attach some meta-information to an object.  The object must have been
 * allocated with one of the _meta allocation functions.
 *
 * The meta object may be GC allocated and this reference will be traced by
 * the GC.
 */
void heap_set_meta_info(Heap * heap, void * obj, void * meta);

void * heap_meta_info(const Heap * heap, void * obj);

void * heap_interior_ptr_to_ptr(const Heap * heap, void * ptr);

/****************************************************************************/

class CellPtrBOP;
class CellPtrFit;

class HeapMarkState
{
   private:
    unsigned num_marked;
    unsigned num_roots_marked;

    Heap * heap;

   public:
    explicit HeapMarkState(Heap * heap_)
        : num_marked(0)
        , num_roots_marked(0)
        , heap(heap_)
    {}

    void mark_root(CellPtrBOP & cell_bop);
    void mark_root(CellPtrFit & cell_fit);

    /*
     * heap_ptr is a pointer into the heap that a root needs to keep alive.
     */
    void mark_root(void * heap_ptr);

    /*
     * As above but heap_ptr is a possibly-interior pointer.
     */
    void mark_root_interior(void * heap_ptr);

    /*
     * root and len_bytes specify a memory area within a root (such as a
     * stack) that may contain pointers the GC should not collect.
     */
    void mark_root_conservative(void * root, size_t len_bytes);

    /*
     * root and len_bytes specify a memory area within a root (such as a
     * stack) that may contain pointers the GC should not collect.  This
     * version supports interior pointers, such as might be found on the
     * return stack.
     */
    void mark_root_conservative_interior(void * root, size_t len_bytes);

    void print_stats(FILE * stream);

    unsigned get_total_marked() const
    {
        return num_marked + num_roots_marked;
    }
};

}  // namespace pz

#endif /* ! PZ_GC_H */
