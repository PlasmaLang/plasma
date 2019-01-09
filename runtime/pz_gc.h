/*
 * Plasma garbage collector
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2018 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#ifndef PZ_GC_H
#define PZ_GC_H

#include "pz_option.h"

typedef struct PZ_Heap_Mark_State_S PZ_Heap_Mark_State;

/*
 * Roots are traced from two different sources (although at the moment
 * they're treated the same).  Global roots and thread-local roots.
 */
typedef void (*trace_fn)(PZ_Heap_Mark_State*, void*);

namespace pz {

class Heap {
  private:
    const Options   &options;
    void            *base_address;
    size_t           heap_size;
    // We're actually using this as a bytemap.
    uint8_t         *bitmap;

    void            *wilderness_ptr;
    void           **free_list;

    trace_fn         trace_global_roots;
    void            *trace_global_roots_data;

  public:
    Heap(const Options &options,
         trace_fn trace_global_roots,
         void *trace_global_roots_data);
    ~Heap();

    bool init();
    bool finalise();

    void * alloc(size_t size_in_words,
            trace_fn trace_thread_roots, void *trace_data);
    void * alloc_bytes(size_t size_in_bytes,
            trace_fn trace_thread_roots, void *trace_data);

    /*
     * Set the new heap size.
     *
     * Note that if the new size, is less than the current size it may be
     * difficult for the GC to shrink the heap.  In such cases this call may
     * fail.
     */
    bool set_heap_size(size_t new_size);

    Heap(const Heap &) = delete;
    Heap& operator=(const Heap &) = delete;

  private:
    void collect(trace_fn trace_thread_roots, void *trace_data);

    unsigned mark(void **cur);

    void sweep();

    void * try_allocate(size_t size_in_words);

    bool is_valid_object(void *ptr);

    bool is_heap_address(void *ptr);

    uint8_t* cell_bits(void *ptr);

    // The size of the cell in machine words
    static uintptr_t * cell_size(void *p_cell);

    friend void pz_gc_mark_root(PZ_Heap_Mark_State*, void*);
    friend void pz_gc_mark_root_conservative(PZ_Heap_Mark_State*,
            void*, size_t);
    friend void pz_gc_mark_root_conservative_interior(PZ_Heap_Mark_State*, 
            void*, size_t);

#ifdef PZ_DEV
    void check_heap();
#endif
};


/*
 * heap_ptr is a pointer into the heap that a root needs to keep alive.
 */
void
pz_gc_mark_root(PZ_Heap_Mark_State *marker, void *heap_ptr);

/*
 * root and len specify a memory area within a root (such as a stack) that
 * may contain pointers the GC should not collect.
 */
void
pz_gc_mark_root_conservative(PZ_Heap_Mark_State *marker, void *root,
        size_t len);

/*
 * root and len specify a memory area within a root (such as a stack) that
 * may contain pointers the GC should not collect.  This version supports
 * interior pointers, such as might be found on the return stack.
 */
void
pz_gc_mark_root_conservative_interior(PZ_Heap_Mark_State *marker, void *root,
        size_t len);


} // namespace pz

#endif /* ! PZ_GC_H */

