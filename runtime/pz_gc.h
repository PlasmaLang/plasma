/*
 * Plasma garbage collector
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2018-2019 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#ifndef PZ_GC_H
#define PZ_GC_H

#include "pz_option.h"

namespace pz {

class GCCapability;
class AbstractGCTracer;

class Heap {
  private:
    const Options      &m_options;
    void               *m_base_address;
    size_t              m_heap_size;
    // We're actually using this as a bytemap.
    uint8_t            *m_bitmap;

    void               *m_wilderness_ptr;
    void              **m_free_list;

    AbstractGCTracer   &m_trace_global_roots;

  public:
    Heap(const Options &options, AbstractGCTracer &trace_global_roots);
    ~Heap();

    bool init();
    bool finalise();

    void * alloc(size_t size_in_words, GCCapability &gc_cap);
    void * alloc_bytes(size_t size_in_bytes, GCCapability &gc_cap);

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

    /*
     * This is not guarenteed to collect, for now we have no logic to decide
     * if we want to collect, just do it.
     */
    void maybe_collect(const AbstractGCTracer *thread_tracer) {
        collect(thread_tracer);
    }

  private:
    void collect(const AbstractGCTracer *thread_tracer);

    unsigned mark(void **cur);

    void sweep();

    void * try_allocate(size_t size_in_words);

    bool is_valid_object(void *ptr);

    bool is_heap_address(void *ptr);

    uint8_t* cell_bits(void *ptr);

    // The size of the cell in machine words
    static uintptr_t * cell_size(void *p_cell);

    friend class HeapMarkState;

#ifdef PZ_DEV
    friend class NoGCScope;
    bool in_no_gc_scope;
    void start_no_gc_scope();
    void end_no_gc_scope();

    void check_heap();
#endif
};

class HeapMarkState {
  private:
    unsigned    num_marked;
    unsigned    num_roots_marked;

    Heap       *heap;

  public:
    explicit HeapMarkState(Heap *heap_) :
        num_marked(0),
        num_roots_marked(0),
        heap(heap_) {}

    /*
     * heap_ptr is a pointer into the heap that a root needs to keep alive.
     */
    void
    mark_root(void *heap_ptr);

    /*
     * root and len specify a memory area within a root (such as a stack) that
     * may contain pointers the GC should not collect.
     */
    void
    mark_root_conservative(void *root, size_t len);

    /*
     * root and len specify a memory area within a root (such as a stack) that
     * may contain pointers the GC should not collect.  This version supports
     * interior pointers, such as might be found on the return stack.
     */
    void
    mark_root_conservative_interior(void *root, size_t len);

    void
    print_stats(FILE *stream);
};

} // namespace pz

#endif /* ! PZ_GC_H */

