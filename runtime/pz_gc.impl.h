/*
 * Plasma garbage collector
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2018-2019 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#ifndef PZ_GC_IMPL_H
#define PZ_GC_IMPL_H

#include "pz_gc.h"

namespace pz {

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

    void * alloc(size_t size_in_words, const GCCapability &gc_cap);
    void * alloc_bytes(size_t size_in_bytes, const GCCapability &gc_cap);

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

    bool is_valid_object(void *ptr) const;

    bool is_heap_address(void *ptr) const;

    uint8_t* cell_bits(void *ptr) const;

    // The size of the cell in machine words
    static uintptr_t * cell_size(void *p_cell);

    friend class HeapMarkState;

#ifdef PZ_DEV
    friend class NoGCScope;
    bool in_no_gc_scope;
    void start_no_gc_scope();
    void end_no_gc_scope();

    void check_heap() const;
#endif
};

} // namespace pz

#endif // ! PZ_GC_IMPL_H
