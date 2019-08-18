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

class CellPtr;
class LBlock;
class BBlock;

class Heap {
  private:
    const Options      &m_options;
    // For now there's exactly one big block.
    BBlock*             m_bblock;
    size_t              m_max_size;
    unsigned            m_collections;

    AbstractGCTracer   &m_trace_global_roots;

  public:
    Heap(const Options &options, AbstractGCTracer &trace_global_roots);
    ~Heap();

    bool init();
    bool finalise();

    void * alloc(size_t size_in_words, const GCCapability &gc_cap);
    void * alloc_bytes(size_t size_in_bytes, const GCCapability &gc_cap);

    size_t max_size() const;
    bool set_max_size(size_t new_size);

    size_t size() const;

    unsigned collections() const;

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

    bool is_empty() const;

    unsigned mark(CellPtr &cell);

    void sweep();

    void * try_allocate(size_t size_in_words);

    LBlock * get_lblock_for_allocation(size_t size_in_words);

    LBlock * allocate_block(size_t size_in_words);

    /*
     * Although these two methods are marked as inline they are defined in
     * pz_gc_layout.h with other inline functions.
     */

    // The address points to memory within the heap (is inside the payload
    // of an actively used block).
    inline bool is_heap_address(void *ptr) const;

    // Same as above plus the address points to the beginning of a valid
    // cell.
    inline bool is_valid_cell(void *ptr) const;

    // An is_valid_address can be converted to a cell here.
    inline CellPtr ptr_to_cell(void *ptr) const;

    friend class HeapMarkState;

#ifdef PZ_DEV
    friend class NoGCScope;
    bool m_in_no_gc_scope;
    void start_no_gc_scope();
    void end_no_gc_scope();

    void check_heap() const;
    void print_usage_stats() const;
#endif
};

} // namespace pz

#endif // ! PZ_GC_IMPL_H
