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

typedef struct PZ_Heap_Mark_State_S PZ_Heap_Mark_State;

namespace pz {

class AbstractGCTracer;
class Heap;

/*
 * This is the base class that the GC will use to determine if its legal to
 * GC.  Do not create subclasses of this, use only AbstractGCTracer.
 */
class GCCapability {
  public:
    virtual bool can_gc() const = 0;

    /*
     * This casts to AbstractGCTracer whenever can_gc() returns true, so it
     * must be the only subclass that overrides can_gc() to return true.
     */
    const AbstractGCTracer& tracer() const;
};

/*
 * Roots are traced from two different sources (although at the moment
 * they're treated the same).  Global roots and thread-local roots.
 */
class AbstractGCTracer : public GCCapability {
  public:
    virtual bool can_gc() const { return true; }
    virtual void do_trace(PZ_Heap_Mark_State*) const = 0;
};

/*
 * Use this RAII class to create scopes where GC is forbidden (the heap will
 * be expanded instead, or return nullptr
 *
 * Note: Callers need to check that all their allocations succeeded.
 * Allocations performed with this scope could return nullptr.
 */
class NoGCScope : public GCCapability {
  public:
    NoGCScope(Heap *heap);

    virtual bool can_gc() const { return false; }
};

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

  private:
    void collect(const AbstractGCTracer &thread_tracer);

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

