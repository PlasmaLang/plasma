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

class AbstractGCTracer;

class Heap;

/*
 * Set the maximum heap size.
 *
 * Note that if the new size, is less than the current size it may be
 * difficult for the GC to shrink the heap.  In such cases this call may
 * fail.
 */
bool
heap_set_max_size(Heap *heap, size_t new_size);

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
     * As above but heap_ptr is a possibly-interior pointer.
     */
    void
    mark_root_interior(void *heap_ptr);

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

