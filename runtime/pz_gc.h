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

    void            *stack;

  public:
    Heap(const Options &options, void *stack);
    ~Heap();

    bool init();
    bool finalise();

    void * alloc(size_t size_in_words, void *top_of_stack);
    void * alloc_bytes(size_t size_in_bytes, void *top_of_stack);

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
    void collect(void *top_of_stack);

    unsigned mark(void **cur);

    void sweep();

    void * try_allocate(size_t size_in_words);

    bool is_valid_object(void *ptr);

    bool is_heap_address(void *ptr);

    uint8_t* cell_bits(void *ptr);

    // The size of the cell in machine words
    static uintptr_t * cell_size(void *p_cell);

#ifdef DEBUG
    void check_heap();
#endif
};

} // namespace pz

#endif /* ! PZ_GC_H */

