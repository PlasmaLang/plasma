/*
 * Plasma garbage collector
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2018 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#ifndef PZ_GC_H
#define PZ_GC_H

#ifdef __cplusplus
extern "C" {
#endif

typedef struct PZ_Heap_S PZ_Heap;

PZ_Heap *
pz_gc_init(void);

void
pz_gc_set_stack(PZ_Heap *heap, void *stack);

void
pz_gc_free(PZ_Heap *heap);

void *
pz_gc_alloc(PZ_Heap *heap, size_t size_in_words, void *top_of_stack);

void *
pz_gc_alloc_bytes(PZ_Heap *heap, size_t size_in_bytes, void *top_of_stack);

/*
 * Set the new heap size.
 *
 * Note that if the new size, is less than the current size it may be
 * difficult for the GC to shrink the heap.  In such cases this call may
 * fail.
 */
bool
pz_gc_set_heap_size(PZ_Heap *heap, size_t new_size);

#ifdef __cplusplus
} // extern "C"
#endif

#endif /* ! PZ_GC_H */

