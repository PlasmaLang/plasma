/*
 * Plasma garbage collector
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2018 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#ifndef PZ_GC_H
#define PZ_GC_H

typedef struct PZ_Heap_S PZ_Heap;

PZ_Heap *
pz_gc_init();

void
pz_gc_free(PZ_Heap *heap);

void *
pz_gc_alloc(PZ_Heap *heap, size_t size);

#endif /* ! PZ_GC_H */

