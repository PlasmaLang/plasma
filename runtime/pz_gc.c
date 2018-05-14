/*
 * Plasma garbage collector
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2018 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#include "pz_common.h"

#include <stdio.h>
#include <sys/mman.h>

#include "pz_util.h"

#include "pz_gc.h"

#define PZ_GC_MAX_SIZE ((1024*1024))

struct PZ_Heap_S {
    void *memory;
    void *heap_ptr;
};

PZ_Heap *
pz_gc_init()
{
    PZ_Heap *heap;

    heap = malloc(sizeof(PZ_Heap));
    heap->memory = mmap(NULL, PZ_GC_MAX_SIZE, PROT_READ | PROT_WRITE,
            MAP_PRIVATE | MAP_ANONYMOUS, -1, 0); 
    if (MAP_FAILED == heap->memory) {
        perror("mmap");
        free(heap);
        return NULL;
    }
    heap->heap_ptr = heap->memory;

    return heap;
}

void
pz_gc_free(PZ_Heap *heap)
{
    if (-1 == munmap(heap->memory, PZ_GC_MAX_SIZE)) {
        perror("munmap");
    }

    free(heap);
}

void *
pz_gc_alloc(PZ_Heap *heap, size_t size)
{
    void *cell = heap->heap_ptr;

    heap->heap_ptr = (void*)ALIGN_UP((uintptr_t)heap->heap_ptr + size,
            MACHINE_WORD_SIZE);

    return cell;
}


