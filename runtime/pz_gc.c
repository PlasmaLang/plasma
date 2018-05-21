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
#include <unistd.h>

#include "pz_util.h"

#include "pz_gc.h"

#define PZ_GC_MAX_HEAP_SIZE ((1024*1024))
#define PZ_GC_HEAP_SIZE 4096

struct PZ_Heap_S {
    void   *base_address;
    size_t  heap_size;

    void   *heap_ptr;
};

static size_t
page_size;
static bool
statics_initalised = false;

PZ_Heap *
pz_gc_init()
{
    PZ_Heap *heap;

    if (!statics_initalised) {
        statics_initalised = true;
        page_size = sysconf(_SC_PAGESIZE);
    }

    heap = malloc(sizeof(PZ_Heap));
    heap->base_address = mmap(NULL, PZ_GC_MAX_HEAP_SIZE,
            PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    if (MAP_FAILED == heap->base_address) {
        perror("mmap");
        free(heap);
        return NULL;
    }
    heap->heap_size = PZ_GC_HEAP_SIZE;
    heap->heap_ptr = heap->base_address;

    return heap;
}

void
pz_gc_free(PZ_Heap *heap)
{
    if (-1 == munmap(heap->base_address, PZ_GC_MAX_HEAP_SIZE)) {
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

bool
pz_gc_set_heap_size(PZ_Heap *heap, size_t new_size)
{
    assert(statics_initalised);
    if (new_size < page_size) return false;
    if (new_size < heap->heap_ptr - heap->base_address) return false;

    heap->heap_size = new_size;
    return true;
}

