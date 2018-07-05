/*
 * Plasma garbage collector
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2018 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#include "pz_common.h"

#include <stdio.h>
#include <string.h>
#include <sys/mman.h>
#include <unistd.h>

#include "pz_util.h"

#include "pz_gc.h"

#define PZ_GC_MAX_HEAP_SIZE ((1024*1024))
#define PZ_GC_HEAP_SIZE 4096

#define GC_BITMAP_ALLOCATED 0x01
#define GC_BITMAP_MARKED    0x02

#if __WORDSIZE == 64
#define TAG_BITS 0x7
#elif __WORDSIZE == 32
#define TAG_BITS 0x3
#endif

#define REMOVE_TAG(x) (void*)((uintptr_t)(x) & (~(uintptr_t)0 ^ TAG_BITS))

struct PZ_Heap_S {
    void       *base_address;
    size_t      heap_size;
    // We're actually using this as a bytemap.
    uint8_t    *bitmap;

    void       *wilderness_ptr;
    void      **free_list;

    void       *stack;
};

static void
collect(PZ_Heap *heap, void *top_of_stack);

static unsigned
obj_index(PZ_Heap *heap, void *ptr);

static bool
is_valid_object(PZ_Heap *heap, void *ptr);

static bool
is_heap_address(PZ_Heap *heap, void *ptr);

static bool
is_marked(PZ_Heap *heap, void *ptr);

static uintptr_t *
cell_size(void *p_cell);

static size_t
page_size;
static bool
statics_initalised = false;

/***************************************************************************/

PZ_Heap *
pz_gc_init(void *stack)
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
    heap->bitmap = malloc(PZ_GC_MAX_HEAP_SIZE / MACHINE_WORD_SIZE);
    memset(heap->bitmap, 0, PZ_GC_MAX_HEAP_SIZE / MACHINE_WORD_SIZE);

    heap->wilderness_ptr = heap->base_address;
    heap->free_list = NULL;

    heap->stack = stack;

    return heap;
}

void
pz_gc_free(PZ_Heap *heap)
{
    if (-1 == munmap(heap->base_address, PZ_GC_MAX_HEAP_SIZE)) {
        perror("munmap");
    }

    free(heap->bitmap);
    free(heap);
}

/***************************************************************************/

static void *
try_allocate(PZ_Heap *heap, size_t size_in_words);

void *
pz_gc_alloc(PZ_Heap *heap, size_t size_in_words, void *top_of_stack)
{
    assert(size_in_words > 0);

    void *cell = try_allocate(heap, size_in_words);
    if (cell == NULL) {
        collect(heap, top_of_stack);
        cell = try_allocate(heap, size_in_words);
        if (cell == NULL) {
            fprintf(stderr, "Out of memory\n");
            abort();
        }
    }

    return cell;
}

static void *
try_allocate(PZ_Heap *heap, size_t size_in_words)
{
    void **cell;

    /*
     * Try the free list
     */
    void **best = NULL;
    void **prev_best = NULL;
    void **prev_cell = NULL;
    cell = heap->free_list;
    while (cell != NULL) {
        if (*cell_size(cell) >= size_in_words) {
            if (best == NULL) {
                prev_best = prev_cell;
                best = cell;
            } else if (*cell_size(cell) < *cell_size(best)) {
                prev_best = prev_cell;
                best = cell;
            }
        }

        prev_cell = cell;
        cell = *cell;
    }
    if (best != NULL) {
        // Unlink the cell from the free list.
        if (prev_best == NULL) {
            assert(heap->free_list == best);
            heap->free_list = *best;
        } else {
            *prev_best = *best;
        }

        // Mark as allocated
        heap->bitmap[obj_index(heap, best)] = GC_BITMAP_ALLOCATED;
        return best;
    }

    /*
     * We also allocate the word before cell and store it's size there.
     */
    cell = heap->wilderness_ptr + MACHINE_WORD_SIZE;

    heap->wilderness_ptr = heap->wilderness_ptr +
        (size_in_words + 1)*MACHINE_WORD_SIZE;
    if (heap->wilderness_ptr > heap->base_address + heap->heap_size)
        return false;

    *cell_size(cell) = size_in_words;

    /*
     * Each cell is a pointer to 'size' bytes of memory.  The "allocated" bit
     * is set for the memory word corresponding to 'cell'.
     */
    heap->bitmap[obj_index(heap, cell)] = GC_BITMAP_ALLOCATED;

    return cell;
}

/***************************************************************************/

static unsigned
mark(PZ_Heap *heap, void **cur);

static void
collect(PZ_Heap *heap, void *top_of_stack)
{
    unsigned num_roots_marked = 0;
    unsigned num_marked = 0;

    // Mark from the root objects.
    for (void **p_cur = (void**)heap->stack;
         p_cur < (void**)top_of_stack;
         p_cur++)
    {
        void *cur = REMOVE_TAG(*p_cur);
        if (is_valid_object(heap, cur) && !is_marked(heap, cur)) {
            num_marked += mark(heap, cur);
            num_roots_marked++;
        }
    }
#ifdef PZ_DEV
    fprintf(stderr,
            "Marked %d out of %ld root pointers, marked %u pointers total\n",
            num_roots_marked,
            (top_of_stack - heap->stack) / MACHINE_WORD_SIZE,
            num_marked);
#endif

    // Sweep
    heap->free_list = NULL;
    unsigned num_checked = 0;
    unsigned num_swept = 0;
    void **p_cell = (void**)heap->base_address + 1;
    while (p_cell < (void**)(heap->base_address + heap->heap_size))
    {
        assert(is_heap_address(heap, p_cell));
        num_checked++;
        if (*cell_size(p_cell) != 0) {
            if (!is_marked(heap, p_cell)) {
                // Add to free list.
                *p_cell = heap->free_list;
                heap->free_list = p_cell;

                // TODO: zero or poision the cell.

                // TODO: Assert size and allocated bits.  Maybe, depending on
                // whether allocated will always mean allocated.

                num_swept++;
            }
        } else {

            // We must be at the end of the heap, but without size
            // information we cannot check it.  Break the loop.
            break;
        }

        // Clear mark bits
        heap->bitmap[obj_index(heap, p_cell)] = 0;
        p_cell = p_cell + *cell_size(p_cell) + 1;
    }

#ifdef PZ_DEV
    fprintf(stderr, "%d/%d cells swept\n", num_swept, num_checked);
#endif
}

static unsigned
mark(PZ_Heap *heap, void **ptr)
{
    unsigned size = *((uintptr_t*)ptr - 1);
    unsigned num_marked = 0;

    heap->bitmap[obj_index(heap, ptr)] |= GC_BITMAP_MARKED;
    num_marked++;

    for (void **p_cur = ptr; p_cur < ptr + size; p_cur++) {
        void *cur = REMOVE_TAG(*p_cur);
        if (is_valid_object(heap, cur) && !is_marked(heap, cur)) {
            num_marked += mark(heap, cur);
        }
    }

    return num_marked;
}

/***************************************************************************/

bool
pz_gc_set_heap_size(PZ_Heap *heap, size_t new_size)
{
    assert(statics_initalised);
    if (new_size < page_size) return false;
    if (new_size < heap->wilderness_ptr - heap->base_address) return false;

    heap->heap_size = new_size;
    return true;
}

/***************************************************************************/

static unsigned
obj_index(PZ_Heap *heap, void *ptr)
{
    return ((uintptr_t)ptr - (uintptr_t)heap->base_address) /
        MACHINE_WORD_SIZE;
}

static bool
is_valid_object(PZ_Heap *heap, void *ptr)
{
    return is_heap_address(heap, ptr) &&
        (heap->bitmap[obj_index(heap, ptr)] & GC_BITMAP_ALLOCATED);
}

static bool
is_heap_address(PZ_Heap *heap, void *ptr)
{
    return ptr >= heap->base_address && ptr < heap->wilderness_ptr;
}

static bool
is_marked(PZ_Heap *heap, void *ptr)
{
    assert(is_heap_address(heap, ptr));
    return !!(heap->bitmap[obj_index(heap, ptr)] & GC_BITMAP_MARKED);
}

static uintptr_t *
cell_size(void *p_cell)
{
    return ((uintptr_t*)p_cell) - 1;
}

/***************************************************************************/
