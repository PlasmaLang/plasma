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

/*
 * Plasma GC
 * ---------
 *
 * We want a GC that provides enough features to meet some MVP-ish goals.  It
 * only needs to be good enough to ensure we recover memory.  We'll re-write it
 * in later stages of the project.
 *
 *  * Mark/Sweep
 *  * Non-moving
 *  * Conservative
 *  * Interior pointers (up to 7 byte offset)
 *  * Allocate from free lists otherwise bump-pointer into wilderness.
 *  * Cell sizes are stored in the word before each cell.
 *  * Each word has an associated byte which stores whether it is the
 *    beginning of an allocation and/or marked.  The mark bit only makes
 *    sense if the object _is_ the beginning of an allocation.
 *
 * This is about the simplest GC one could imagine, it is very naive in the
 * short term we should:
 *
 *  * Sort the free list or use multiple free lists to speed up allocation.
 *  * Use a mark stack
 *  * Tune "when to collect" decision.
 *
 * In the slightly longer term we should:
 *
 *  * Use a BIBOP heap layout, generally saving memory.
 *  * Use accurate pointer information and test it by adding compaction.
 *
 * In the long term, and with much tweaking, this GC will become the
 * tenured and maybe the tenured/mutable part of a larger GC with more
 * features and improvements.
 *
 * Define the PZ_GC_TRACE C macro to trace some operations of the collector.
 */

#define PZ_GC_MAX_HEAP_SIZE ((1024*1024))
#define PZ_GC_HEAP_SIZE 4096

#ifdef PZ_DEV
#define PZ_GC_POISON 1
#endif

/*
 * Mask off the low bits so that we can see the real pointer rather than a
 * tagged pointer.
 */
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

static bool
is_valid_object(PZ_Heap *heap, void *ptr);

static bool
is_heap_address(PZ_Heap *heap, void *ptr);

#define GC_BITS_ALLOCATED 0x01
#define GC_BITS_MARKED    0x02
#define GC_BITS_VALID     0x04

static uint8_t*
cell_bits(PZ_Heap *heap, void *ptr);

// The size of the cell in machine words
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
        assert(*cell_bits(heap, cell) == GC_BITS_VALID);

        assert(*cell_size(cell) != 0);
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
        assert(*cell_bits(heap, best) == GC_BITS_VALID);
        *cell_bits(heap, best) = GC_BITS_VALID | GC_BITS_ALLOCATED;
        return best;
    }

    /*
     * We also allocate the word before cell and store it's size there.
     */
    cell = heap->wilderness_ptr + MACHINE_WORD_SIZE;

    void *new_wilderness_ptr = heap->wilderness_ptr +
        (size_in_words + 1)*MACHINE_WORD_SIZE;
    if (new_wilderness_ptr > heap->base_address + heap->heap_size)
        return NULL;

    heap->wilderness_ptr = new_wilderness_ptr;
    assert(*cell_size(cell) == 0);
    *cell_size(cell) = size_in_words;

    /*
     * Each cell is a pointer to 'size' bytes of memory.  The "allocated" bit
     * is set for the memory word corresponding to 'cell'.
     */
    assert(*cell_bits(heap, cell) == 0);
    *cell_bits(heap, cell) = GC_BITS_ALLOCATED | GC_BITS_VALID;

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
        if (is_valid_object(heap, cur) &&
                !(*cell_bits(heap, cur) & GC_BITS_MARKED))
        {
            num_marked += mark(heap, cur);
            num_roots_marked++;
        }
    }
#ifdef PZ_GC_TRACE
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
    while (p_cell < (void**)heap->wilderness_ptr)
    {
        assert(is_heap_address(heap, p_cell));
        num_checked++;
        assert(*cell_size(p_cell) != 0);

        assert(*cell_bits(heap, p_cell) & GC_BITS_VALID);

        if (!(*cell_bits(heap, p_cell) & GC_BITS_MARKED)) {
#ifdef PZ_GC_POISON
            // Poison the cell.
            memset(p_cell, 0x77, *cell_size(p_cell) * MACHINE_WORD_SIZE);
#endif

            // Add to free list.
            *p_cell = heap->free_list;
            heap->free_list = p_cell;

            // Clear allocated bit
            *cell_bits(heap, p_cell) &= ~GC_BITS_ALLOCATED;

            num_swept++;
        } else {
            assert(*cell_bits(heap, p_cell) & GC_BITS_ALLOCATED);
            // Clear mark bit
            *cell_bits(heap, p_cell) &= ~GC_BITS_MARKED;
        }

        p_cell = p_cell + *cell_size(p_cell) + 1;
    }

#ifdef PZ_GC_TRACE
    fprintf(stderr, "%d/%d cells swept\n", num_swept, num_checked);
#endif
}

static unsigned
mark(PZ_Heap *heap, void **ptr)
{
    unsigned size = *((uintptr_t*)ptr - 1);
    unsigned num_marked = 0;

    *cell_bits(heap, ptr) |= GC_BITS_MARKED;
    num_marked++;

    for (void **p_cur = ptr; p_cur < ptr + size; p_cur++) {
        void *cur = REMOVE_TAG(*p_cur);
        if (is_valid_object(heap, cur) &&
                !(*cell_bits(heap, cur) & GC_BITS_MARKED))
        {
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

#ifdef PZ_GC_TRACE
    fprintf(stderr, "New heap size: %ld\n", new_size);
#endif

    heap->heap_size = new_size;
    return true;
}

/***************************************************************************/

static bool
is_valid_object(PZ_Heap *heap, void *ptr)
{
    bool valid = is_heap_address(heap, ptr) &&
        ((*cell_bits(heap, ptr) & (GC_BITS_ALLOCATED | GC_BITS_VALID)) ==
            (GC_BITS_ALLOCATED | GC_BITS_VALID));

    if (valid) {
        assert(*cell_size(ptr) != 0);
    }

    return valid;
}

static bool
is_heap_address(PZ_Heap *heap, void *ptr)
{
    return ptr >= heap->base_address && ptr < heap->wilderness_ptr;
}

static uint8_t*
cell_bits(PZ_Heap *heap, void *ptr)
{
    assert(is_heap_address(heap, ptr));
    unsigned index = ((uintptr_t)ptr - (uintptr_t)heap->base_address) /
        MACHINE_WORD_SIZE;

    return &heap->bitmap[index];
}

static uintptr_t *
cell_size(void *p_cell)
{
    return ((uintptr_t*)p_cell) - 1;
}

/***************************************************************************/
