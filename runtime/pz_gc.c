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
#define PZ_GC_HEAP_SIZE 4096*2

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

    trace_fn    trace_global_roots;
    void       *trace_global_roots_data;
};

struct PZ_Heap_Mark_State_S {
    unsigned    num_marked;
    unsigned    num_roots_marked;

    PZ_Heap    *heap;
};

static void
collect(PZ_Heap *heap, trace_fn trace_thread_roots, void *trace_data);

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

#ifdef DEBUG
static void
check_heap(PZ_Heap *heap);
#endif

static size_t
page_size;
static bool
statics_initalised = false;

/***************************************************************************/

PZ_Heap *
pz_gc_init(trace_fn trace_global_roots, void *trace_global_roots_data)
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

    heap->trace_global_roots = trace_global_roots;
    heap->trace_global_roots_data = trace_global_roots_data;

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
pz_gc_alloc(PZ_Heap *heap, size_t size_in_words, trace_fn trace_thread_roots,
        void *trace_data)
{
    assert(size_in_words > 0);

    void *cell = try_allocate(heap, size_in_words);
    if (cell == NULL) {
        collect(heap, trace_thread_roots, trace_data);
        cell = try_allocate(heap, size_in_words);
        if (cell == NULL) {
            fprintf(stderr, "Out of memory, tried to allocate %lu bytes.\n",
                        size_in_words * MACHINE_WORD_SIZE);
            abort();
        }
    }

    return cell;
}

void *
pz_gc_alloc_bytes(PZ_Heap *heap, size_t size_in_bytes,
                  trace_fn trace_thread_roots, void *trace_data)
{
    size_t size_in_words = ALIGN_UP(size_in_bytes, MACHINE_WORD_SIZE) /
        MACHINE_WORD_SIZE;

    return pz_gc_alloc(heap, size_in_words, trace_thread_roots, trace_data);
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

        if (*cell_size(best) >= size_in_words + 2) {
            // This cell is bigger than we need, so shrink it.
            unsigned old_size = *cell_size(best);
            *cell_size(best) = size_in_words;
            void** next_cell = best + size_in_words + 1;
            *cell_size(next_cell) = old_size - (size_in_words + 1);
            *cell_bits(heap, next_cell) = GC_BITS_VALID;
            *next_cell = heap->free_list;
            heap->free_list = next_cell;
            #ifdef PZ_GC_TRACE
            fprintf(stderr, "Split cell %p from %u into %lu and %u\n",
                best, old_size, size_in_words, (unsigned)*cell_size(next_cell));
            #endif
        }
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
sweep(PZ_Heap *heap);

static void
collect(PZ_Heap *heap, trace_fn trace_thread_roots, void *trace_data)
{
    PZ_Heap_Mark_State state = { 0, 0, heap };

    #ifdef DEBUG
    check_heap(heap);
    #endif

#ifdef PZ_GC_TRACE
    fprintf(stderr, "Tracing from global roots\n");
#endif
    heap->trace_global_roots(&state, heap->trace_global_roots_data);
#ifdef PZ_GC_TRACE
    fprintf(stderr, "Done tracing from global roots\n");
#endif

    if (trace_thread_roots) {
#ifdef PZ_GC_TRACE
        fprintf(stderr, "Tracing from thread roots (eg stacks)\n");
#endif
        trace_thread_roots(&state, trace_data);
#ifdef PZ_GC_TRACE
        fprintf(stderr, "Done tracing from stack\n");
#endif
    }

#ifdef PZ_GC_TRACE
    fprintf(stderr,
            "Marked %d root pointers, marked %u pointers total\n",
            state.num_roots_marked,
            state.num_marked);
#endif

    sweep(heap);

    #ifdef DEBUG
    check_heap(heap);
    #endif
}

static unsigned
mark(PZ_Heap *heap, void **ptr)
{
    unsigned size = *cell_size(ptr);
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

static void
sweep(PZ_Heap *heap)
{
    // Sweep
    heap->free_list = NULL;
    unsigned num_checked = 0;
    unsigned num_swept = 0;
    unsigned num_merged = 0;
    void **p_cell = (void**)heap->base_address + 1;
    void **p_first_in_run = NULL;
    while (p_cell < (void**)heap->wilderness_ptr)
    {
        assert(is_heap_address(heap, p_cell));
        assert(*cell_size(p_cell) != 0);
        assert(*cell_bits(heap, p_cell) & GC_BITS_VALID);
        unsigned old_size = *cell_size(p_cell);

        num_checked++;
        if (!(*cell_bits(heap, p_cell) & GC_BITS_MARKED)) {
#ifdef PZ_GC_POISON
            // Poison the cell.
            memset(p_cell, 0x77, *cell_size(p_cell) * MACHINE_WORD_SIZE);
#endif
            if (p_first_in_run == NULL) {
                // Add to free list.
                *p_cell = heap->free_list;
                heap->free_list = p_cell;

                // Clear allocated bit
                *cell_bits(heap, p_cell) &= ~GC_BITS_ALLOCATED;

                p_first_in_run = p_cell;
            } else {
                // Clear valid bit, this cell will be merged.
                *cell_bits(heap, p_cell) = 0;
                // poison the size field.
                memset(cell_size(p_cell), 0x77, MACHINE_WORD_SIZE);
                num_merged++;
            }

            num_swept++;
        } else {
            assert(*cell_bits(heap, p_cell) & GC_BITS_ALLOCATED);
            // Clear mark bit
            *cell_bits(heap, p_cell) &= ~GC_BITS_MARKED;

            if (p_first_in_run != NULL) {
                // fixup size
                *cell_size(p_first_in_run) = p_cell - (p_first_in_run + 1);
                p_first_in_run = NULL;
            }
        }

        p_cell = p_cell + old_size + 1;
    }

    if (p_first_in_run != NULL) {
        // fixup size
        *cell_size(p_first_in_run) = (void**)heap->wilderness_ptr -
            (p_first_in_run + 1);
        p_first_in_run = NULL;
    }

#ifdef PZ_GC_TRACE
    fprintf(stderr, "%u/%u cells swept (%u merged)\n", num_swept, num_checked,
        num_merged);
#endif
}

/***************************************************************************/

void
pz_gc_mark_root(PZ_Heap_Mark_State *marker, void *heap_ptr)
{
    if (is_valid_object(marker->heap, heap_ptr) &&
           !(*cell_bits(marker->heap, heap_ptr) & GC_BITS_MARKED))
    {
        marker->num_marked += mark(marker->heap, heap_ptr);
        marker->num_roots_marked++;
    }
}

void
pz_gc_mark_root_conservative(PZ_Heap_Mark_State *marker, void *root,
        size_t len)
{
    PZ_Heap *heap = marker->heap;

    // Mark from the root objects.
    for (void **p_cur = (void**)root;
         p_cur < (void**)(root + len);
         p_cur++)
    {
        void *cur = REMOVE_TAG(*p_cur);
        // We don't generally support interior pointers, however return
        // addresses on the stack may point to any place within a
        // procedure.
        if (is_valid_object(heap, cur) &&
                !(*cell_bits(heap, cur) & GC_BITS_MARKED))
        {
            marker->num_marked += mark(heap, cur);
            marker->num_roots_marked++;
        }
    }
}

void
pz_gc_mark_root_conservative_interior(PZ_Heap_Mark_State *marker, void *root,
        size_t len)
{
    PZ_Heap *heap = marker->heap;

    // Mark from the root objects.
    for (void **p_cur = (void**)root;
         p_cur < (void**)(root + len);
         p_cur++)
    {
        void *cur = REMOVE_TAG(*p_cur);
        // We don't generally support interior pointers, however return
        // addresses on the stack may point to any place within a
        // procedure.
        if (is_heap_address(heap, cur)) {
            while ((*cell_bits(heap, cur) & GC_BITS_VALID) == 0) {
                // Step backwards until we find a valid address.
                cur -= MACHINE_WORD_SIZE;
            }
            if (is_valid_object(heap, cur) &&
                    !(*cell_bits(heap, cur) & GC_BITS_MARKED))
            {
                marker->num_marked += mark(heap, cur);
                marker->num_roots_marked++;
            }
        }
    }
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

#ifdef DEBUG
static void
check_heap(PZ_Heap *heap)
{
    assert(statics_initalised);
    assert(heap != NULL);
    assert(heap->base_address != NULL);
    assert(heap->heap_size >= page_size);
    assert(heap->heap_size % page_size == 0);
    assert(heap->bitmap);
    assert(heap->wilderness_ptr != NULL);
    assert(heap->base_address < heap->wilderness_ptr);
    assert(heap->wilderness_ptr <= heap->base_address + heap->heap_size);

    // Scan for consistency between flags and size values
    void **next_valid = (void**)heap->base_address + 1;
    void **cur = heap->base_address;
    for (cur = heap->base_address; cur < (void**)heap->wilderness_ptr; cur++) {
        if (cur == next_valid) {
            unsigned size;
            assert(*cell_bits(heap, cur) & GC_BITS_VALID);
            size = *cell_size(cur);
            assert(size > 0);
            next_valid = cur + size + 1;
        } else {
            assert(*cell_bits(heap, cur) == 0);
        }
    }

    // Check the free list for consistency.
    cur = heap->free_list;
    while (cur) {
        assert(*cell_bits(heap, cur) == GC_BITS_VALID);
        // TODO check to avoid duplicates
        // TODO check to avoid free cells not on the free list.
        cur = *cur;
    }
}
#endif

/***************************************************************************/
