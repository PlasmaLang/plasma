/*
 * Plasma garbage collector
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2018-2019 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#include "pz_common.h"

#include <stdio.h>
#include <string.h>
#include <sys/mman.h>
#include <unistd.h>

#include "pz_util.h"

#include "pz_gc.h"
#include "pz_gc_util.h"

#include "pz_gc.impl.h"

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
 */

static const size_t PZ_GC_MAX_HEAP_SIZE = 1024*1024;
static const size_t PZ_GC_HEAP_SIZE = 4096*2;

/*
 * Mask off the low bits so that we can see the real pointer rather than a
 * tagged pointer.
 */
static const uintptr_t TAG_BITS = WORDSIZE_BYTES - 1;

constexpr void*
REMOVE_TAG(void* tagged_ptr) {
    return reinterpret_cast<void*>(
            reinterpret_cast<uintptr_t>(tagged_ptr) & (~0 ^ TAG_BITS));
}

const static uintptr_t GC_BITS_ALLOCATED = 0x01;
const static uintptr_t GC_BITS_MARKED    = 0x02;
const static uintptr_t GC_BITS_VALID     = 0x04;

namespace pz {

static size_t
s_page_size;
static bool
s_statics_initalised = false;

bool
heap_set_size(Heap *heap, size_t new_size)
{
    return heap->set_heap_size(new_size);
}

/***************************************************************************/

static inline void init_statics()
{
    if (!s_statics_initalised) {
        s_statics_initalised = true;
        s_page_size = sysconf(_SC_PAGESIZE);
    }
}

Heap::Heap(const Options &options_, AbstractGCTracer &trace_global_roots_)
        : m_options(options_)
        , m_base_address(nullptr)
        , m_heap_size(PZ_GC_HEAP_SIZE)
        , m_wilderness_ptr(nullptr)
        , m_free_list(nullptr)
        , m_trace_global_roots(trace_global_roots_)
#ifdef PZ_DEV
        , in_no_gc_scope(false)
#endif
{
    // TODO: This array doesn't need to be this big.
    // Use std::vector and allow it to change size as the heap size changes.
    // and also catch out-of-bounds access.
    m_bitmap = new uint8_t[PZ_GC_MAX_HEAP_SIZE / WORDSIZE_BYTES];
    memset(m_bitmap, 0, PZ_GC_MAX_HEAP_SIZE / WORDSIZE_BYTES);
}

Heap::~Heap()
{
    // Check that finalise was called.
    assert(!m_base_address);

    delete[] m_bitmap;
}

bool
Heap::init()
{
    init_statics();

    m_base_address = mmap(NULL, PZ_GC_MAX_HEAP_SIZE,
            PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    if (MAP_FAILED == m_base_address) {
        perror("mmap");
        return false;
    }

    m_wilderness_ptr = m_base_address;

    return true;
}

bool
Heap::finalise()
{
    if (!m_base_address)
        return true;

    bool result = -1 != munmap(m_base_address, PZ_GC_MAX_HEAP_SIZE);
    if (!result) {
        perror("munmap");
    }

    m_base_address = nullptr;
    m_wilderness_ptr = nullptr;
    return result;
}

/***************************************************************************/

void *
Heap::alloc(size_t size_in_words, const GCCapability &gc_cap)
{
    assert(size_in_words > 0);

    void *cell;
#ifdef PZ_DEV
    if (m_options.gc_zealous() && 
        gc_cap.can_gc() && 
        m_wilderness_ptr > m_base_address)
    {
        // Force a collect before each allocation in this mode.
        cell = NULL;
    } else
#endif
    {
        cell = try_allocate(size_in_words);
    }
    if (cell == NULL && gc_cap.can_gc()) {
        collect(&gc_cap.tracer());
        cell = try_allocate(size_in_words);
        if (cell == NULL) {
            fprintf(stderr, "Out of memory, tried to allocate %lu bytes.\n",
                        size_in_words * WORDSIZE_BYTES);
            abort();
        }
    }

    return cell;
}

void *
Heap::alloc_bytes(size_t size_in_bytes, const GCCapability &gc_cap) {
    size_t size_in_words = ALIGN_UP(size_in_bytes, WORDSIZE_BYTES) /
        WORDSIZE_BYTES;

    return alloc(size_in_words, gc_cap);
}

void *
Heap::try_allocate(size_t size_in_words)
{
    void **cell;

    /*
     * Try the free list
     */
    void **best = NULL;
    void **prev_best = NULL;
    void **prev_cell = NULL;
    cell = m_free_list;
    while (cell != NULL) {
        assert(*cell_bits(cell) == GC_BITS_VALID);

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
        cell = static_cast<void**>(*cell);
    }
    if (best != NULL) {
        // Unlink the cell from the free list.
        if (prev_best == NULL) {
            assert(m_free_list == best);
            m_free_list = static_cast<void**>(*best);
        } else {
            *prev_best = *best;
        }

        // Mark as allocated
        assert(*cell_bits(best) == GC_BITS_VALID);
        *cell_bits(best) = GC_BITS_VALID | GC_BITS_ALLOCATED;

        if (*cell_size(best) >= size_in_words + 2) {
            // This cell is bigger than we need, so shrink it.
            unsigned old_size = *cell_size(best);

            // Assert that the cell after this one is valid.
            assert((best + old_size == m_wilderness_ptr) ||
                    (*cell_bits(best + old_size + 1) & GC_BITS_VALID));

            *cell_size(best) = size_in_words;
            void** next_cell = best + size_in_words + 1;
            *cell_size(next_cell) = old_size - (size_in_words + 1);
            *cell_bits(next_cell) = GC_BITS_VALID;
            *next_cell = m_free_list;
            m_free_list = next_cell;
            #ifdef PZ_DEV
            if (m_options.gc_trace()) {
                fprintf(stderr, "Split cell %p from %u into %lu and %u\n",
                    best, old_size, size_in_words,
                    (unsigned)*cell_size(next_cell));
            }
            #endif
        }
        #ifdef PZ_DEV
        if (m_options.gc_trace2()) {
            fprintf(stderr, "Allocated %p from free list\n", best);
        }
        #endif
        return best;
    }

    /*
     * We also allocate the word before cell and store it's size there.
     */
    cell = static_cast<void**>(m_wilderness_ptr + WORDSIZE_BYTES);

    void *new_wilderness_ptr = m_wilderness_ptr +
        (size_in_words + 1)*WORDSIZE_BYTES;
    if (new_wilderness_ptr > m_base_address + m_heap_size) return nullptr;

    m_wilderness_ptr = new_wilderness_ptr;
    assert(*cell_size(cell) == 0);
    *cell_size(cell) = size_in_words;

    /*
     * Each cell is a pointer to 'size' bytes of memory.  The "allocated" bit
     * is set for the memory word corresponding to 'cell'.
     */
    assert(*cell_bits(cell) == 0);
    *cell_bits(cell) = GC_BITS_ALLOCATED | GC_BITS_VALID;

    #ifdef PZ_DEV
    if (m_options.gc_trace2()) {
        fprintf(stderr, "Allocated %p from the wilderness\n", cell);
    }
    #endif

    return cell;
}

/***************************************************************************/

void
Heap::collect(const AbstractGCTracer *trace_thread_roots)
{
    HeapMarkState state(this);

    // There's nothing to collect, the heap is empty.
    if (m_wilderness_ptr == m_base_address) return;

#ifdef PZ_DEV
    assert(!in_no_gc_scope);

    if (m_options.gc_slow_asserts()) {
        check_heap();
    }
#endif

#ifdef PZ_DEV
    if (m_options.gc_trace()) {
        fprintf(stderr, "Tracing from global roots\n");
    }
#endif
    m_trace_global_roots.do_trace(&state);
#ifdef PZ_DEV
    if (m_options.gc_trace()) {
        fprintf(stderr, "Done tracing from global roots\n");
    }
#endif

#ifdef PZ_DEV
    if (m_options.gc_trace()) {
        fprintf(stderr, "Tracing from thread roots (eg stacks)\n");
    }
#endif
    assert(trace_thread_roots);
    trace_thread_roots->do_trace(&state);
#ifdef PZ_DEV
    if (m_options.gc_trace()) {
        fprintf(stderr, "Done tracing from stack\n");
    }
#endif

#ifdef PZ_DEV
    if (m_options.gc_trace()) {
        state.print_stats(stderr);
    }
#endif

    sweep();

    #ifdef PZ_DEV
    if (m_options.gc_slow_asserts()) {
        check_heap();
    }
    #endif
}

unsigned
Heap::mark(void **ptr)
{
    unsigned size = *cell_size(ptr);
    unsigned num_marked = 0;

    *cell_bits(ptr) |= GC_BITS_MARKED;
    num_marked++;

    for (void **p_cur = ptr; p_cur < ptr + size; p_cur++) {
        void *cur = REMOVE_TAG(*p_cur);
        if (is_valid_object(cur) &&
                !(*cell_bits(cur) & GC_BITS_MARKED))
        {
            num_marked += mark(static_cast<void**>(cur));
        }
    }

    return num_marked;
}

void
Heap::sweep()
{
    // Sweep
    m_free_list = NULL;
    unsigned num_checked = 0;
    unsigned num_swept = 0;
    unsigned num_merged = 0;
    void **p_cell = (void**)m_base_address + 1;
    void **p_first_in_run = NULL;
    while (p_cell < (void**)m_wilderness_ptr)
    {
        assert(is_heap_address(p_cell));
        assert(*cell_size(p_cell) != 0);
        assert(*cell_bits(p_cell) & GC_BITS_VALID);
        unsigned old_size = *cell_size(p_cell);

        num_checked++;
        if (!(*cell_bits(p_cell) & GC_BITS_MARKED)) {
#ifdef PZ_DEV
            // Poison the cell.
            if (m_options.gc_poison()) {
                memset(p_cell, 0x77, old_size * WORDSIZE_BYTES);
            }
#endif
            if (p_first_in_run == NULL) {
                // Add to free list.
                *p_cell = m_free_list;
                m_free_list = p_cell;

                // Clear allocated bit
                *cell_bits(p_cell) &= ~GC_BITS_ALLOCATED;

                p_first_in_run = p_cell;
            } else {
                // Clear valid bit, this cell will be merged.
                *cell_bits(p_cell) = 0;
#ifdef PZ_DEV
                // poison the size field.
                if (m_options.gc_poison()) {
                    memset(cell_size(p_cell), 0x77, WORDSIZE_BYTES);
                }
#endif
                num_merged++;
            }

            assert(p_cell + old_size <= (void**)m_wilderness_ptr);
            assert((p_cell + old_size == m_wilderness_ptr) ||
                (*cell_bits(p_cell + old_size + 1) & GC_BITS_VALID));

            #ifdef PZ_DEV
            if (m_options.gc_trace2()) {
                fprintf(stderr, "Swept %p\n", p_cell);
            }
            #endif
            num_swept++;
        } else {
            assert(*cell_bits(p_cell) & GC_BITS_ALLOCATED);
            // Clear mark bit
            *cell_bits(p_cell) &= ~GC_BITS_MARKED;

            if (p_first_in_run != NULL) {
                // fixup size
                *cell_size(p_first_in_run) = p_cell - 1 - p_first_in_run;
                p_first_in_run = NULL;
            }
        }

        p_cell = p_cell + old_size + 1;
    }

    if (p_first_in_run != NULL) {
        // fixup size
        *cell_size(p_first_in_run) = (void**)m_wilderness_ptr -
            p_first_in_run;
        p_first_in_run = NULL;
    }

#ifdef PZ_DEV
    if (m_options.gc_trace()) {
        fprintf(stderr, "%u/%u cells swept (%u merged)\n",
                num_swept, num_checked, num_merged);
    }
#endif
}

/***************************************************************************/

void
HeapMarkState::mark_root(void *heap_ptr)
{
    if (heap->is_valid_object(heap_ptr) &&
            !(*(heap->cell_bits(heap_ptr)) & GC_BITS_MARKED))
    {
        num_marked += heap->mark((void**)heap_ptr);
        num_roots_marked++;
    }
}

void
HeapMarkState::mark_root_interior(void *heap_ptr)
{
    // This actually makes the pointer aligned to the GC's alignment.  We
    // should have a different macro for this particular use. (issue #154)
    heap_ptr = REMOVE_TAG(heap_ptr);
    if (heap->is_heap_address(heap_ptr)) {
        while ((*(heap->cell_bits(heap_ptr)) & GC_BITS_VALID) == 0) {
            heap_ptr -= WORDSIZE_BYTES;
        }
        mark_root(heap_ptr);
    }
}
    
void
HeapMarkState::mark_root_conservative(void *root, size_t len)
{
    // Mark from the root objects.
    for (void **p_cur = (void**)root;
         p_cur < (void**)(root + len);
         p_cur++)
    {
        mark_root(REMOVE_TAG(*p_cur));
    }
}

void
HeapMarkState::mark_root_conservative_interior(void *root, size_t len)
{
    // Mark from the root objects.
    for (void **p_cur = (void**)root;
         p_cur < (void**)(root + len);
         p_cur++)
    {
        mark_root_interior(*p_cur);
    }
}

void
HeapMarkState::print_stats(FILE *stream)
{
    fprintf(stream,
            "Marked %d root pointers, marked %u pointers total\n",
            num_roots_marked,
            num_marked);
}

/***************************************************************************/

bool
Heap::set_heap_size(size_t new_size)
{
    assert(s_statics_initalised);
    if (new_size < s_page_size) return false;
    if (m_base_address + new_size < m_wilderness_ptr) return false;

#ifdef PZ_DEV
    if (m_options.gc_trace()) {
        fprintf(stderr, "New heap size: %ld\n", new_size);
    }
#endif

    m_heap_size = new_size;
    return true;
}

/***************************************************************************/

bool
Heap::is_valid_object(void *ptr)
{
    bool valid = is_heap_address(ptr) &&
        ((*cell_bits(ptr) & (GC_BITS_ALLOCATED | GC_BITS_VALID)) ==
            (GC_BITS_ALLOCATED | GC_BITS_VALID));

    if (valid) {
        assert(*cell_size(ptr) != 0);
    }

    return valid;
}

bool
Heap::is_heap_address(void *ptr)
{
    return ptr >= m_base_address && ptr < m_wilderness_ptr;
}

uint8_t*
Heap::cell_bits(void *ptr)
{
    assert(is_heap_address(ptr));
    unsigned index = ((uintptr_t)ptr - (uintptr_t)m_base_address) /
        WORDSIZE_BYTES;

    return &m_bitmap[index];
}

uintptr_t *
Heap::cell_size(void *p_cell)
{
    return ((uintptr_t*)p_cell) - 1;
}

/***************************************************************************/

#ifdef PZ_DEV
void
Heap::start_no_gc_scope() {
    assert(!in_no_gc_scope);
    in_no_gc_scope = true;
}

void
Heap::end_no_gc_scope() {
    assert(in_no_gc_scope);
    in_no_gc_scope = false;
}

void
Heap::check_heap()
{
    assert(s_statics_initalised);
    assert(m_base_address != NULL);
    assert(m_heap_size >= s_page_size);
    assert(m_heap_size % s_page_size == 0);
    assert(m_bitmap);
    assert(m_wilderness_ptr != NULL);
    assert(m_base_address < m_wilderness_ptr);
    assert(m_wilderness_ptr <= m_base_address + m_heap_size);

    // Scan for consistency between flags and size values
    void **next_valid = static_cast<void**>(m_base_address) + 1;
    void **cur = static_cast<void**>(m_base_address);
    for (cur = static_cast<void**>(m_base_address);
         cur < (void**)m_wilderness_ptr;
         cur++)
    {
        if (cur == next_valid) {
            unsigned size;
            assert(*cell_bits(cur) & GC_BITS_VALID);
            size = *cell_size(cur);
            assert(size > 0);
            next_valid = cur + size + 1;
        } else {
            assert(*cell_bits(cur) == 0);
        }
    }

    // Check the free list for consistency.
    cur = m_free_list;
    while (cur) {
        assert(*cell_bits(cur) == GC_BITS_VALID);
        // TODO check to avoid duplicates
        // TODO check to avoid free cells not on the free list.
        cur = static_cast<void**>(*cur);
    }
}
#endif

} // namespace pz

/***************************************************************************
 *
 * Check arhitecture assumptions
 */

// 8 bits per byte
static_assert(WORDSIZE_BYTES * 8 == WORDSIZE_BITS);

// 32 or 64 bit.
static_assert(WORDSIZE_BITS == 64 || WORDSIZE_BITS == 32);

