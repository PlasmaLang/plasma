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
#include "pz_gc_layout.h"

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

namespace pz {

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

bool
LBlock::is_empty() const
{
    if (!is_in_use()) return true;

    for (unsigned i = 0; i < num_cells(); i++) {
        if (*cell_bits(i) & GC_BITS_ALLOCATED) {
            return false;
        }
    }

    return true;
}

bool
LBlock::is_full() const
{
    assert(is_in_use());

    for (unsigned i = 0; i < num_cells(); i++) {
        if (0 == (*cell_bits(i) & GC_BITS_ALLOCATED)) {
            return false;
        }
    }

    return true;
}

Cell*
LBlock::allocate_cell()
{
    assert(is_in_use());

    for (unsigned i = 0; i < num_cells(); i++) {
        if (0 == (*cell_bits(i) & GC_BITS_ALLOCATED)) {
            assert(*cell_bits(i) == 0);
            *cell_bits(i) = GC_BITS_ALLOCATED;
            return cell(i);
        }
    }

    return nullptr;
}

void
LBlock::sweep()
{
    if (is_empty()) return;

    for (unsigned i = 0; i < num_cells(); i++) {
        if (*cell_bits(i) & GC_BITS_MARKED) {
            // Cell is marked, clear the mark bit, keep the allocated bit.
            assert(*cell_bits(i) & GC_BITS_MARKED);
            *cell_bits(i) = GC_BITS_ALLOCATED;
        } else {
            // Free the cell.
            *cell_bits(i) = 0;
        }
    }
}

LBlock*
BBlock::next_block()
{
    if (m_wilderness >= GC_LBLOCK_PER_BBLOCK)
        return nullptr;

    return &m_blocks[m_wilderness++];
}

bool
BBlock::is_empty() const
{
    for (unsigned i = 0; i < GC_LBLOCK_PER_BBLOCK; i++) {
        if (!m_blocks[i].is_empty()) return false;
    }
    return true;
}

void
BBlock::sweep()
{
    for (unsigned i = 0; i < GC_LBLOCK_PER_BBLOCK; i++) {
        m_blocks[i].sweep();
    }
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
        , m_bblock(nullptr)
        , m_heap_size(GC_HEAP_SIZE)
        , m_trace_global_roots(trace_global_roots_)
#ifdef PZ_DEV
        , in_no_gc_scope(false)
#endif
{ }

Heap::~Heap()
{
    // Check that finalise was called.
    assert(!m_bblock);
}

bool
Heap::init()
{
    init_statics();

    m_bblock = static_cast<BBlock*>(mmap(NULL, GC_BBLOCK_SIZE,
            PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0));
    if (MAP_FAILED == m_bblock) {
        perror("mmap");
        return false;
    }

    return true;
}

bool
Heap::finalise()
{
    if (!m_bblock)
        return true;

    bool result = -1 != munmap(m_bblock, GC_MAX_HEAP_SIZE);
    if (!result) {
        perror("munmap");
    }

    m_bblock = nullptr;
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
        !is_empty())
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
    size_t size_in_words = AlignUp(size_in_bytes, WORDSIZE_BYTES) /
        WORDSIZE_BYTES;

    return alloc(size_in_words, gc_cap);
}

void *
Heap::try_allocate(size_t size_in_words)
{
    Cell *cell;

    /*
     * Try the free list
     */
    size_in_words = size_in_words < GC_MIN_CELL_SIZE ? GC_MIN_CELL_SIZE :
        size_in_words;
    LBlock *block = get_free_list(size_in_words);
    if (!block) {
        block = allocate_block(size_in_words);
        if (!block) {
            #ifdef PZ_DEV
            if (m_options.gc_trace2()) {
                fprintf(stderr, "Heap full for allocation of %ld words\n",
                        size_in_words);
            }
            #endif
            return nullptr;
        }
    }

    cell = block->allocate_cell();

    if (!cell) return nullptr;

    #ifdef PZ_DEV
    if (m_options.gc_trace2()) {
        fprintf(stderr, "Allocated %p from free list\n", cell);
    }
    #endif

    return cell;
}

LBlock *
Heap::get_free_list(size_t size_in_words)
{
    for (unsigned i = 0; i < GC_LBLOCK_PER_BBLOCK; i++) {
        LBlock *lblock = &(m_bblock->m_blocks[i]);

        // TODO: Appropiriate size?
        if (lblock->is_in_use() && lblock->size() == size_in_words &&
                !lblock->is_full())
        {
            return lblock;
        }
    }

    return nullptr;
}

LBlock *
Heap::allocate_block(size_t size_in_words)
{
    LBlock *block;

    if (m_bblock->m_wilderness * GC_LBLOCK_SIZE >= m_heap_size)
        return nullptr;

    block = m_bblock->next_block();
    if (!block) return nullptr;

    #ifdef PZ_DEV
    if (m_options.gc_trace()) {
        fprintf(stderr, "Allocated new block for %ld cells\n", size_in_words);
    }
    #endif

    new(block) LBlock(size_in_words);

    return block;
}

/***************************************************************************/

void
Heap::collect(const AbstractGCTracer *trace_thread_roots)
{
    HeapMarkState state(this);

    // There's nothing to collect, the heap is empty.
    if (is_empty()) return;

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
Heap::mark(Cell *cell)
{
    unsigned num_marked = 0;

    cell->mark();
    num_marked++;

    void **ptr = cell->pointer();
    for (unsigned i = 0; i < cell->size(); i++) {
        void *cur = REMOVE_TAG(ptr[i]);
        if (is_valid_cell(cur)) {
            Cell *field = ptr_to_cell(cur);

            if (field->is_allocated() && !field->is_marked()) {
                num_marked += mark(field);
            }
        }
    }

    return num_marked;
}

void
Heap::sweep()
{
    m_bblock->sweep();
}

/***************************************************************************/

void
HeapMarkState::mark_root(void *heap_ptr)
{
    if (heap->is_valid_cell(heap_ptr)) {
        Cell *cell = heap->ptr_to_cell(heap_ptr);

        if (cell->is_allocated() && !cell->is_marked()) {
            num_marked += heap->mark(cell);
            num_roots_marked++;
        }
    }
}

void
HeapMarkState::mark_root_interior(void *heap_ptr)
{
    // This actually makes the pointer aligned to the GC's alignment.  We
    // should have a different macro for this particular use. (issue #154)
    heap_ptr = REMOVE_TAG(heap_ptr);
    if (heap->is_heap_address(heap_ptr)) {
        while (!heap->is_valid_cell(heap_ptr)) {
            heap_ptr -= WORDSIZE_BYTES;
        }
        // WIP: Maybe we want to calculate the block and call all these
        // methods on it here.  Then we're not re-calculating it in the
        // while loop and we can stop searching between blocks.
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

    if (new_size % sizeof(LBlock) != 0) return false;

    if (new_size < m_bblock->m_wilderness * GC_LBLOCK_SIZE) return false;

#ifdef PZ_DEV
    if (m_options.gc_trace()) {
        fprintf(stderr, "New heap size: %ld\n", new_size);
    }
#endif

    m_heap_size = new_size;
    return true;
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
Heap::check_heap() const
{
    assert(s_statics_initalised);
    assert(m_bblock != NULL);
    assert(m_heap_size >= s_page_size);
    assert(m_heap_size % s_page_size == 0);
    assert(m_heap_size % GC_LBLOCK_SIZE == 0);

    // TODO Check the free list for consistency.
    // TODO check to avoid duplicates
    // TODO check to avoid free cells not on the free list.
}

bool Heap::is_empty() const
{
    return m_bblock == nullptr || m_bblock->is_empty();
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

