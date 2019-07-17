/*
 * Plasma garbage collector collection procedures 
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2018-2019 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#include "pz_common.h"

#include <string.h>

#include "pz_util.h"

#include "pz_gc.h"
#include "pz_gc_util.h"

#include "pz_gc.impl.h"
#include "pz_gc_layout.h"

namespace pz {

/*
 * Mask off the low bits so that we can see the real pointer rather than a
 * tagged pointer.
 */
constexpr uintptr_t TAG_BITS = WORDSIZE_BYTES - 1;

constexpr void*
REMOVE_TAG(void* tagged_ptr) {
    return reinterpret_cast<void*>(
            reinterpret_cast<uintptr_t>(tagged_ptr) & (~0 ^ TAG_BITS));
}

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
Heap::mark(CellPtr &cell)
{
    unsigned num_marked = 0;

    assert(cell.isValid());
    LBlock *lblock = cell.lblock();

    lblock->mark(cell);
    num_marked++;

    void **ptr = cell.pointer();
    for (unsigned i = 0; i < lblock->size(); i++) {
        void *cur = REMOVE_TAG(ptr[i]);
        if (is_valid_cell(cur)) {
            CellPtr field = ptr_to_cell(cur);
            LBlock *field_lblock = field.lblock();

            if (field_lblock->is_allocated(field) &&
                    !field_lblock->is_marked(field)) {
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

void
BBlock::sweep()
{
    for (unsigned i = 0; i < GC_LBLOCK_PER_BBLOCK; i++) {
        m_blocks[i].sweep();
    }
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

/***************************************************************************/

void
HeapMarkState::mark_root(void *heap_ptr)
{
    if (heap->is_valid_cell(heap_ptr)) {
        CellPtr cell = heap->ptr_to_cell(heap_ptr);
        assert(cell.isValid());
        LBlock *lblock = cell.lblock();

        if (lblock->is_allocated(cell) && !lblock->is_marked(cell)) {
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

} // namespace pz
