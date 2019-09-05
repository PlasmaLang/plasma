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
    assert(!m_in_no_gc_scope);

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
    m_collections++;

#ifdef PZ_DEV
    if (m_options.gc_slow_asserts()) {
        check_heap();
    }
    if (m_options.gc_usage_stats()) {
        print_usage_stats();
    }
#endif
}

unsigned
Heap::mark(CellPtr &cell)
{
    unsigned num_marked = 0;
    size_t cell_size;

    assert(cell.is_valid());
    if (cell.is_bop_cell()) {
        CellPtrBOP cell_bop(cell.pointer());
        Block *block = cell_bop.block();
        block->mark(cell_bop);
        cell_size = block->size();
    } else {
        assert(cell.is_fit_cell());
        // TODO
        fprintf(stderr, "WIP: Fit cell in mark");
        abort();
    }
    num_marked++;

    void **ptr = cell.pointer();
    for (unsigned i = 0; i < cell_size; i++) {
        void *cur = REMOVE_TAG(ptr[i]);
        
        CellPtrBOP field_bop = ptr_to_bop_cell(cur);
        if (field_bop.is_valid()) {
            Block *field_block = field_bop.block();

            if (field_block->is_allocated(field_bop) &&
                    !field_block->is_marked(field_bop)) {
                num_marked += mark(field_bop);
            }
        } else {
            /*
             * No test for Fit cell yet
            fprintf(stderr, "WIP: Fit cell in mark (field)");
            abort();
            */
        }
    }

    return num_marked;
}

void
Heap::sweep()
{
    m_chunk_bop->sweep(m_options);
}

void
ChunkBOP::sweep(const Options &options)
{
    for (unsigned i = 0; i < m_wilderness; i++) {
        if (m_blocks[i].sweep(options)) {
            m_blocks[i].make_unused();
        }
    }
}

bool
Block::sweep(const Options &options)
{
    if (!is_in_use()) return true;

    int free_list = Header::Empty_Free_List;
    unsigned num_used = 0;

    for (unsigned i = 0; i < num_cells(); i++) {
        CellPtrBOP cell(this, i);
        if (is_marked(cell)) {
            // Cell is marked, clear the mark bit, keep the allocated bit.
            unmark(cell);
            num_used++;
        } else {
            // Free the cell.
            unallocate(cell);
#if PZ_DEV
            if (options.gc_poison()) {
                memset(cell.pointer(), Poison_Byte, size());
            }
#endif
            cell.set_next_in_list(free_list);
            free_list = cell.index();
        }
    }

    m_header.free_list = free_list;

    return num_used == 0;
}

void
Block::make_unused()
{
    m_header.block_type_or_size = Header::Block_Empty;
}

/***************************************************************************/

void
HeapMarkState::mark_root(CellPtrBOP &cell_bop)
{
    assert(cell_bop.is_valid());

    Block *block = cell_bop.block();

    if (block->is_allocated(cell_bop) && !block->is_marked(cell_bop)) {
        num_marked += heap->mark(cell_bop);
        num_roots_marked++;
    }
}

void
HeapMarkState::mark_root(void *heap_ptr)
{
    // TODO: Support fit cells.

    CellPtrBOP cell = heap->ptr_to_bop_cell(heap_ptr);
    if (cell.is_valid()) {
        mark_root(cell);
    }
}

void
HeapMarkState::mark_root_interior(void *heap_ptr)
{
    // TODO: Support Fit cells.

    // This actually makes the pointer aligned to the GC's alignment.  We
    // should have a different macro for this particular use. (issue #154)
    heap_ptr = REMOVE_TAG(heap_ptr);
    if (heap->is_heap_address(heap_ptr)) {
        while (!heap->is_valid_bop_cell(heap_ptr)) {
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
