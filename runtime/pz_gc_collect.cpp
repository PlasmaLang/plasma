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
#include "pz_gc_layout.impl.h"

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
    size_t initial_usage = usage();

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
        print_usage_stats(initial_usage);
    }
#endif
}

template<typename Cell>
unsigned
Heap::mark(Cell &cell)
{
    unsigned num_marked = 0;
    size_t cell_size;

    assert(cell.is_valid());
    cell_size = do_mark(cell);
    num_marked++;

    void **ptr = cell.pointer();
    for (unsigned i = 0; i < cell_size; i++) {
        void *cur = REMOVE_TAG(ptr[i]);

        CellPtrBOP field_bop = ptr_to_bop_cell(cur);
        if (field_bop.is_valid()) {
            Block *field_block = field_bop.block();

            /*
             * Note that because we use conservative we may find values that
             * exactly match valid but unallocated cells.  Therefore we also
             * test is_allocated().
             */
            if (field_block->is_allocated(field_bop) &&
                    !field_block->is_marked(field_bop)) {
                num_marked += mark(field_bop);
            }
        } else {
            CellPtrFit field_fit = ptr_to_fit_cell(cur);
            if (field_fit.is_valid()) {
                /*
                 * We also test is_allocated() here, see the above comment.
                 */
                if (field_fit.is_allocated() &&
                        !field_fit.is_marked()) {
                    num_marked += mark(field_fit);
                }
            }
        }
    }

    return num_marked;
}

unsigned
Heap::do_mark(CellPtrBOP &cell)
{
    Block *block = cell.block();
    block->mark(cell);
    return block->size();
}

unsigned
Heap::do_mark(CellPtrFit &cell)
{
    cell.mark();
    return cell.size();
}

void
Heap::sweep()
{
    m_chunk_bop->sweep(m_options);
    m_chunk_fit->sweep();

    m_usage = m_chunk_bop->usage() + m_chunk_fit->usage();
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
                memset(cell.pointer(), Poison_Byte, size() * WORDSIZE_BYTES);
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

void
ChunkFit::sweep()
{
    for (CellPtrFit cell = first_cell();
            cell.is_valid();
            cell = cell.next_in_chunk())
    {
        if (cell.is_marked()) {
            cell.unmark();
        } else {
#ifdef PZ_DEV
            fprintf(stderr,
                    "Running previously-unused code path, "
                    "see https://github.com/PlasmaLang/plasma/issues/196\n");
#endif
            // TODO: Free the cell
        }
    }
}

/****************************************************************************/

CellPtrBOP
Heap::ptr_to_bop_cell(void *ptr) const
{
    if (m_chunk_bop->contains_pointer(ptr)) {
        Block *block = m_chunk_bop->ptr_to_block(ptr);
        if (block && block->is_in_use() && block->is_valid_address(ptr)) {
            return CellPtrBOP(block, block->index_of(ptr), ptr);
        } else {
            return CellPtrBOP::Invalid();
        }
    } else {
        return CellPtrBOP::Invalid();
    }
}

CellPtrBOP
Heap::ptr_to_bop_cell_interior(void *ptr) const
{
    if (m_chunk_bop->contains_pointer(ptr)) {
        Block *block = m_chunk_bop->ptr_to_block(ptr);
        if (block && block->is_in_use()) {
            // Compute index then re-compute pointer to find the true
            // beginning of the cell.
            unsigned index = block->index_of(ptr);
            ptr = block->index_to_pointer(index);
            return CellPtrBOP(block, index, ptr);
        } else {
            return CellPtrBOP::Invalid();
        }
    } else {
        return CellPtrBOP::Invalid();
    }
}

CellPtrFit
Heap::ptr_to_fit_cell(void *ptr) const
{
    if (m_chunk_fit->contains_pointer(ptr)) {
        // TODO Speed up this search with a crossing-map.
        for (CellPtrFit cell = m_chunk_fit->first_cell(); cell.is_valid();
                cell = cell.next_in_chunk())
        {
            if (cell.pointer() == ptr) {
                return cell;
            } else if (cell.pointer() > ptr) {
                // The pointer points into the middle of a cell.
                return CellPtrFit::Invalid();
            }
        }
        return CellPtrFit::Invalid();
    } else {
        return CellPtrFit::Invalid();
    }
}

CellPtrFit
Heap::ptr_to_fit_cell_interior(void *ptr) const
{
    if (m_chunk_fit->contains_pointer(ptr)) {
        // TODO Speed up this search with a crossing-map.
        CellPtrFit prev = CellPtrFit::Invalid();
        for (CellPtrFit cell = m_chunk_fit->first_cell(); cell.is_valid();
                cell = cell.next_in_chunk())
        {
            if (cell.pointer() == ptr) {
                return cell;
            } else if (cell.pointer() > ptr) {
                if (prev.is_valid()) {
                    return prev;
                } else {
                    return CellPtrFit::Invalid();
                }
            }

            prev = cell;
        }
        return CellPtrFit::Invalid();
    } else {
        return CellPtrFit::Invalid();
    }
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
HeapMarkState::mark_root(CellPtrFit &cell_fit)
{
    assert(cell_fit.is_valid());

    if (cell_fit.is_allocated() && !cell_fit.is_marked()) {
        num_marked += heap->mark(cell_fit);
        num_roots_marked++;
    }
}

void
HeapMarkState::mark_root(void *heap_ptr)
{
    CellPtrBOP cell_bop = heap->ptr_to_bop_cell(heap_ptr);
    if (cell_bop.is_valid()) {
        mark_root(cell_bop);
        return;
    }

    CellPtrFit cell_fit = heap->ptr_to_fit_cell(heap_ptr);
    if (cell_fit.is_valid()) {
        mark_root(cell_fit);
        return;
    }
}

void
HeapMarkState::mark_root_interior(void *heap_ptr)
{
    // This actually makes the pointer aligned to the GC's alignment.  We
    // should have a different macro for this particular use. (issue #154)
    heap_ptr = REMOVE_TAG(heap_ptr);

    CellPtrBOP cell_bop = heap->ptr_to_bop_cell_interior(heap_ptr);
    if (cell_bop.is_valid()) {
        mark_root(cell_bop);
        return;
    }

    CellPtrFit cell_fit = heap->ptr_to_fit_cell_interior(heap_ptr);
    if (cell_fit.is_valid()) {
        mark_root(cell_fit);
        return;
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
