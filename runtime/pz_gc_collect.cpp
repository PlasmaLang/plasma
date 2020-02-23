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

// TODO: This can't be constexpr due to the casts. It'd be nice if it could
// be.
void*
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

    num_marked += do_mark_special_field(cell);
    void **ptr = cell.pointer();
    for (unsigned i = 0; i < cell_size; i++) {
        num_marked += mark_field(REMOVE_TAG(ptr[i]));
    }

    return num_marked;
}

unsigned
Heap::mark_field(void *cur)
{
    CellPtrBOP field_bop = ptr_to_bop_cell(cur);
    if (field_bop.is_valid()) {
        /*
         * Note that because we use conservative we may find values that
         * exactly match valid but unallocated cells.  Therefore we also
         * test is_allocated().
         */
        if (field_bop.is_allocated() &&
                !field_bop.is_marked()) {
            return mark(field_bop);
        }
    } else {
        CellPtrFit field_fit = ptr_to_fit_cell(cur);
        if (field_fit.is_valid()) {
            /*
             * We also test is_allocated() here, see the above comment.
             */
            if (field_fit.is_allocated() &&
                    !field_fit.is_marked()) {
                return mark(field_fit);
            }
        }
    }

    return 0;
}

unsigned
Heap::do_mark_special_field(CellPtrBOP &cell)
{
    return 0;
}

unsigned
Heap::do_mark_special_field(CellPtrFit &cell)
{
    return mark_field(*cell.meta());
}

unsigned
Heap::do_mark(CellPtrBOP &cell)
{
    cell.mark();
    return cell.block()->size();
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
    m_chunk_fit->sweep(m_options);

    m_usage = m_chunk_bop->usage() + m_chunk_fit->usage();
    m_threshold = size_t(m_usage * GC_Threshold_Factor);
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
        if (cell.is_marked()) {
            // Cell is marked, clear the mark bit, keep the allocated bit.
            cell.unmark();
            num_used++;
        } else {
            // Free the cell.
            cell.unallocate();
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
ChunkFit::sweep(const Options &options)
{
    for (CellPtrFit cell = first_cell();
            cell.is_valid();
            cell = cell.next_in_chunk())
    {
        if (cell.is_marked()) {
            cell.unmark();
        } else {
#ifdef PZ_DEV
            static int seldom_used_path = 0;
            seldom_used_path++;
            if (seldom_used_path > 100) {
                fprintf(stderr,
                        "Running previously-unused code path, "
                        "see https://github.com/PlasmaLang/plasma/issues/196\n");
            }
            if (options.gc_poison()) {
                memset(cell.meta(), Poison_Byte, sizeof(*cell.meta()));
                // We cannot poison the first word of the cell since that
                // contains the next pointer.
                memset(reinterpret_cast<uint8_t*>(cell.pointer()) + 
                        WORDSIZE_BYTES,
                    Poison_Byte, (cell.size() - 1) * WORDSIZE_BYTES);
            }
            cell.check();
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

    if (cell_bop.is_allocated() && !cell_bop.is_marked()) {
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
         p_cur < (void**)((uint8_t*)root + len);
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
         p_cur < (void**)((uint8_t*)root + len);
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
