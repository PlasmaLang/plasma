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

static size_t
s_page_size;
static bool
s_statics_initalised = false;

/***************************************************************************
 *
 * These procedures will likely move somewhere else, but maybe after some
 * refactoring.
 */

size_t
heap_get_max_size(const Heap *heap)
{
    return heap->max_size();
}

bool
heap_set_max_size(Heap *heap, size_t new_size)
{
    return heap->set_max_size(new_size);
}

size_t
heap_get_size(const Heap *heap)
{
    return heap->size();
}

unsigned
heap_get_collections(const Heap *heap)
{
    return heap->collections();
}

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

bool
BBlock::is_empty() const
{
    for (unsigned i = 0; i < GC_LBLOCK_PER_BBLOCK; i++) {
        if (!m_blocks[i].is_empty()) return false;
    }
    return true;
}

bool Heap::is_empty() const
{
    return m_bblock == nullptr || m_bblock->is_empty();
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
        , m_max_size(GC_HEAP_SIZE)
        , m_collections(0)
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

LBlock::LBlock(const Options &options, size_t cell_size_) :
        m_header(cell_size_)
{
    assert(cell_size_ >= GC_MIN_CELL_SIZE);
    memset(m_header.bitmap, 0, GC_CELLS_PER_LBLOCK * sizeof(uint8_t));

#if PZ_DEV
    if (options.gc_poison()) {
        memset(m_bytes, PoisonByte, PAYLOAD_BYTES);
    }
#endif
}

/***************************************************************************/

size_t
Heap::max_size() const
{
    return m_max_size;
}

bool
Heap::set_max_size(size_t new_size)
{
    assert(s_statics_initalised);
    if (new_size < s_page_size) return false;

    if (new_size % sizeof(LBlock) != 0) return false;

    if (new_size < m_bblock->size()) return false;

#ifdef PZ_DEV
    if (m_options.gc_trace()) {
        fprintf(stderr, "New heap size: %ld\n", new_size);
    }
#endif

    m_max_size = new_size;
    return true;
}

size_t
Heap::size() const
{
    if (m_bblock) {
        return m_bblock->size();
    } else {
        return 0;
    }
}

unsigned
Heap::collections() const
{
    return m_collections;
}

size_t
BBlock::size() const
{
    size_t num_blocks = 0;

    for (unsigned i = 0; i < m_wilderness; i++) {
        if (!m_blocks[i].is_empty()) {
            num_blocks += 1;
        }
    }

    return num_blocks * 4096;
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
    assert(m_max_size >= s_page_size);
    assert(m_max_size % s_page_size == 0);
    assert(m_max_size % GC_LBLOCK_SIZE == 0);

    // TODO Check the free list for consistency.
    // TODO check to avoid duplicates
    // TODO check to avoid free cells not on the free list.
}
#endif

/***************************************************************************/

#ifdef PZ_DEV
void
Heap::print_usage_stats() const
{
    m_bblock->print_usage_stats();
}

void
BBlock::print_usage_stats() const
{
    printf("\nBBLOCK\n------\n");
    printf("Num lblocks: %d/%ld, %ldKB\n",
        m_wilderness, GC_LBLOCK_PER_BBLOCK,
        m_wilderness * GC_LBLOCK_SIZE / 1024);
    for (unsigned i = 0; i < m_wilderness; i++) {
        m_blocks[i].print_usage_stats();
    }
}

void
LBlock::print_usage_stats() const
{
    if (is_in_use()) {
        unsigned cells_used = 0;
        for (unsigned i = 0; i < num_cells(); i++) {
            CellPtr cell(const_cast<LBlock*>(this), i);
            if (is_allocated(cell)) {
                cells_used++;
            }
        }
        printf("Lblock for %ld-word objects: %d/%d cells\n",
            size(), cells_used, num_cells());
    } else {
        printf("Lblock out of use\n");
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

