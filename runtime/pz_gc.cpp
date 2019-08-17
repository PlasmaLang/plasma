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
 * only needs to be good enough to ensure we recover memory.  It is
 * currently a little bit better than that.
 *
 *  * Mark/Sweep
 *  * Non-moving
 *  * Conservative
 *  * Interior pointers (up to 7 byte offset)
 *  * Block based, each block contains cells of a particular size, a marking
 *    bitmap and free list pointer (the free list is made of unused cell
 *    contents.
 *  * Blocks (LBlocks) are allocated from BBlocks (big blocks).  We allocate
 *    big blocks from the OS.
 *
 * This is about the simplest GC one could imagine, it is very naive in the
 * short term we should:
 *
 *  * Support larger allocations:
 *    https://github.com/PlasmaLang/plasma/issues/188
 *  * Use a mark stack
 *  * Tune "when to collect" decision.
 *  * Plus other open bugs in the bugtracker:
 *    https://github.com/PlasmaLang/plasma/labels/component%3A%20gc 
 *
 * In the slightly longer term we should:
 *
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
BBlock::is_empty() const
{
    for (unsigned i = 0; i < m_wilderness; i++) {
        if (m_blocks[i].is_in_use()) return false;
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
        , m_max_size(GC_Heap_Size)
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

    m_bblock = BBlock::new_bblock();
    return m_bblock != nullptr ? true : false;
}

BBlock*
BBlock::new_bblock()
{
    BBlock *block;

    block = static_cast<BBlock*>(mmap(NULL, GC_BBlock_Size,
            PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0));
    if (MAP_FAILED == block) {
        perror("mmap");
        return nullptr;
    }

    return block;
}

bool
Heap::finalise()
{
    if (!m_bblock)
        return true;

    bool result = -1 != munmap(m_bblock, GC_Max_Heap_Size);
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
    assert(cell_size_ >= GC_Min_Cell_Size);
    memset(m_header.bitmap, 0, GC_Cells_Per_LBlock * sizeof(uint8_t));

#if PZ_DEV
    if (options.gc_poison()) {
        memset(m_bytes, Poison_Byte, Payload_Bytes);
    }
#endif

    sweep(options);
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
        if (m_blocks[i].is_in_use()) {
            num_blocks += 1;
        }
    }

    return num_blocks * GC_LBlock_Size;
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
    assert(m_max_size % GC_LBlock_Size == 0);

    m_bblock->check();
}

void
BBlock::check()
{
    assert(m_wilderness < GC_LBlock_Per_BBlock);

    for (unsigned i = 0; i < m_wilderness; i++) {
        m_blocks[i].check();
    }
}

void
LBlock::check()
{
    if (!is_in_use()) return;

    assert(size() >= GC_Min_Cell_Size);
    assert(size() <= LBlock::Max_Cell_Size);
    assert(num_cells() <= GC_Cells_Per_LBlock);

    unsigned num_free_ = 0;
    for (unsigned i = 0; i < num_cells(); i++) {
        CellPtr cell(this, i);

        if (!is_allocated(cell)) {
            assert(!is_marked(cell));

            assert(is_in_free_list(cell));

            num_free_++;
        } else {
            assert(!is_in_free_list(cell));
        }
    }

    assert(num_free() == num_free_);
}

bool
LBlock::is_in_free_list(CellPtr &search)
{
    int cur = m_header.free_list;

    while (cur != Header::Empty_Free_List) {
        assert(cur >= 0);
        CellPtr cell(this, unsigned(cur));
        if (search.index() == cell.index()) {
            return true;
        }
        cur = cell.next_in_list();
    }

    return false;
}

unsigned
LBlock::num_free()
{
    int cur = m_header.free_list;
    unsigned num = 0;

    while (cur != Header::Empty_Free_List) {
        num++;
        assert(cur >= 0);
        CellPtr cell(this, unsigned(cur));
        cur = cell.next_in_list();
    }

    return num;
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
        m_wilderness, GC_LBlock_Per_BBlock,
        m_wilderness * GC_LBlock_Size / 1024);
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

