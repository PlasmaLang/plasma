/*
 * Plasma GC rooting, scopes & C++ allocation utilities
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2018-2020 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#include "pz_common.h"

#include "pz_util.h"

#include "pz_gc.h"
#include "pz_gc_util.h"

#include "pz_gc.impl.h"

namespace pz {

void * GCCapability::alloc(size_t size_in_words, AllocOpts opts)
{
    assert(m_heap);
    return m_heap->alloc(size_in_words, *this, opts);
}

void * GCCapability::alloc_bytes(size_t size_in_bytes, AllocOpts opts)
{
    assert(m_heap);
    return m_heap->alloc_bytes(size_in_bytes, *this, opts);
}

const AbstractGCTracer & GCCapability::tracer() const
{
    assert(can_gc());
    return *static_cast<const AbstractGCTracer *>(this);
}

void AbstractGCTracer::oom(size_t size_bytes)
{
    fprintf(
        stderr, "Out of memory, tried to allocate %lu bytes.\n", size_bytes);
    abort();
}

void GCTracer::do_trace(HeapMarkState * state) const
{
    for (void * root : m_roots) {
        state->mark_root(*(void **)root);
    }
}

void GCTracer::add_root(void * root)
{
    m_roots.push_back(root);
}

void GCTracer::remove_root(void * root)
{
    assert(!m_roots.empty());
    assert(m_roots.back() == root);
    m_roots.pop_back();
}

NoGCScope::NoGCScope(const GCCapability * gc_cap)
    : GCCapability(gc_cap->heap())
#ifdef PZ_DEV
    , m_needs_check(true)
#endif
    , m_did_oom(false)
{
    if (gc_cap->can_gc()) {
#ifdef PZ_DEV
        m_heap = gc_cap->heap();
        m_heap->start_no_gc_scope();
    } else {
        m_heap = nullptr;
#endif
    }
}

NoGCScope::~NoGCScope()
{
#ifdef PZ_DEV
    if (m_heap) {
        m_heap->end_no_gc_scope();
    }

    if (m_needs_check) {
        fprintf(
            stderr,
            "Caller did not check the NoGCScope before the destructor ran.\n");
        abort();
    }
#endif

    if (m_did_oom) {
        fprintf(stderr,
                "Out of memory, tried to allocate %lu bytes.\n",
                m_oom_size);
        abort();
    }
}

void NoGCScope::oom(size_t size_bytes)
{
    if (!m_did_oom) {
        m_did_oom  = true;
        m_oom_size = size_bytes;
    }
}

void NoGCScope::abort_for_oom_slow(const char * label)
{
    assert(m_did_oom);
    fprintf(stderr,
            "Out of memory while %s, tried to allocate %ld bytes.\n",
            label,
            m_oom_size);
    abort();
}

/****************************************************************************/

static void * do_new(size_t size, GCCapability & gc_cap, AllocOpts opts);

/*
 * This is not exactly conformant to C++ normals/contracts.  It doesn't call
 * the new handler when allocation fails which is what should normally
 * happen.  However the GC's alloc_bytes function already makes an attempt to
 * recover memory via the GCCapability parameter.
 *
 * See: Scott Meyers: Effective C++ Digital Collection, Item 51 regarding
 * this behaviour.
 */
void * GCNew::operator new(size_t size, GCCapability & gc_cap)
{
    return do_new(size, gc_cap, NORMAL);
}

void * GCNewTrace::operator new(size_t size, GCCapability & gc_cap)
{
    return do_new(size, gc_cap, TRACE);
}

static void * do_new(size_t size, GCCapability & gc_cap, AllocOpts opts)
{
    if (0 == size) {
        size = 1;
    }

    void * mem = gc_cap.alloc_bytes(size, opts);
    if (!mem) {
        fprintf(stderr, "Out of memory in operator new!\n");
        abort();
    }

    return mem;
}

}  // namespace pz

void * operator new[](size_t size, pz::GCCapability & gc_cap)
{
    return pz::do_new(size, gc_cap, pz::NORMAL);
}
