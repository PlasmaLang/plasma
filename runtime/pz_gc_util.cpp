/*
 * Plasma GC rooting, scopes & C++ allocation utilities
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2018-2019 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#include "pz_common.h"

#include "pz_gc.h"
#include "pz_gc_util.h"

#include "pz_gc.impl.h"

namespace pz {

void *
GCCapability::alloc(size_t size_in_words) const {
    assert(m_heap);
    return m_heap->alloc(size_in_words, *this);
}

void *
GCCapability::alloc_bytes(size_t size_in_bytes) const {
    assert(m_heap);
    return m_heap->alloc_bytes(size_in_bytes, *this);
}

const AbstractGCTracer& 
GCCapability::tracer() const {
    assert(can_gc());
    return *static_cast<const AbstractGCTracer*>(this);
}

void GCTracer::do_trace(HeapMarkState *state) const
{
    for (void *root : m_roots) {
        state->mark_root(*(void**)root);
    }
}

void GCTracer::add_root(void *root)
{
    m_roots.push_back(root);
}

void GCTracer::remove_root(void *root)
{
    assert(!m_roots.empty());
    assert(m_roots.back() == root);
    m_roots.pop_back();
}

NoGCScope::NoGCScope(const GCCapability *gc_cap) 
    : GCCapability(gc_cap->heap())
{
    if (gc_cap->can_gc()) {
        gc_cap->heap()->maybe_collect(&gc_cap->tracer());
#ifdef PZ_DEV
        m_heap = gc_cap->heap();
        m_heap->start_no_gc_scope();
    } else {
        m_heap = nullptr;
#endif
    }
}

NoGCScope::~NoGCScope() {
#ifdef PZ_DEV
    if (m_heap) {
        m_heap->end_no_gc_scope();
    }
#endif
}

static void *
do_new(size_t size, const GCCapability &gc_cap);

/*
 * This is not exactly conformant to C++ normals/contracts.  It doesn't call
 * the new handler when allocation fails which is what should normally
 * happen.  However the GC's alloc_bytes function already makes an attempt to
 * recover memory via the GCCapability parameter. 
 * 
 * See: Scott Meyers: Effective C++ Digital Collection, Item 51 regarding
 * this behaviour.
 */
void *
GCNew::operator new(size_t size, const GCCapability &gc_cap)
{
    return do_new(size, gc_cap);
}

void *
GCNew::operator new[](size_t size, const GCCapability &gc_cap)
{
    return do_new(size, gc_cap);
}

static void *
do_new(size_t size, const GCCapability &gc_cap)
{
    if (0 == size) {
        size = 1;
    }

    void *mem = gc_cap.alloc_bytes(size);
    if (!mem) {
        fprintf(stderr, "Out of memory in operator new!\n");
        abort();
    }

    return mem;
}

}
