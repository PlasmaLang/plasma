/*
 * Plasma GC rooting
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2018-2019 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#include "pz_common.h"

#include "pz_gc.h"
#include "pz_gc_rooting.h"

namespace pz {

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

}
