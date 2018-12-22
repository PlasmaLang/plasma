/*
 * Plasma GC rooting
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2018 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#include "pz_common.h"

#include "pz_gc.h"
#include "pz_gc_rooting.h"

namespace pz {

void Tracer::do_trace(PZ_Heap_Mark_State *state) const
{
    for (auto root : roots) {
        pz_gc_mark_root(state, *(void**)root);
    }
}

void Tracer::add_root(void *root)
{
    roots.push_back(root);
}

void Tracer::remove_root(void *root)
{
    assert(!roots.empty());
    assert(roots.back() == root);
    roots.pop_back();
}

}
