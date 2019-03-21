/*
 * Plasma bytecode code structures and functions
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2018-2019 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#ifndef PZ_CLOSURE_H
#define PZ_CLOSURE_H

#include "pz_gc.h"
#include "pz_gc_rooting.h"

namespace pz {

struct Closure;

Closure *
alloc_closure(Heap *heap,
        trace_fn trace_thread_roots, void *trace_data);

void
init_closure(Closure *closure, uint8_t *code, void *data);

inline Closure *
alloc_closure_cxx(Heap *heap, Traceable &traceable)
{
    return alloc_closure(heap, Traceable::trace, &traceable);
}

}

#endif // ! PZ_CLOSURE_H
