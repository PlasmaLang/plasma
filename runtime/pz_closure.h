/*
 * Plasma bytecode code structures and functions
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2018 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#ifndef PZ_CLOSURE_H
#define PZ_CLOSURE_H

#include "pz_gc.h"
#include "pz_gc_rooting.h"

typedef struct PZ_Closure_S PZ_Closure;

PZ_Closure *
pz_alloc_closure(pz::Heap *heap,
        trace_fn trace_thread_roots, void *trace_data);

void
pz_init_closure(PZ_Closure *closure, uint8_t *code, void *data);

namespace pz {

inline PZ_Closure *
pz_alloc_closure_cxx(Heap *heap, Traceable &traceable)
{
    return pz_alloc_closure(heap, Traceable::trace, &traceable);
}

}

#endif // ! PZ_CLOSURE_H
