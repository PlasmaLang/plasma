/*
 * Plasma closures
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2018-2019 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#include "pz_common.h"

#include "pz_closure.h"
#include "pz_generic_closure.h"

namespace pz {

Closure *
alloc_closure(Heap *heap, 
        trace_fn trace_thread_roots, void *trace_data)
{
    return static_cast<Closure*>(
            heap->alloc_bytes(sizeof(Closure),
                trace_thread_roots, trace_data));
}

void
init_closure(Closure *closure, uint8_t *code, void *data)
{
    closure->code = code;
    closure->data = data;
}

} // namespace pz

