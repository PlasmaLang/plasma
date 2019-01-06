/*
 * Plasma closures
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2018 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#include "pz_common.h"

#include "pz_closure.h"
#include "pz_generic_closure.h"

PZ_Closure *
pz_init_closure(pz::Heap *heap, uint8_t *code, void *data,
        trace_fn trace_thread_roots, void *trace_data)
{
    PZ_Closure *closure = static_cast<PZ_Closure*>(
            heap->alloc_bytes(sizeof(PZ_Closure),
                trace_thread_roots, trace_data));

    closure->code = code;
    closure->data = data;

    return closure;
}

