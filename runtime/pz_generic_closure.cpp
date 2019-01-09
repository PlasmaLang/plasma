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
pz_alloc_closure(pz::Heap *heap, 
        trace_fn trace_thread_roots, void *trace_data)
{
    return static_cast<PZ_Closure*>(
            heap->alloc_bytes(sizeof(PZ_Closure),
                trace_thread_roots, trace_data));
}

void
pz_init_closure(PZ_Closure *closure, uint8_t *code, void *data)
{
    closure->code = code;
    closure->data = data;
}

