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
pz_init_closure(PZ_Heap *heap, uint8_t *code, void *data, void *top_of_stack)
{
    PZ_Closure *closure = pz_gc_alloc_bytes(heap, sizeof(PZ_Closure),
            top_of_stack);

    closure->code = code;
    closure->data = data;

    return closure;
}

