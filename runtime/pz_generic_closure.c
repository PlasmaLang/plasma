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
pz_init_closure(uint8_t *code, void *data)
{
    PZ_Closure *closure = malloc(sizeof(PZ_Closure));

    closure->code = code;
    closure->data = data;

    return closure;
}

void
pz_closure_free(PZ_Closure *closure)
{
    free(closure);
}

