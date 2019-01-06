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

typedef struct PZ_Closure_S PZ_Closure;

PZ_Closure *
pz_init_closure(pz::Heap *heap, uint8_t *code, void *data,
                trace_fn trace_thread_roots, void *trace_data);

#endif // ! PZ_CLOSURE_H
