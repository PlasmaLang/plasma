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
alloc_closure(Heap *heap, GCCapability &gc_cap);

void
init_closure(Closure *closure, uint8_t *code, void *data);

}

#endif // ! PZ_CLOSURE_H
