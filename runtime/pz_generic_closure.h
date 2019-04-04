/*
 * Plasma closures
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2018-2019 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#ifndef PZ_GENERIC_CLOSURE_H
#define PZ_GENERIC_CLOSURE_H

#include "pz_gc.h"

namespace pz {

struct Closure {
    void     *code;
    void     *data;
};

Closure *
new_closure(Heap *heap, GCCapability &gc_cap, uint8_t *code, void *data);

}

#endif // !PZ_GENERIC_CLOSURE_H
