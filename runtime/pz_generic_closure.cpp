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
alloc_closure(Heap *heap, GCCapability &can_gc)
{
    return static_cast<Closure*>(heap->alloc_bytes(sizeof(Closure), can_gc));
}

void
init_closure(Closure *closure, uint8_t *code, void *data)
{
    closure->code = code;
    closure->data = data;
}

} // namespace pz

