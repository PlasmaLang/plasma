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
new_closure(GCCapability &can_gc, uint8_t *code, void *data)
{
    Closure *closure = static_cast<Closure*>(
            can_gc.alloc_bytes(sizeof(Closure)));

    if (closure) {
        new(closure) Closure(code, data);
    }

    return closure;
}

} // namespace pz

