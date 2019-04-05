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

/*
 * This is not exactly conformant to C++ normals/contracts.  It doesn't call
 * the new handler when allocation fails which is what should normally
 * happen.  However the GC's alloc_bytes function already makes an attempt to
 * recover memory via the GCCapability parameter. 
 * 
 * See: Scott Meyers: Effective C++ Digital Collection, Item 51 regarding
 * this behaviour.
 */
void *
Closure::operator new(size_t size, GCCapability &gc_cap)
{
    if (0 == size) {
        size = 1;
    }

    void *mem = gc_cap.alloc_bytes(size);
    if (!mem) {
        fprintf(stderr, "Out of memory in operator new!\n");
        abort();
    }

    return mem;
}

} // namespace pz

