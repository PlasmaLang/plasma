/*
 * Plasma bytecode code structures and functions
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2015-2016, 2018 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#include "pz_common.h"

#include "pz_code.h"

namespace pz {

Proc::Proc(PZ_Heap *heap, unsigned size) :
    code_size(size)
{
    code_ = (uint8_t*)pz_gc_alloc_bytes(heap, size, NULL);
}

} // namespace pz

