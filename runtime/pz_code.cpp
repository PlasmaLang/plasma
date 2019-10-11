/*
 * Plasma bytecode code structures and functions
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2015-2016, 2018-2019 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#include "pz_common.h"

#include "pz_code.h"

namespace pz {

Proc::Proc(NoGCScope &gc_cap, unsigned size) :
    m_code_size(size)
{
    m_code = (uint8_t*)gc_cap.alloc_bytes_meta(size);
}

} // namespace pz

