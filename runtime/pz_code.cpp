/*
 * Plasma bytecode code structures and functions
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2015-2016, 2018-2019 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#include "pz_common.h"

#include "pz_code.h"
#include "pz_gc.h"

namespace pz {

Proc::Proc(NoGCScope &gc_cap, bool is_builtin, unsigned size) :
    m_code_size(size),
    m_is_builtin(is_builtin),
    m_contexts(gc_cap, size)
{
    m_code = (uint8_t*)gc_cap.alloc_bytes_meta(size);
    heap_set_meta_info(gc_cap.heap(), code(), this);
    m_contexts.zerofill();
}

void
Proc::add_context(Heap *heap, unsigned offset, const char *filename,
        unsigned line)
{
    if (m_filename) {
        // Pointer equality is okay.
        assert(m_filename == filename);
    } else {
        m_filename = filename;
    }

    m_contexts[offset] = line;
}

unsigned
Proc::line(unsigned offset) const
{
    return m_contexts[offset];
}


} // namespace pz

