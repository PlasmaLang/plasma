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

Proc::Proc(NoGCScope &gc_cap, const char *name, bool is_builtin,
        unsigned size) :
    m_code_size(size),
    m_name(name),
    m_is_builtin(is_builtin),
    m_contexts(gc_cap)
{
    m_code = (uint8_t*)gc_cap.alloc_bytes_meta(size);
    heap_set_meta_info(gc_cap.heap(), code(), this);
}

void
Proc::add_context(GCCapability &gc_cap, unsigned offset, const char *filename,
        unsigned line)
{
    if (m_filename) {
        // Pointer equality is okay.
        assert(m_filename == filename);
    } else {
        m_filename = filename;
    }

    set_context(gc_cap, offset, line);
}

void
Proc::no_context(GCCapability &gc_cap, unsigned offset)
{
    set_context(gc_cap, offset, 0);
}

void
Proc::set_context(GCCapability &gc_cap, unsigned offset, unsigned value)
{
    bool res = m_contexts.append(gc_cap, OffsetContext(offset, value));

    // We expect the return code to be true unless GCCapability is a
    // NoGCScope, and it probably isn't.
    assert(res);
    // Check that this isn't a NoGCScope so we know to fix the above
    // assumption if that changes.
    assert(gc_cap.can_gc());
}

unsigned
Proc::line(unsigned offset) const
{
    unsigned last = 0;

    for (unsigned i = 0; i < m_contexts.size(); i++) {
        if (m_contexts[i].offset == offset) {
            return m_contexts[i].line;
        } else if (m_contexts[i].offset > offset) {
            return last;
        }

        last = m_contexts[i].line;
    }

    return last;
}


} // namespace pz

