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
Proc::add_context(GCCapability &gc_cap, unsigned offset, unsigned line)
{
    assert(m_filename);
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
Proc::line(unsigned offset, unsigned *last_lookup) const
{
    unsigned start;
    if (*last_lookup == 0 || m_contexts[*last_lookup - 1].offset > offset) {
        start = 0;
    } else {
        start = *last_lookup - 1;
    }

    /*
     * The loop condition is such that i and i+1 are both within the bounds
     * of m_contexts.
     */
    for (unsigned i = start; i + 1 < m_contexts.size(); i++) {
        // If the current offset is between this and the next.
        if ((m_contexts[i].offset <= offset) &&
            (m_contexts[i+1].offset > offset))
        {
            *last_lookup = i;
            return m_contexts[i].line;
        }
    }
    if (m_contexts.size() > 0 && m_contexts.last().offset <= offset) {
        *last_lookup = m_contexts.size() - 1;
        return m_contexts.last().line;
    }

    return 0;
}


} // namespace pz

