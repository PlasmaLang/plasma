/*
 * Plasma bytecode code structures and functions
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2015-2016, 2018-2019 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#ifndef PZ_CODE_H
#define PZ_CODE_H

#include "pz_gc_util.h"
#include "pz_vector.h"

namespace pz {

struct OffsetContext {
    OffsetContext() : offset(0), line(0) {}
    OffsetContext(unsigned offset_, unsigned line_)
        : offset(offset_)
        , line(line_)
    {}

    unsigned offset;
    unsigned line;
};

/*
 * Code layout in memory
 *
 *************************/
class Proc : public GCNew
{
   private:
    uint8_t * m_code;
    unsigned m_code_size;
    const char * m_name = nullptr;

    bool m_is_builtin;
    const char * m_filename = nullptr;
    Vector<OffsetContext> m_contexts;

   public:
    Proc(NoGCScope & gc_cap, const char * name, bool is_builtin, unsigned size);

    void set_name(const char * name)
    {
        m_name = name;
    }
    const char * name() const
    {
        return m_name;
    }

    uint8_t * code() const
    {
        return m_code;
    }
    unsigned size() const
    {
        return m_code_size;
    }

    bool is_builtin() const
    {
        return m_is_builtin;
    }

    Proc()             = delete;
    Proc(const Proc &) = delete;
    void operator=(const Proc & other) = delete;

    // Add context information for this and the following code offsets.
    void add_context(GCCapability & gc_cap, unsigned offset,
                     const char * filename, unsigned line);
    void add_context(GCCapability & gc_cap, unsigned offset, unsigned line);
    // This and the following code offsets have no context infomation.
    void no_context(GCCapability & gc_cap, unsigned offset);

    const char * filename() const
    {
        return m_filename;
    }
    unsigned line(unsigned offset, unsigned * last_lookup) const;

   private:
    void set_context(GCCapability & gc_cap, unsigned offset, unsigned value);
};

}  // namespace pz

#endif /* ! PZ_CODE_H */
