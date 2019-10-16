/*
 * Plasma bytecode code structures and functions
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2015-2016, 2018-2019 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#ifndef PZ_CODE_H
#define PZ_CODE_H

#include "pz_array.h"
#include "pz_gc_util.h"

namespace pz {

/*
 * Code layout in memory
 *
 *************************/
class Proc : public GCNew {
  private:
    uint8_t                *m_code;
    unsigned                m_code_size;

    bool                    m_is_builtin;
    const char             *m_filename;
    Array<unsigned>         m_contexts;

  public:
    Proc(NoGCScope &gc_cap, bool is_builtin, unsigned size);

    uint8_t * code() const { return m_code; }
    unsigned size() const { return m_code_size; }

    bool is_builtin() const { return m_is_builtin; }

    Proc() = delete;
    Proc(const Proc&) = delete;
    void operator=(const Proc &other) = delete;

    void add_context(Heap *heap, unsigned offset, const char *filename,
            unsigned line);

    const char * filename() const { return m_filename; }
    unsigned line(unsigned offset) const;
};

} // namespace pz

#endif /* ! PZ_CODE_H */
