/*
 * Plasma closures
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#ifndef PZ_GENERIC_CLOSURE_H
#define PZ_GENERIC_CLOSURE_H

#include "pz_gc_util.h"

namespace pz {

class Closure : public GCNew
{
   private:
    void * m_code;
    void * m_data;

   public:
    Closure() : m_code(nullptr), m_data(nullptr) {}

    Closure(void * code, void * data) : m_code(code), m_data(data) {}

    void init(void * code, void * data)
    {
        assert(!m_code);
        m_code = code;
        m_data = data;
    }

    void * code() const
    {
        return m_code;
    }
    void * data() const
    {
        return m_data;
    }
};

}  // namespace pz

#endif  // !PZ_GENERIC_CLOSURE_H
