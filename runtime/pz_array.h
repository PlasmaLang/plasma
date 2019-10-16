/*
 * Plasma GC-compatible bounds-checked array
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2019 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#ifndef PZ_ARRAY_H
#define PZ_ARRAY_H

#include "string.h"

#include "pz_gc_util.h"

namespace pz {

template<typename T>
class Array : public GCNew {
  private:
    /*
     * The array data is stored seperately.  Array types can be
     * passed-by-value and easilly embeded within other values.
     */
    size_t  m_len;
    T      *m_data;

  public:
    Array(GCCapability &gc_cap, size_t len) : m_len(len) {
        assert(m_len > 0);
        m_data = new (gc_cap) T[len];
    }

    const T& operator[](size_t offset) const {
        assert(offset < m_len);
        return m_data[offset];
    }

    T& operator[](size_t offset) {
        assert(offset < m_len);
        return m_data[offset];
    }

    void zerofill() {
        memset(m_data, 0, sizeof(T) * m_len);
    }

    /*
     * These are deleted until they're needed (and can be tested) later.
     */
    Array(const Array&) = delete;
    void operator=(const Array&) = delete;
};

} // namespace pz

#endif /* ! PZ_ARRAY_H */
