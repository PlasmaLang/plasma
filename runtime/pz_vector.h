/*
 * Plasma GC-compatible bounds-checked array
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2019, 2021 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#ifndef PZ_VECTOR_H
#define PZ_VECTOR_H

#include "string.h"

#include "pz_gc_util.h"

namespace pz {

template <typename T>
class Vector : public GCNew
{
   private:
    /*
     * The array data is stored seperately.  Array types can be
     * passed-by-value and easilly embeded within other values.
     */
    size_t m_len;
    size_t m_capacity;
    T *    m_data;

   public:
    Vector(NoGCScope & gc, size_t capacity = 8)
        : m_len(0)
        , m_capacity(capacity)
    {
        if (m_capacity > 0) {
            m_data = new (gc) T[m_capacity];
        } else {
            m_data = nullptr;
        }
    }

    size_t size() const
    {
        return m_len;
    }

    const T & operator[](size_t offset) const
    {
        assert(offset < m_len);
        return m_data[offset];
    }

    T & operator[](size_t offset)
    {
        assert(offset < m_len);
        return m_data[offset];
    }

    const T & front() const
    {
        assert(m_len > 0);
        return m_data[0];
    }

    T & front()
    {
        assert(m_len > 0);
        return m_data[0];
    }

    const T & back() const
    {
        assert(m_len > 0);
        return m_data[m_len - 1];
    }

    T & back()
    {
        assert(m_len > 0);
        return m_data[m_len - 1];
    }

    class Iterator : public std::iterator<std::input_iterator_tag, T> {
      private:
        const Vector   *m_vector;
        size_t          m_pos;

      protected:
        friend class Vector;
        Iterator(const Vector *v, size_t pos) : m_vector(v), m_pos(pos) {}

      public:
        bool operator!=(const Iterator &r) const {
            assert(m_vector == r.m_vector);
            return m_pos != r.m_pos;
        }

        Iterator& operator++() {
            m_pos++;
            return *this;
        }

        const T& operator*() {
            return (*m_vector)[m_pos];
        };
    };

    Iterator begin() const {
        return Iterator(this, 0);
    }

    Iterator end() const {
        return Iterator(this, m_len);
    }

    bool append(GCCapability & gc_cap, T value)
    {
        if (m_len == m_capacity) {
            if (!grow(gc_cap)) return false;
        }
        
        assert(m_len < m_capacity);
        m_data[m_len++] = value;
        return true;
    }

    bool grow(GCCapability & gc_cap)
    {
        if (m_capacity) {
            assert(m_data);
            // TODO: Tune this, right now we double the size of the array.
            // TODO: Implement realloc in the GC (Bug #208).
            T * new_data = new (gc_cap) T[m_capacity * 2];
            if (!new_data) return false;
            for (unsigned i = 0; i < m_len; i++) {
                new_data[i] = m_data[i];
            }
            m_data = new_data;
            m_capacity *= 2;
        } else {
            assert(!m_data);
            m_data     = new (gc_cap) T[8];
            m_capacity = 8;
        }
        return true;
    }

    /*
     * These are deleted until they're needed (and can be tested) later.
     */
    Vector(const Vector &) = delete;
    void operator=(const Vector &) = delete;
};

}  // namespace pz

#endif /* ! PZ_VECTOR_H */
