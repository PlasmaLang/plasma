/*
 * PZ C++ future library functions.
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2018-2019, 2021 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 *
 * This file contains library code that has been added to a more recent C++
 * version than the one we've standardised on (C++11) or features that we
 * might reasonably expect to be added to a future version.  If/when we move
 * to a newer standard we can delete entries here and update code as
 * necessary.
 */

#ifndef PZ_CXX_FUTURE_H
#define PZ_CXX_FUTURE_H

#include <string>

/*
 * C++17 libraries don't seem to be on my dev system,
 * other people might also be missing them.  So just implement this
 * ourselves.
 */
template <typename T>
class Optional
{
   private:
    bool m_present = false;

    /*
     * AlaskanEmily suggested this trick, allocate space for T here and use
     * placement new below so that T's without default constructors can be
     * used.
     */
    static_assert(sizeof(T) >= 1, "T must have non-zero size");
    alignas(alignof(T)) char m_data[sizeof(T)] = {0};

   public:
    constexpr Optional() {}

    // Implicit constructor
    Optional(const T & val)
    {
        set(val);
    }

    Optional(T && val) : m_present(true)
    {
        value() = std::move(val);
    }

    Optional(const Optional & other)
    {
        if (other.hasValue()) {
            set(other.value());
        }
    }

    Optional(Optional && other)
    {
        if (other.hasValue()) {
            set(other.release());
        }
    }

    ~Optional()
    {
        clear();
    }

    Optional & operator=(const Optional & other)
    {
        if (this != &other) {
            if (other.hasValue()) {
                set(other.value());
            } else {
                clear();
            }
        }

        return *this;
    }

    Optional & operator=(Optional && other)
    {
        if (this != &other) { 
            if (other.hasValue()) {
                set(other.release());
            } else {
                clear();
            }
        }

        return *this;
    }

    static constexpr Optional Nothing()
    {
        return Optional();
    }

    bool hasValue() const
    {
        return m_present;
    }

    void set(const T & val)
    {
        clear();
        new (m_data) T(val);
        m_present = true;
    }

    void set(T && val) {
        clear();
        raw() = std::move(val);
        m_present = true;
    }

    T & value()
    {
        assert(m_present);
        return raw();
    }

    const T & value() const
    {
        assert(m_present);
        return raw();
    }

    T && release()
    {
        assert(m_present);
        m_present = false;
        return std::move(raw());
    }

    void clear()
    {
        if (m_present) {
            raw().~T();
        }
        m_present = false;
    }

  private:
    // Access the storage as the correct type without an assertion.
    T & raw() {
        return reinterpret_cast<T &>(m_data);
    }
    const T & raw() const {
        return reinterpret_cast<const T &>(m_data);
    }
};

#endif  // ! PZ_CXX_FUTURE_H
