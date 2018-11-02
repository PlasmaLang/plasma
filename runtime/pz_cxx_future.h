/*
 * PZ C++ future library functions.
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2018 Plasma Team
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

template<typename T>
class Deleter {
  public:
    static void delete_if_nonnull(T data) { }
};

template<typename T>
class Deleter<T*> {
  public:
    static void delete_if_nonnull(T* data)
    {
        if (nullptr != data) {
            delete data;
        }
    }
};

/*
 * C++17 libraries don't seem to be on my dev system,
 * other people might also be missing them.  So just implement this
 * ourselves.
 */
template<typename T>
class Optional {
  private:
    T value_;
    bool present;

  public:
    constexpr Optional() : present(false) {}
    explicit constexpr Optional(const T &val) : value_(val), present(true) {}

    static constexpr Optional Nothing() { return Optional(); }

    bool hasValue() const { return present; }

    const void set(const T &val)
    {
        value_ = val;
        present = true;
    }

    const T & value() const { assert(present); return value_; }
};

/*
 * We won't need this with C++20
 */
bool startsWith(const std::string &string, const char *beginning);

#endif // ! PZ_CXX_FUTURE_H

