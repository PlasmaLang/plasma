/*
 * PZ Utils.
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2015, 2018 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#ifndef PZ_UTIL_H
#define PZ_UTIL_H

/*
 * The machine word size.
 */
#define MACHINE_WORD_SIZE sizeof(uintptr_t)

#if __WORDSIZE == 64
#define WORDSIZE_HEX_CHARS_STR "16"
#elif __WORDSIZE == 32
#define WORDSIZE_HEX_CHARS_STR "8"
#endif

#define ALIGN_UP(X, Y) (((X) + ((Y)-1)) & ~((Y)-1))

#ifdef __cplusplus

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
        return value_;
    }

    const T & value() const { assert(present); return value_; }
};

#endif

#endif /* ! PZ_UTIL_H */
