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
#endif

#endif /* ! PZ_UTIL_H */
