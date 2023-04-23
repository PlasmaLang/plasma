/*
 * PZ Utils.
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#ifndef PZ_UTIL_H
#define PZ_UTIL_H

/*
 * The machine word size.
 */
#define WORDSIZE_BYTES sizeof(void *)

#if UINTPTR_MAX == 0xFFFFFFFFFFFFFFFF
#define WORDSIZE_BITS 64
#elif UINTPTR_MAX == 0xFFFFFFFF
#define WORDSIZE_BYTES 32
#else
#pragma error "Unknown pointer size"
#endif

#if WORDSIZE_BITS == 64
#define WORDSIZE_HEX_CHARS_STR "16"
#elif WORDSIZE_BITS == 32
#define WORDSIZE_HEX_CHARS_STR "8"
#endif

template <typename T>
constexpr T RoundUp(T x, T y)
{
    return ((x + y - 1) / y) * y;
}

template <typename T>
constexpr T RoundDown(T x, T y)
{
    return (x / y) * y;
}

constexpr size_t AlignUp(size_t x, size_t y)
{
    return RoundUp<size_t>(x, y);
}

#endif /* ! PZ_UTIL_H */
