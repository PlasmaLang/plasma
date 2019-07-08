/*
 * PZ Utils.
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2015, 2018-2019 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#ifndef PZ_UTIL_H
#define PZ_UTIL_H

/*
 * The machine word size.
 */
#define WORDSIZE_BYTES sizeof(void*)
#define WORDSIZE_BITS __WORDSIZE

#if WORDSIZE_BITS == 64
#define WORDSIZE_HEX_CHARS_STR "16"
#elif WORDSIZE_BITS == 32
#define WORDSIZE_HEX_CHARS_STR "8"
#endif

constexpr size_t AlignUp(size_t x, size_t y)
{
    return (x + y - 1) & ~(y - 1);
}

#endif /* ! PZ_UTIL_H */
