/*
 * Plasma bytecode execution configuration.
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2015 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#ifndef PZ_CONFIG_H
#define PZ_CONFIG_H

/*
 * Either 32 or 64 bit
 */
#define PZ_FAST_INTEGER_WIDTH 32
#define PZ_FAST_INTEGER_TYPE int32_t
#define PZ_FAST_UINTEGER_TYPE uint32_t

/*
 * Debugging
 */
#ifdef DEBUG
#else
#define NDEBUG
#endif

#endif /* ! PZ_CONFIG_H */
