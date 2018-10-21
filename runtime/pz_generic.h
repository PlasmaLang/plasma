/*
 * Plasma bytecode generic interpreter definitions 
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2015, 2018 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#ifndef PZ_GENERIC_H
#define PZ_GENERIC_H

typedef union {
    uint8_t   u8;
    int8_t    s8;
    uint16_t  u16;
    int16_t   s16;
    uint32_t  u32;
    int32_t   s32;
    uint64_t  u64;
    int64_t   s64;
    uintptr_t uptr;
    intptr_t  sptr;
    void *    ptr;
} Stack_Value;

typedef unsigned (*ccall_func)(Stack_Value *, unsigned, PZ_Heap *);

#endif // ! PZ_GENERIC_H
