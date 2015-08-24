/*
 * Plasma bytecode exection
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2015 Paul Bone
 * Distributed under the terms of the MIT license, see ../LICENSE.runtime
 */

#ifndef PZ_RUN_H
#define PZ_RUN_H

#include "pz.h"

typedef union stack_value {
    uint8_t     u8;
    uint16_t    u16;
    uint32_t    u32;
    uint64_t    u64;
    uintptr_t   uptr;
} stack_value;

typedef unsigned (*ccall_func)(stack_value*, unsigned);

unsigned builtin_print(stack_value* stack, unsigned sp);

/*
 * PZ instruction values, as they're stored in memory.
 */
extern uint8_t pz_instruction_words[];

/*
 * Run the program.
 */
int pz_run(pz*);

#endif /* ! PZ_RUN_H */
