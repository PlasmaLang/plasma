/*
 * Plasma bytecode instructions
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2015 Paul Bone
 * Distributed under the terms of the MIT license, see ../LICENSE.runtime
 */

#ifndef PZ_INSTRUCTIONS_H
#define PZ_INSTRUCTIONS_H

typedef enum {
    /*
     * These instructions may appear in bytecode.
     */
    PZI_LOAD_IMMEDIATE_8 = 0,
    PZI_LOAD_IMMEDIATE_16,
    PZI_LOAD_IMMEDIATE_32,
    PZI_LOAD_IMMEDIATE_64,
    PZI_LOAD_IMMEDIATE_DATA,
    PZI_ZE_8_16,
    PZI_ZE_16_32,
    PZI_ZE_32_64,
    PZI_SE_8_16,
    PZI_SE_16_32,
    PZI_SE_32_64,
    PZI_TRUNC_64_32,
    PZI_TRUNC_32_16,
    PZI_TRUNC_16_8,
    PZI_ZE_32_FAST,
    PZI_SE_32_FAST,
    PZI_TRUNC_FAST_32,
    PZI_ADD,
    PZI_SUB,
    PZI_MUL,
    PZI_DIV,
    PZI_DUP,
    PZI_SWAP,
    PZI_CALL,

    /*
     * These instructions do not appear in bytecode, they may be used by the
     * interpreter.
     */
    PZI_RETURN,
    PZI_END,
    PZI_CCALL
} opcode;

enum immediate_type {
    IMT_NONE,
    IMT_8,
    IMT_16,
    IMT_32,
    IMT_64,
    IMT_CODE_REF,
    IMT_DATA_REF
};

/*
 * Get the immediate type following the instruction opcode.
 */
enum immediate_type
pz_immediate(opcode opcode);

#endif /* ! PZ_INSTRUCTIONS_H */

