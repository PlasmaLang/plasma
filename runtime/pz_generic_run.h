/*
 * Plasma bytecode generic interpreter definitions 
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2015, 2018-2019 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#ifndef PZ_GENERIC_RUN_H
#define PZ_GENERIC_RUN_H

#include "pz.h"
#include "pz_closure.h"
#include "pz_gc.h"
#include "pz_generic_closure.h"

namespace pz {

/*
 * Tokens for the token-oriented execution.
 */
enum InstructionToken {
    PZT_NOP,
    PZT_LOAD_IMMEDIATE_8,
    PZT_LOAD_IMMEDIATE_16,
    PZT_LOAD_IMMEDIATE_32,
    PZT_LOAD_IMMEDIATE_64,
    PZT_ZE_8_16,
    PZT_ZE_8_32,
    PZT_ZE_8_64,
    PZT_ZE_16_32,
    PZT_ZE_16_64,
    PZT_ZE_32_64,
    PZT_SE_8_16,
    PZT_SE_8_32,
    PZT_SE_8_64,
    PZT_SE_16_32,
    PZT_SE_16_64,
    PZT_SE_32_64,
    PZT_TRUNC_64_32,
    PZT_TRUNC_64_16,
    PZT_TRUNC_64_8,
    PZT_TRUNC_32_16,
    PZT_TRUNC_32_8,
    PZT_TRUNC_16_8,
    PZT_ADD_8,
    PZT_ADD_16,
    PZT_ADD_32,
    PZT_ADD_64,
    PZT_SUB_8,
    PZT_SUB_16,
    PZT_SUB_32,
    PZT_SUB_64,
    PZT_MUL_8,
    PZT_MUL_16,
    PZT_MUL_32,
    PZT_MUL_64,
    PZT_DIV_8,
    PZT_DIV_16,
    PZT_DIV_32,
    PZT_DIV_64,
    PZT_MOD_8,
    PZT_MOD_16,
    PZT_MOD_32,
    PZT_MOD_64,
    PZT_LSHIFT_8,
    PZT_LSHIFT_16,
    PZT_LSHIFT_32,
    PZT_LSHIFT_64,
    PZT_RSHIFT_8,
    PZT_RSHIFT_16,
    PZT_RSHIFT_32,
    PZT_RSHIFT_64,
    PZT_AND_8,
    PZT_AND_16,
    PZT_AND_32,
    PZT_AND_64,
    PZT_OR_8,
    PZT_OR_16,
    PZT_OR_32,
    PZT_OR_64,
    PZT_XOR_8,
    PZT_XOR_16,
    PZT_XOR_32,
    PZT_XOR_64,
    PZT_LT_U_8,
    PZT_LT_U_16,
    PZT_LT_U_32,
    PZT_LT_U_64,
    PZT_LT_S_8,
    PZT_LT_S_16,
    PZT_LT_S_32,
    PZT_LT_S_64,
    PZT_GT_U_8,
    PZT_GT_U_16,
    PZT_GT_U_32,
    PZT_GT_U_64,
    PZT_GT_S_8,
    PZT_GT_S_16,
    PZT_GT_S_32,
    PZT_GT_S_64,
    PZT_EQ_8,
    PZT_EQ_16,
    PZT_EQ_32,
    PZT_EQ_64,
    PZT_NOT_8,
    PZT_NOT_16,
    PZT_NOT_32,
    PZT_NOT_64,
    PZT_DUP,
    PZT_DROP,
    PZT_SWAP,
    PZT_ROLL,
    PZT_PICK,
    PZT_CALL,
    PZT_TCALL,
    PZT_CALL_CLOSURE,
    PZT_CALL_IND,
    PZT_CJMP_8,
    PZT_CJMP_16,
    PZT_CJMP_32,
    PZT_CJMP_64,
    PZT_JMP,
    PZT_RET,
    PZT_ALLOC,
    PZT_MAKE_CLOSURE,
    PZT_LOAD_8,
    PZT_LOAD_16,
    PZT_LOAD_32,
    PZT_LOAD_64,
    PZT_LOAD_PTR,
    PZT_STORE_8,
    PZT_STORE_16,
    PZT_STORE_32,
    PZT_STORE_64,
    PZT_GET_ENV,
    PZT_END,                // Not part of PZ format.
    PZT_CCALL,              // Not part of PZ format.
    PZT_CCALL_ALLOC,        // Not part of PZ format.
    PZT_CCALL_SPECIAL,      // Not part of PZ format.
    PZT_LAST_TOKEN = PZT_CCALL_ALLOC,
#ifdef PZ_DEV
    PZT_INVALID_TOKEN = 0x77,
#endif
};

union StackValue {
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
};

struct Stacks {
    uint8_t          **return_stack;
    unsigned           rsp;
    StackValue        *expr_stack;
    unsigned           esp;
};

int
generic_main_loop(Stacks    *stacks,
                  Heap      &heap,
                  Closure   *closure,
                  PZ        &pz);

} // namespace pz

#endif // ! PZ_GENERIC_RUN_H
