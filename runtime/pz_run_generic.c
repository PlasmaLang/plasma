/*
 * Plasma bytecode exection (generic portable version)
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2015-2017 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#include "pz_common.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>

#include "pz_code.h"
#include "pz_instructions.h"
#include "pz_run.h"
#include "pz_trace.h"
#include "pz_util.h"

#define RETURN_STACK_SIZE 1024
#define EXPR_STACK_SIZE 1024

typedef union {
    uint8_t     u8;
    int8_t      s8;
    uint16_t    u16;
    int16_t     s16;
    uint32_t    u32;
    int32_t     s32;
    uint64_t    u64;
    int64_t     s64;
    uintptr_t   uptr;
    intptr_t    sptr;
    void        *ptr;
} Stack_Value;

/*
 * Tokens for the token-oriented execution.
 */
typedef enum {
    PZT_NOP,
    PZT_LOAD_IMMEDIATE_8,
    PZT_LOAD_IMMEDIATE_16,
    PZT_LOAD_IMMEDIATE_32,
    PZT_LOAD_IMMEDIATE_64,
    PZT_LOAD_IMMEDIATE_DATA,
    PZT_LOAD_IMMEDIATE_CODE,
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
    PZT_CALL_IND,
    PZT_CJMP_8,
    PZT_CJMP_16,
    PZT_CJMP_32,
    PZT_CJMP_64,
    PZT_JMP,
    PZT_RET,
    PZT_ALLOC,
    PZT_LOAD_8,
    PZT_LOAD_16,
    PZT_LOAD_32,
    PZT_LOAD_64,
    PZT_STORE_8,
    PZT_STORE_16,
    PZT_STORE_32,
    PZT_STORE_64,
    PZT_END,
    PZT_CCALL,
    PZT_LAST_TOKEN = PZT_CCALL,
} PZ_Instruction_Token;

/*
 * Instruction and intermedate data sizes, and procedures to write them.
 */

static unsigned
pz_immediate_size(Immediate_Type imt);

/*
 * Imported procedures
 *
 **********************/

typedef unsigned (*ccall_func)(Stack_Value*, unsigned);

unsigned
builtin_print_func(void *void_stack, unsigned sp)
{
    Stack_Value *stack = void_stack;

    char *string = (char*)(stack[sp--].uptr);
    printf("%s", string);
    return sp;
}

/*
 * Long enough for a 32 bit value, plus a sign, plus a null termination
 * byte.
 */
#define INT_TO_STRING_BUFFER_SIZE 11

unsigned
builtin_int_to_string_func(void *void_stack, unsigned sp)
{
    char        *string;
    int32_t     num;
    int         result;
    Stack_Value *stack = void_stack;

    num = stack[sp].s32;
    string = malloc(INT_TO_STRING_BUFFER_SIZE);
    result = snprintf(string, INT_TO_STRING_BUFFER_SIZE, "%d", (int)num);
    if ((result < 0) || (result > (INT_TO_STRING_BUFFER_SIZE-1))) {
        free(string);
        stack[sp].ptr = NULL;
    } else {
        stack[sp].ptr = string;
    }
    return sp;
}

unsigned
builtin_free_func(void *void_stack, unsigned sp)
{
    Stack_Value *stack = void_stack;

    free(stack[sp--].ptr);
    return sp;
}

unsigned
builtin_setenv_func(void *void_stack, unsigned sp)
{
    Stack_Value *stack = void_stack;
    int result;
    const char *value = stack[sp--].ptr;
    const char *name = stack[sp--].ptr;

    result = setenv(name, value, 1);

    stack[++sp].u32 = !result;

    return sp;
}

unsigned
builtin_gettimeofday_func(void *void_stack, unsigned sp)
{
    Stack_Value *stack = void_stack;
    struct timeval tv;
    int res;

    res = gettimeofday(&tv, NULL);

    stack[++sp].u32 = res == 0 ? 1 : 0;
    // This is aweful, but Plasma itself doesn't handle other inttypes yet.
    stack[++sp].u32 = (uint32_t)tv.tv_sec;
    stack[++sp].u32 = (uint32_t)tv.tv_usec;

    return sp;
}

unsigned
builtin_concat_string_func(void *void_stack, unsigned sp)
{
    const char  *s1, *s2;
    char        *s;
    size_t      len;
    Stack_Value *stack = void_stack;

    s2 = stack[sp--].ptr;
    s1 = stack[sp].ptr;

    len = strlen(s1) + strlen(s2) + 1;
    s = malloc(sizeof(char) * len);
    strcpy(s, s1);
    strcat(s, s2);

    stack[sp].ptr = s;
    return sp;
}

unsigned
builtin_die_func(void *void_stack, unsigned sp)
{
    const char  *s;
    Stack_Value *stack = void_stack;

    s = stack[sp].ptr;
    fprintf(stderr, "Die: %s\n", s);
    exit(1);
}

const unsigned pz_fast_word_size = PZ_FAST_INTEGER_WIDTH / 8;

/* Must match or exceed ptag_bits from src/core.types.m */
const unsigned pz_num_tag_bits = 2;
const uintptr_t pz_tag_bits = 0x3;

/*
 * Run the program
 *
 ******************/

int
pz_run(PZ *pz) {
    uint8_t         **return_stack;
    unsigned        rsp = 0;
    Stack_Value     *expr_stack;
    unsigned        esp = 0;
    uint8_t         *ip;
    uint8_t         *wrapper_proc;
    unsigned        wrapper_proc_size;
    int             retcode;
    Immediate_Value imv_none;
    PZ_Module       *entry_module;
    int32_t         entry_proc;

    assert(PZT_LAST_TOKEN < 256);

    return_stack = malloc(sizeof(uint8_t*) * RETURN_STACK_SIZE);
    expr_stack = malloc(sizeof(Stack_Value) * EXPR_STACK_SIZE);
    expr_stack[0].u64 = 0;

    /*
     * Assemble a special procedure that exits the interpreter and put its
     * address on the call stack.
     */
    memset(&imv_none, 0, sizeof(imv_none));
    wrapper_proc_size = pz_write_instr(NULL, 0, PZI_END, 0, 0, IMT_NONE,
        imv_none);
    wrapper_proc = malloc(wrapper_proc_size);
    pz_write_instr(wrapper_proc, 0, PZI_END, 0, 0, IMT_NONE, imv_none);
    return_stack[0] = wrapper_proc;

    // Determine the entry procedure.
    entry_module = pz_get_entry_module(pz);
    entry_proc = -1;
    if (NULL != entry_module) {
        entry_proc = pz_module_get_entry_proc(entry_module);
    }
    if (entry_proc < 0) {
        fprintf(stderr, "No entry procedure\n");
        abort();
    }

    // Set the instruction pointer and start execution.
    ip = pz_module_get_proc_code(entry_module, entry_proc);
    retcode = 255;
    pz_trace_state(ip, rsp, esp, (uint64_t*)expr_stack);
    while (true) {
        PZ_Instruction_Token token = (PZ_Instruction_Token)(*ip);

        ip++;
        switch (token) {
            case PZT_NOP:
                break;
            case PZT_LOAD_IMMEDIATE_8:
                expr_stack[++esp].u8 = *ip;
                ip++;
                pz_trace_instr(rsp, "load imm:8");
                break;
            case PZT_LOAD_IMMEDIATE_16:
                ip = (uint8_t*)ALIGN_UP((uintptr_t)ip, 2);
                expr_stack[++esp].u16 = *(uint16_t*)ip;
                ip += 2;
                pz_trace_instr(rsp, "load imm:16");
                break;
            case PZT_LOAD_IMMEDIATE_32:
                ip = (uint8_t*)ALIGN_UP((uintptr_t)ip, 4);
                expr_stack[++esp].u32 = *(uint32_t*)ip;
                ip += 4;
                pz_trace_instr(rsp, "load imm:32");
                break;
            case PZT_LOAD_IMMEDIATE_64:
                ip = (uint8_t*)ALIGN_UP((uintptr_t)ip, 8);
                expr_stack[++esp].u64 = *(uint64_t*)ip;
                ip += 8;
                pz_trace_instr(rsp, "load imm:64");
                break;
            case PZT_LOAD_IMMEDIATE_DATA:
                ip = (uint8_t*)ALIGN_UP((uintptr_t)ip, MACHINE_WORD_SIZE);
                expr_stack[++esp].uptr = *(uintptr_t*)ip;
                ip += MACHINE_WORD_SIZE;
                pz_trace_instr(rsp, "load imm data:ptr");
                break;
            case PZT_LOAD_IMMEDIATE_CODE:
                /*
                 * Consider merging this instruction with the previous one
                 * as an optimisation.
                 */
                ip = (uint8_t*)ALIGN_UP((uintptr_t)ip, MACHINE_WORD_SIZE);
                expr_stack[++esp].uptr = *(uintptr_t*)ip;
                ip += MACHINE_WORD_SIZE;
                pz_trace_instr(rsp, "Load imm code");
                break;
            case PZT_ZE_8_16:
                expr_stack[esp].u16 = expr_stack[esp].u8;
                pz_trace_instr(rsp, "ze:8:16");
                break;
            case PZT_ZE_8_32:
                expr_stack[esp].u32 = expr_stack[esp].u8;
                pz_trace_instr(rsp, "ze:8:32");
                break;
            case PZT_ZE_8_64:
                expr_stack[esp].u64 = expr_stack[esp].u8;
                pz_trace_instr(rsp, "ze:8:64");
                break;
            case PZT_ZE_16_32:
                expr_stack[esp].u32 = expr_stack[esp].u16;
                pz_trace_instr(rsp, "ze:16:32");
                break;
            case PZT_ZE_16_64:
                expr_stack[esp].u64 = expr_stack[esp].u16;
                pz_trace_instr(rsp, "ze:16:64");
                break;
            case PZT_ZE_32_64:
                expr_stack[esp].u64 = expr_stack[esp].u32;
                pz_trace_instr(rsp, "ze:32:64");
                break;
            case PZT_SE_8_16:
                expr_stack[esp].s16 = expr_stack[esp].s8;
                pz_trace_instr(rsp, "se:8:16");
                break;
            case PZT_SE_8_32:
                expr_stack[esp].s32 = expr_stack[esp].s8;
                pz_trace_instr(rsp, "se:8:32");
                break;
            case PZT_SE_8_64:
                expr_stack[esp].s64 = expr_stack[esp].s8;
                pz_trace_instr(rsp, "se:8:64");
                break;
            case PZT_SE_16_32:
                expr_stack[esp].s32 = expr_stack[esp].s16;
                pz_trace_instr(rsp, "se:16:32");
                break;
            case PZT_SE_16_64:
                expr_stack[esp].s64 = expr_stack[esp].s16;
                pz_trace_instr(rsp, "se:16:64");
                break;
            case PZT_SE_32_64:
                expr_stack[esp].s64 = expr_stack[esp].s32;
                pz_trace_instr(rsp, "se:32:64");
                break;
            case PZT_TRUNC_64_32:
                expr_stack[esp].u32 = expr_stack[esp].u64 & 0xFFFFFFFFu;
                pz_trace_instr(rsp, "trunc:64:32");
                break;
            case PZT_TRUNC_64_16:
                expr_stack[esp].u16 = expr_stack[esp].u64 & 0xFFFF;
                pz_trace_instr(rsp, "trunc:64:16");
                break;
            case PZT_TRUNC_64_8:
                expr_stack[esp].u8 = expr_stack[esp].u64 & 0xFF;
                pz_trace_instr(rsp, "trunc:64:8");
                break;
            case PZT_TRUNC_32_16:
                expr_stack[esp].u16 = expr_stack[esp].u32 & 0xFFFF;
                pz_trace_instr(rsp, "trunc:32:16");
                break;
            case PZT_TRUNC_32_8:
                expr_stack[esp].u8 = expr_stack[esp].u32 & 0xFF;
                pz_trace_instr(rsp, "trunc:32:8");
                break;
            case PZT_TRUNC_16_8:
                expr_stack[esp].u8 = expr_stack[esp].u16 & 0xFF;
                pz_trace_instr(rsp, "trunc:16:8");
                break;

#define PZ_RUN_ARITHMETIC(opcode_base, width, signedness, operator, op_name) \
            case opcode_base ## _ ## width: \
                expr_stack[esp-1].signedness ## width = \
                    (expr_stack[esp-1].signedness ## width \
                        operator expr_stack[esp].signedness ## width); \
                esp--; \
                pz_trace_instr(rsp, op_name); \
                break
#define PZ_RUN_ARITHMETIC1(opcode_base, width, signedness, operator, op_name) \
            case opcode_base ## _ ## width: \
                expr_stack[esp].signedness ## width = \
                        operator expr_stack[esp].signedness ## width; \
                pz_trace_instr(rsp, op_name); \
                break

            PZ_RUN_ARITHMETIC(PZT_ADD, 8, s, +,     "add:8");
            PZ_RUN_ARITHMETIC(PZT_ADD, 16, s, +,    "add:16");
            PZ_RUN_ARITHMETIC(PZT_ADD, 32, s, +,    "add:32");
            PZ_RUN_ARITHMETIC(PZT_ADD, 64, s, +,    "add:64");
            PZ_RUN_ARITHMETIC(PZT_SUB, 8, s, -,     "sub:8");
            PZ_RUN_ARITHMETIC(PZT_SUB, 16, s, -,    "sub:16");
            PZ_RUN_ARITHMETIC(PZT_SUB, 32, s, -,    "sub:32");
            PZ_RUN_ARITHMETIC(PZT_SUB, 64, s, -,    "sub:64");
            PZ_RUN_ARITHMETIC(PZT_MUL, 8, s, *,     "mul:8");
            PZ_RUN_ARITHMETIC(PZT_MUL, 16, s, *,    "mul:16");
            PZ_RUN_ARITHMETIC(PZT_MUL, 32, s, *,    "mul:32");
            PZ_RUN_ARITHMETIC(PZT_MUL, 64, s, *,    "mul:64");
            PZ_RUN_ARITHMETIC(PZT_DIV, 8, s, /,     "div:8");
            PZ_RUN_ARITHMETIC(PZT_DIV, 16, s, /,    "div:16");
            PZ_RUN_ARITHMETIC(PZT_DIV, 32, s, /,    "div:32");
            PZ_RUN_ARITHMETIC(PZT_DIV, 64, s, /,    "div:64");
            PZ_RUN_ARITHMETIC(PZT_MOD, 8, s, %,     "rem:8");
            PZ_RUN_ARITHMETIC(PZT_MOD, 16, s, %,    "rem:16");
            PZ_RUN_ARITHMETIC(PZT_MOD, 32, s, %,    "rem:32");
            PZ_RUN_ARITHMETIC(PZT_MOD, 64, s, %,    "rem:64");
            PZ_RUN_ARITHMETIC(PZT_AND, 8, u, &,     "and:8");
            PZ_RUN_ARITHMETIC(PZT_AND, 16, u, &,    "and:16");
            PZ_RUN_ARITHMETIC(PZT_AND, 32, u, &,    "and:32");
            PZ_RUN_ARITHMETIC(PZT_AND, 64, u, &,    "and:64");
            PZ_RUN_ARITHMETIC(PZT_OR, 8, u, |,      "or:8");
            PZ_RUN_ARITHMETIC(PZT_OR, 16, u, |,     "or:16");
            PZ_RUN_ARITHMETIC(PZT_OR, 32, u, |,     "or:32");
            PZ_RUN_ARITHMETIC(PZT_OR, 64, u, |,     "or:64");
            PZ_RUN_ARITHMETIC(PZT_XOR, 8, u, ^,     "xor:8");
            PZ_RUN_ARITHMETIC(PZT_XOR, 16, u, ^,    "xor:16");
            PZ_RUN_ARITHMETIC(PZT_XOR, 32, u, ^,    "xor:32");
            PZ_RUN_ARITHMETIC(PZT_XOR, 64, u, ^,    "xor:64");
            PZ_RUN_ARITHMETIC(PZT_LT_U, 8, u, <,    "ltu:8");
            PZ_RUN_ARITHMETIC(PZT_LT_U, 16, u, <,   "ltu:16");
            PZ_RUN_ARITHMETIC(PZT_LT_U, 32, u, <,   "ltu:32");
            PZ_RUN_ARITHMETIC(PZT_LT_U, 64, u, <,   "ltu:64");
            PZ_RUN_ARITHMETIC(PZT_LT_S, 8, s, <,    "lts:8");
            PZ_RUN_ARITHMETIC(PZT_LT_S, 16, s, <,   "lts:16");
            PZ_RUN_ARITHMETIC(PZT_LT_S, 32, s, <,   "lts:32");
            PZ_RUN_ARITHMETIC(PZT_LT_S, 64, s, <,   "lts:64");
            PZ_RUN_ARITHMETIC(PZT_GT_U, 8, u, >,    "gtu:8");
            PZ_RUN_ARITHMETIC(PZT_GT_U, 16, u, >,   "gtu:16");
            PZ_RUN_ARITHMETIC(PZT_GT_U, 32, u, >,   "gtu:32");
            PZ_RUN_ARITHMETIC(PZT_GT_U, 64, u, >,   "gtu:64");
            PZ_RUN_ARITHMETIC(PZT_GT_S, 8, s, >,    "gts:8");
            PZ_RUN_ARITHMETIC(PZT_GT_S, 16, s, >,   "gts:16");
            PZ_RUN_ARITHMETIC(PZT_GT_S, 32, s, >,   "gts:32");
            PZ_RUN_ARITHMETIC(PZT_GT_S, 64, s, >,   "gts:64");
            PZ_RUN_ARITHMETIC(PZT_EQ, 8, s, ==,     "eq:8");
            PZ_RUN_ARITHMETIC(PZT_EQ, 16, s, ==,    "eq:16");
            PZ_RUN_ARITHMETIC(PZT_EQ, 32, s, ==,    "eq:32");
            PZ_RUN_ARITHMETIC(PZT_EQ, 64, s, ==,    "eq:64");
            PZ_RUN_ARITHMETIC1(PZT_NOT, 8, u, !,    "not:8");
            PZ_RUN_ARITHMETIC1(PZT_NOT, 16, u, !,   "not:16");
            PZ_RUN_ARITHMETIC1(PZT_NOT, 32, u, !,   "not:16");
            PZ_RUN_ARITHMETIC1(PZT_NOT, 64, u, !,   "not:16");

#undef PZ_RUN_ARITHMETIC
#undef PZ_RUN_ARITHMETIC1

#define PZ_RUN_SHIFT(opcode_base, width, operator, op_name) \
            case opcode_base ## _ ## width: \
                expr_stack[esp-1].u ## width = \
                    (expr_stack[esp-1].u ## width \
                        operator expr_stack[esp].u8); \
                esp--; \
                pz_trace_instr(rsp, op_name); \
                break

            PZ_RUN_SHIFT(PZT_LSHIFT, 8, <<,     "lshift:8");
            PZ_RUN_SHIFT(PZT_LSHIFT, 16, <<,    "lshift:16");
            PZ_RUN_SHIFT(PZT_LSHIFT, 32, <<,    "lshift:32");
            PZ_RUN_SHIFT(PZT_LSHIFT, 64, <<,    "lshift:64");
            PZ_RUN_SHIFT(PZT_RSHIFT, 8, >>,     "rshift:8");
            PZ_RUN_SHIFT(PZT_RSHIFT, 16, >>,    "rshift:16");
            PZ_RUN_SHIFT(PZT_RSHIFT, 32, >>,    "rshift:32");
            PZ_RUN_SHIFT(PZT_RSHIFT, 64, >>,    "rshift:64");

#undef PZ_RUN_SHIFT

            case PZT_DUP:
                esp++;
                expr_stack[esp] = expr_stack[esp-1];
                pz_trace_instr(rsp, "dup");
                break;
            case PZT_DROP:
                esp--;
                pz_trace_instr(rsp, "drop");
                break;
            case PZT_SWAP: {
                Stack_Value temp;
                temp = expr_stack[esp];
                expr_stack[esp] = expr_stack[esp-1];
                expr_stack[esp-1] = temp;
                pz_trace_instr(rsp, "swap");
                break;
            }
            case PZT_ROLL: {
                uint8_t depth = *ip;
                Stack_Value temp;
                ip++;
                switch (depth) {
                    case 0:
                        fprintf(stderr, "Illegal rot depth 0");
                        abort();
                    case 1:
                        break;
                    default:
                        /*
                         * subtract 1 as the 1st element on the stack is
                         * esp - 0, not esp - 1
                         */
                        depth--;
                        temp = expr_stack[esp - depth];
                        for (int i = depth; i > 0; i--) {
                            expr_stack[esp - i] = expr_stack[esp - (i - 1)];
                        }
                        expr_stack[esp] = temp;
                }
                pz_trace_instr2(rsp, "roll", depth+1);
                break;
            }
            case PZT_PICK: {
                /*
                 * As with PZT_ROLL we would subract 1 here, but we also
                 * have to add 1 because we increment the stack pointer
                 * before accessing the stack.
                 */
                uint8_t depth = *ip;
                ip++;
                esp++;
                expr_stack[esp] = expr_stack[esp - depth];
                pz_trace_instr2(rsp, "pick", depth);
                break;
            }
            case PZT_CALL:
                ip = (uint8_t*)ALIGN_UP((uintptr_t)ip, MACHINE_WORD_SIZE);
                return_stack[++rsp] = (ip + MACHINE_WORD_SIZE);
                ip = *(uint8_t**)ip;
                pz_trace_instr(rsp, "call");
                break;
            case PZT_CALL_IND:
                return_stack[++rsp] = ip;
                ip = (uint8_t*)expr_stack[esp--].ptr;
                pz_trace_instr(rsp, "call_ind");
                break;
            case PZT_CJMP_8:
                ip = (uint8_t*)ALIGN_UP((uintptr_t)ip, MACHINE_WORD_SIZE);
                if (expr_stack[esp--].u8) {
                    ip = *(uint8_t**)ip;
                    pz_trace_instr(rsp, "cjmp:8 taken");
                } else {
                    ip += MACHINE_WORD_SIZE;
                    pz_trace_instr(rsp, "cjmp:8 not taken");
                }
                break;
            case PZT_CJMP_16:
                ip = (uint8_t*)ALIGN_UP((uintptr_t)ip, MACHINE_WORD_SIZE);
                if (expr_stack[esp--].u16) {
                    ip = *(uint8_t**)ip;
                    pz_trace_instr(rsp, "cjmp:16 taken");
                } else {
                    ip += MACHINE_WORD_SIZE;
                    pz_trace_instr(rsp, "cjmp:16 not taken");
                }
                break;
            case PZT_CJMP_32:
                ip = (uint8_t*)ALIGN_UP((uintptr_t)ip, MACHINE_WORD_SIZE);
                if (expr_stack[esp--].u32) {
                    ip = *(uint8_t**)ip;
                    pz_trace_instr(rsp, "cjmp:32 taken");
                } else {
                    ip += MACHINE_WORD_SIZE;
                    pz_trace_instr(rsp, "cjmp:32 not taken");
                }
                break;
            case PZT_CJMP_64:
                ip = (uint8_t*)ALIGN_UP((uintptr_t)ip, MACHINE_WORD_SIZE);
                if (expr_stack[esp--].u64) {
                    ip = *(uint8_t**)ip;
                    pz_trace_instr(rsp, "cjmp:64 taken");
                } else {
                    ip += MACHINE_WORD_SIZE;
                    pz_trace_instr(rsp, "cjmp:64 not taken");
                }
                break;
            case PZT_JMP:
                ip = (uint8_t*)ALIGN_UP((uintptr_t)ip, MACHINE_WORD_SIZE);
                ip = *(uint8_t**)ip;
                pz_trace_instr(rsp, "jmp");
                break;
            case PZT_RET:
                ip = return_stack[rsp--];
                pz_trace_instr(rsp, "ret");
                break;
            case PZT_ALLOC: {
                uintptr_t size;
                void *addr;
                ip = (uint8_t*)ALIGN_UP((uintptr_t)ip, MACHINE_WORD_SIZE);
                size = *(uintptr_t*)ip;
                ip += MACHINE_WORD_SIZE;
                addr = malloc(size);
                expr_stack[++esp].ptr = addr;
                pz_trace_instr(rsp, "alloc");
            }
            break;
            case PZT_LOAD_8: {
                uint16_t offset;
                void *addr;
                ip = (uint8_t*)ALIGN_UP((uintptr_t)ip, 2);
                offset = *(uint16_t*)ip;
                ip += 2;
                /* (ptr - * ptr) */
                addr = expr_stack[esp].ptr + offset;
                expr_stack[esp+1].ptr = expr_stack[esp].ptr;
                expr_stack[esp].u8 = *(uint8_t*)addr;
                esp++;
                pz_trace_instr(rsp, "load_8");
            }
            break;
            case PZT_LOAD_16: {
                uint16_t offset;
                void *addr;
                ip = (uint8_t*)ALIGN_UP((uintptr_t)ip, 2);
                offset = *(uint16_t*)ip;
                ip += 2;
                /* (ptr - * ptr) */
                addr = expr_stack[esp].ptr + offset;
                expr_stack[esp+1].ptr = expr_stack[esp].ptr;
                expr_stack[esp].u16 = *(uint16_t*)addr;
                esp++;
                pz_trace_instr(rsp, "load_16");
            }
            break;
            case PZT_LOAD_32: {
                uint16_t offset;
                void *addr;
                ip = (uint8_t*)ALIGN_UP((uintptr_t)ip, 2);
                offset = *(uint16_t*)ip;
                ip += 2;
                /* (ptr - * ptr) */
                addr = expr_stack[esp].ptr + offset;
                expr_stack[esp+1].ptr = expr_stack[esp].ptr;
                expr_stack[esp].u32 = *(uint32_t*)addr;
                esp++;
                pz_trace_instr(rsp, "load_32");
            }
            break;
            case PZT_LOAD_64: {
                uint16_t offset;
                void *addr;
                ip = (uint8_t*)ALIGN_UP((uintptr_t)ip, 2);
                offset = *(uint16_t*)ip;
                ip += 2;
                /* (ptr - * ptr) */
                addr = expr_stack[esp].ptr + offset;
                expr_stack[esp+1].ptr = expr_stack[esp].ptr;
                expr_stack[esp].u64 = *(uint64_t*)addr;
                esp++;
                pz_trace_instr(rsp, "load_64");
            }
            break;
            case PZT_STORE_8: {
                uint16_t offset;
                void *addr;
                ip = (uint8_t*)ALIGN_UP((uintptr_t)ip, 2);
                offset = *(uint16_t*)ip;
                ip += 2;
                /* (* ptr - ptr) */
                addr = expr_stack[esp].ptr + offset;
                *(uint8_t*)addr = expr_stack[esp-1].u8;
                expr_stack[esp-1].ptr = expr_stack[esp].ptr;
                esp--;
                pz_trace_instr(rsp, "store_8");
            }
            break;
            case PZT_STORE_16: {
                uint16_t offset;
                void *addr;
                ip = (uint8_t*)ALIGN_UP((uintptr_t)ip, 2);
                offset = *(uint16_t*)ip;
                ip += 2;
                /* (* ptr - ptr) */
                addr = expr_stack[esp].ptr + offset;
                *(uint16_t*)addr = expr_stack[esp-1].u16;
                expr_stack[esp-1].ptr = expr_stack[esp].ptr;
                esp--;
                pz_trace_instr(rsp, "store_16");
            }
            break;
            case PZT_STORE_32: {
                uint16_t offset;
                void *addr;
                ip = (uint8_t*)ALIGN_UP((uintptr_t)ip, 2);
                offset = *(uint16_t*)ip;
                ip += 2;
                /* (* ptr - ptr) */
                addr = expr_stack[esp].ptr + offset;
                *(uint32_t*)addr = expr_stack[esp-1].u32;
                expr_stack[esp-1].ptr = expr_stack[esp].ptr;
                esp--;
                pz_trace_instr(rsp, "store_32");
            }
            break;
            case PZT_STORE_64: {
                uint16_t offset;
                void *addr;
                ip = (uint8_t*)ALIGN_UP((uintptr_t)ip, 2);
                offset = *(uint16_t*)ip;
                ip += 2;
                /* (* ptr - ptr) */
                addr = expr_stack[esp].ptr + offset;
                *(uint64_t*)addr = expr_stack[esp-1].u64;
                expr_stack[esp-1].ptr = expr_stack[esp].ptr;
                esp--;
                pz_trace_instr(rsp, "store_64");
            }
            break;

            case PZT_END:
                retcode = expr_stack[esp].s32;
                pz_trace_instr(rsp, "end");
                pz_trace_state(ip, rsp, esp, (uint64_t*)expr_stack);
                goto finish;
            case PZT_CCALL:
            {
                ccall_func callee;
                ip = (uint8_t*)ALIGN_UP((uintptr_t)ip, MACHINE_WORD_SIZE);
                callee = *(ccall_func*)ip;
                esp = callee(expr_stack, esp);
                ip += MACHINE_WORD_SIZE;
                pz_trace_instr(rsp, "ccall");
                break;
            }
            default:
                fprintf(stderr, "Unknown opcode\n");
                abort();
        }
        pz_trace_state(ip, rsp, esp, (uint64_t*)expr_stack);
    }

finish:
    free(wrapper_proc);
    free(return_stack);
    free(expr_stack);

    return retcode;
}

/*
 * Instruction and intermedate data sizes, and procedures to write them.
 *
 *********************/

static unsigned
pz_immediate_size(Immediate_Type imt)
{
    switch (imt) {
        case IMT_NONE:
            return 0;
        case IMT_8:
            // return ROUND_UP(1, MACHINE_WORD_SIZE)/MACHINE_WORD_SIZE;
            return 1;
        case IMT_16:
        case IMT_STRUCT_REF_FIELD:
            return 2;
        case IMT_32:
            return 4;
        case IMT_64:
            return 8;
        case IMT_DATA_REF:
        case IMT_CODE_REF:
        case IMT_STRUCT_REF:
        case IMT_LABEL_REF:
            return MACHINE_WORD_SIZE;
    }
    abort();
}

unsigned
pz_write_instr(uint8_t *proc, unsigned offset, Opcode opcode,
    Width width1, Width width2,
    Immediate_Type imm_type, Immediate_Value imm_value)
{
    PZ_Instruction_Token token;
    unsigned imm_size;

    width1 = pz_normalize_width(width1);
    width2 = pz_normalize_width(width2);

#define PZ_WRITE_INSTR_0(code, tok) \
    do { \
        if (opcode == (code)) { \
            token = (tok); \
            goto write_opcode; \
        } \
    } while (0)
#define PZ_WRITE_INSTR_1(code, w1, tok) \
    do { \
        if (opcode == (code) && width1 == (w1)) { \
            token = (tok); \
            goto write_opcode; \
        } \
    } while (0)
#define PZ_WRITE_INSTR_2(code, w1, w2, tok) \
    do { \
        if (opcode == (code) && width1 == (w1) && width2 == (w2)) { \
            token = (tok); \
            goto write_opcode; \
        } \
    } while (0)

#define SELECT_IMMEDIATE(type, value, result) \
    do { \
        switch (type) { \
            case IMT_8: \
                (result) = (value).uint8; \
                break; \
            case IMT_16: \
                (result) = (value).uint16; \
                break; \
            case IMT_32: \
                (result) = (value).uint32; \
                break; \
            case IMT_64: \
                (result) = (value).uint64; \
                break; \
            default: \
                fprintf(stderr, \
                    "Invalid immediate value for laod immediate number"); \
                abort(); \
        } \
    } while (0)

    if (opcode == PZI_LOAD_IMMEDIATE_NUM) {
        switch (width1) {
            case PZW_8:
                token = PZT_LOAD_IMMEDIATE_8;
                SELECT_IMMEDIATE(imm_type, imm_value, imm_value.uint8);
                imm_type = IMT_8;
                goto write_opcode;
            case PZW_16:
                token = PZT_LOAD_IMMEDIATE_16;
                SELECT_IMMEDIATE(imm_type, imm_value, imm_value.uint16);
                imm_type = IMT_16;
                goto write_opcode;
            case PZW_32:
                token = PZT_LOAD_IMMEDIATE_32;
                SELECT_IMMEDIATE(imm_type, imm_value, imm_value.uint32);
                imm_type = IMT_32;
                goto write_opcode;
            case PZW_64:
                token = PZT_LOAD_IMMEDIATE_64;
                SELECT_IMMEDIATE(imm_type, imm_value, imm_value.uint64);
                imm_type = IMT_64;
                goto write_opcode;
            default:
                goto error;
        }
    }

    PZ_WRITE_INSTR_0(PZI_LOAD_IMMEDIATE_DATA, PZT_LOAD_IMMEDIATE_DATA);
    PZ_WRITE_INSTR_0(PZI_LOAD_IMMEDIATE_CODE, PZT_LOAD_IMMEDIATE_CODE);

    PZ_WRITE_INSTR_2(PZI_ZE, PZW_8, PZW_8, PZT_NOP);
    PZ_WRITE_INSTR_2(PZI_ZE, PZW_8, PZW_16, PZT_ZE_8_16);
    PZ_WRITE_INSTR_2(PZI_ZE, PZW_8, PZW_32, PZT_ZE_8_32);
    PZ_WRITE_INSTR_2(PZI_ZE, PZW_8, PZW_64, PZT_ZE_8_64);
    PZ_WRITE_INSTR_2(PZI_ZE, PZW_16, PZW_16, PZT_NOP);
    PZ_WRITE_INSTR_2(PZI_ZE, PZW_16, PZW_32, PZT_ZE_16_32);
    PZ_WRITE_INSTR_2(PZI_ZE, PZW_16, PZW_64, PZT_ZE_16_64);
    PZ_WRITE_INSTR_2(PZI_ZE, PZW_32, PZW_32, PZT_NOP);
    PZ_WRITE_INSTR_2(PZI_ZE, PZW_32, PZW_64, PZT_ZE_32_64);

    PZ_WRITE_INSTR_2(PZI_SE, PZW_8, PZW_8, PZT_NOP);
    PZ_WRITE_INSTR_2(PZI_SE, PZW_8, PZW_16, PZT_SE_8_16);
    PZ_WRITE_INSTR_2(PZI_SE, PZW_8, PZW_32, PZT_SE_8_32);
    PZ_WRITE_INSTR_2(PZI_SE, PZW_8, PZW_64, PZT_SE_8_64);
    PZ_WRITE_INSTR_2(PZI_SE, PZW_16, PZW_16, PZT_NOP);
    PZ_WRITE_INSTR_2(PZI_SE, PZW_16, PZW_32, PZT_SE_16_32);
    PZ_WRITE_INSTR_2(PZI_SE, PZW_16, PZW_64, PZT_SE_16_64);
    PZ_WRITE_INSTR_2(PZI_SE, PZW_32, PZW_32, PZT_NOP);
    PZ_WRITE_INSTR_2(PZI_SE, PZW_32, PZW_64, PZT_SE_32_64);

    PZ_WRITE_INSTR_2(PZI_TRUNC, PZW_8, PZW_8, PZT_NOP);
    PZ_WRITE_INSTR_2(PZI_TRUNC, PZW_16, PZW_16, PZT_NOP);
    PZ_WRITE_INSTR_2(PZI_TRUNC, PZW_16, PZW_8, PZT_TRUNC_16_8);
    PZ_WRITE_INSTR_2(PZI_TRUNC, PZW_32, PZW_32, PZT_NOP);
    PZ_WRITE_INSTR_2(PZI_TRUNC, PZW_32, PZW_16, PZT_TRUNC_32_16);
    PZ_WRITE_INSTR_2(PZI_TRUNC, PZW_32, PZW_8, PZT_TRUNC_32_8);
    PZ_WRITE_INSTR_2(PZI_TRUNC, PZW_64, PZW_64, PZT_NOP);
    PZ_WRITE_INSTR_2(PZI_TRUNC, PZW_64, PZW_32, PZT_TRUNC_64_32);
    PZ_WRITE_INSTR_2(PZI_TRUNC, PZW_64, PZW_16, PZT_TRUNC_64_16);
    PZ_WRITE_INSTR_2(PZI_TRUNC, PZW_64, PZW_8, PZT_TRUNC_64_8);

    PZ_WRITE_INSTR_1(PZI_ADD, PZW_8, PZT_ADD_8);
    PZ_WRITE_INSTR_1(PZI_ADD, PZW_16, PZT_ADD_16);
    PZ_WRITE_INSTR_1(PZI_ADD, PZW_32, PZT_ADD_32);
    PZ_WRITE_INSTR_1(PZI_ADD, PZW_64, PZT_ADD_64);

    PZ_WRITE_INSTR_1(PZI_SUB, PZW_8, PZT_SUB_8);
    PZ_WRITE_INSTR_1(PZI_SUB, PZW_16, PZT_SUB_16);
    PZ_WRITE_INSTR_1(PZI_SUB, PZW_32, PZT_SUB_32);
    PZ_WRITE_INSTR_1(PZI_SUB, PZW_64, PZT_SUB_64);

    PZ_WRITE_INSTR_1(PZI_MUL, PZW_8, PZT_MUL_8);
    PZ_WRITE_INSTR_1(PZI_MUL, PZW_16, PZT_MUL_16);
    PZ_WRITE_INSTR_1(PZI_MUL, PZW_32, PZT_MUL_32);
    PZ_WRITE_INSTR_1(PZI_MUL, PZW_64, PZT_MUL_64);

    PZ_WRITE_INSTR_1(PZI_DIV, PZW_8, PZT_DIV_8);
    PZ_WRITE_INSTR_1(PZI_DIV, PZW_16, PZT_DIV_16);
    PZ_WRITE_INSTR_1(PZI_DIV, PZW_32, PZT_DIV_32);
    PZ_WRITE_INSTR_1(PZI_DIV, PZW_64, PZT_DIV_64);

    PZ_WRITE_INSTR_1(PZI_MOD, PZW_8, PZT_MOD_8);
    PZ_WRITE_INSTR_1(PZI_MOD, PZW_16, PZT_MOD_16);
    PZ_WRITE_INSTR_1(PZI_MOD, PZW_32, PZT_MOD_32);
    PZ_WRITE_INSTR_1(PZI_MOD, PZW_64, PZT_MOD_64);

    PZ_WRITE_INSTR_1(PZI_LSHIFT, PZW_8, PZT_LSHIFT_8);
    PZ_WRITE_INSTR_1(PZI_LSHIFT, PZW_16, PZT_LSHIFT_16);
    PZ_WRITE_INSTR_1(PZI_LSHIFT, PZW_32, PZT_LSHIFT_32);
    PZ_WRITE_INSTR_1(PZI_LSHIFT, PZW_64, PZT_LSHIFT_64);

    PZ_WRITE_INSTR_1(PZI_RSHIFT, PZW_8, PZT_RSHIFT_8);
    PZ_WRITE_INSTR_1(PZI_RSHIFT, PZW_16, PZT_RSHIFT_16);
    PZ_WRITE_INSTR_1(PZI_RSHIFT, PZW_32, PZT_RSHIFT_32);
    PZ_WRITE_INSTR_1(PZI_RSHIFT, PZW_64, PZT_RSHIFT_64);

    PZ_WRITE_INSTR_1(PZI_AND, PZW_8, PZT_AND_8);
    PZ_WRITE_INSTR_1(PZI_AND, PZW_16, PZT_AND_16);
    PZ_WRITE_INSTR_1(PZI_AND, PZW_32, PZT_AND_32);
    PZ_WRITE_INSTR_1(PZI_AND, PZW_64, PZT_AND_64);

    PZ_WRITE_INSTR_1(PZI_OR, PZW_8, PZT_OR_8);
    PZ_WRITE_INSTR_1(PZI_OR, PZW_16, PZT_OR_16);
    PZ_WRITE_INSTR_1(PZI_OR, PZW_32, PZT_OR_32);
    PZ_WRITE_INSTR_1(PZI_OR, PZW_64, PZT_OR_64);

    PZ_WRITE_INSTR_1(PZI_XOR, PZW_8, PZT_XOR_8);
    PZ_WRITE_INSTR_1(PZI_XOR, PZW_16, PZT_XOR_16);
    PZ_WRITE_INSTR_1(PZI_XOR, PZW_32, PZT_XOR_32);
    PZ_WRITE_INSTR_1(PZI_XOR, PZW_64, PZT_XOR_64);

    PZ_WRITE_INSTR_1(PZI_LT_U, PZW_8, PZT_LT_U_8);
    PZ_WRITE_INSTR_1(PZI_LT_U, PZW_16, PZT_LT_U_16);
    PZ_WRITE_INSTR_1(PZI_LT_U, PZW_32, PZT_LT_U_32);
    PZ_WRITE_INSTR_1(PZI_LT_U, PZW_64, PZT_LT_U_64);

    PZ_WRITE_INSTR_1(PZI_LT_S, PZW_8, PZT_LT_S_8);
    PZ_WRITE_INSTR_1(PZI_LT_S, PZW_16, PZT_LT_S_16);
    PZ_WRITE_INSTR_1(PZI_LT_S, PZW_32, PZT_LT_S_32);
    PZ_WRITE_INSTR_1(PZI_LT_S, PZW_64, PZT_LT_S_64);

    PZ_WRITE_INSTR_1(PZI_GT_U, PZW_8, PZT_GT_U_8);
    PZ_WRITE_INSTR_1(PZI_GT_U, PZW_16, PZT_GT_U_16);
    PZ_WRITE_INSTR_1(PZI_GT_U, PZW_32, PZT_GT_U_32);
    PZ_WRITE_INSTR_1(PZI_GT_U, PZW_64, PZT_GT_U_64);

    PZ_WRITE_INSTR_1(PZI_GT_S, PZW_8, PZT_GT_S_8);
    PZ_WRITE_INSTR_1(PZI_GT_S, PZW_16, PZT_GT_S_16);
    PZ_WRITE_INSTR_1(PZI_GT_S, PZW_32, PZT_GT_S_32);
    PZ_WRITE_INSTR_1(PZI_GT_S, PZW_64, PZT_GT_S_64);

    PZ_WRITE_INSTR_1(PZI_EQ, PZW_8, PZT_EQ_8);
    PZ_WRITE_INSTR_1(PZI_EQ, PZW_16, PZT_EQ_16);
    PZ_WRITE_INSTR_1(PZI_EQ, PZW_32, PZT_EQ_32);
    PZ_WRITE_INSTR_1(PZI_EQ, PZW_64, PZT_EQ_64);

    PZ_WRITE_INSTR_1(PZI_NOT, PZW_8, PZT_NOT_8);
    PZ_WRITE_INSTR_1(PZI_NOT, PZW_16, PZT_NOT_16);
    PZ_WRITE_INSTR_1(PZI_NOT, PZW_32, PZT_NOT_32);
    PZ_WRITE_INSTR_1(PZI_NOT, PZW_64, PZT_NOT_64);

    PZ_WRITE_INSTR_0(PZI_DROP, PZT_DROP);

    if ((opcode == PZI_ROLL) && (imm_type == IMT_8) &&
            (imm_value.uint8 == 2))
    {
        /* Optimize roll 2 into swap */
        token = PZT_SWAP;
        imm_type = IMT_NONE;
        goto write_opcode;
    }
    PZ_WRITE_INSTR_0(PZI_ROLL, PZT_ROLL);

    if ((opcode == PZI_PICK) && (imm_type == IMT_8) &&
            (imm_value.uint8 == 1))
    {
        /* Optimize pick 1 into dup */
        token = PZT_DUP;
        imm_type = IMT_NONE;
        goto write_opcode;
    }
    PZ_WRITE_INSTR_0(PZI_PICK, PZT_PICK);

    PZ_WRITE_INSTR_0(PZI_CALL, PZT_CALL);
    PZ_WRITE_INSTR_0(PZI_CALL_IND, PZT_CALL_IND);

    PZ_WRITE_INSTR_1(PZI_CJMP, PZW_8, PZT_CJMP_8);
    PZ_WRITE_INSTR_1(PZI_CJMP, PZW_16, PZT_CJMP_16);
    PZ_WRITE_INSTR_1(PZI_CJMP, PZW_32, PZT_CJMP_32);
    PZ_WRITE_INSTR_1(PZI_CJMP, PZW_64, PZT_CJMP_64);

    PZ_WRITE_INSTR_0(PZI_JMP, PZT_JMP);
    PZ_WRITE_INSTR_0(PZI_RET, PZT_RET);

    PZ_WRITE_INSTR_0(PZI_ALLOC, PZT_ALLOC);
    PZ_WRITE_INSTR_1(PZI_LOAD, PZW_8, PZT_LOAD_8);
    PZ_WRITE_INSTR_1(PZI_LOAD, PZW_16, PZT_LOAD_16);
    PZ_WRITE_INSTR_1(PZI_LOAD, PZW_32, PZT_LOAD_32);
    PZ_WRITE_INSTR_1(PZI_LOAD, PZW_64, PZT_LOAD_64);
    PZ_WRITE_INSTR_1(PZI_STORE, PZW_8, PZT_STORE_8);
    PZ_WRITE_INSTR_1(PZI_STORE, PZW_16, PZT_STORE_16);
    PZ_WRITE_INSTR_1(PZI_STORE, PZW_32, PZT_STORE_32);
    PZ_WRITE_INSTR_1(PZI_STORE, PZW_64, PZT_STORE_64);

    PZ_WRITE_INSTR_0(PZI_END, PZT_END);
    PZ_WRITE_INSTR_0(PZI_CCALL, PZT_CCALL);

#undef SELECT_IMMEDIATE
#undef PZ_WRITE_INSTR_2
#undef PZ_WRITE_INSTR_1
#undef PZ_WRITE_INSTR_0

error:
    fprintf(stderr, "Bad or unimplemented instruction\n");
    abort();

write_opcode:
    if (proc != NULL) {
        *((uint8_t*)(&proc[offset])) = token;
    }
    offset += 1;

    if (imm_type != IMT_NONE) {
        imm_size = pz_immediate_size(imm_type);
        offset = ALIGN_UP(offset, imm_size);

        if (proc != NULL) {
            switch (imm_type) {
                case IMT_NONE:
                    break;
                case IMT_8:
                    *((uint8_t*)(&proc[offset])) = imm_value.uint8;
                    break;
                case IMT_16:
                case IMT_STRUCT_REF_FIELD:
                    *((uint16_t*)(&proc[offset])) = imm_value.uint16;
                    break;
                case IMT_32:
                    *((uint32_t*)(&proc[offset])) = imm_value.uint32;
                    break;
                case IMT_64:
                    *((uint64_t*)(&proc[offset])) = imm_value.uint64;
                    break;
                case IMT_DATA_REF:
                case IMT_CODE_REF:
                case IMT_STRUCT_REF:
                case IMT_LABEL_REF:
                    *((uintptr_t*)(&proc[offset])) = imm_value.word;
                    break;
            }
        }

        offset += imm_size;
    }

    return offset;
}

