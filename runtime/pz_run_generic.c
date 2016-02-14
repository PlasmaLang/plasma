/*
 * Plasma bytecode exection (generic portable version)
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2015 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#include <stdio.h>
#include <stdlib.h>

#include "pz_common.h"
#include "pz_code.h"
#include "pz_instructions.h"
#include "pz_run.h"
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
 * When given the fast width, return the equivalent absolute width.
 */
static Operand_Width pz_normalize_operand_width(Operand_Width w);


/*
 * Imported procedures
 *
 **********************/

typedef unsigned (*ccall_func)(Stack_Value*, unsigned);

static unsigned
builtin_print_func(Stack_Value *stack, unsigned sp)
{
    char *string = (char*)(stack[sp--].uptr);
    printf("%s", string);
    return sp;
}

Imported_Proc builtin_print = {
    BUILTIN_FOREIGN,
    builtin_print_func
};

/*
 * Long enough for a 32 bit value, plus a sign, plus a null termination
 * byte.
 */
#define INT_TO_STRING_BUFFER_SIZE 11

static unsigned
builtin_int_to_string_func(Stack_Value *stack, unsigned sp)
{
    char    *string;
    int32_t num;
    int     result;

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

Imported_Proc builtin_int_to_string = {
    BUILTIN_FOREIGN,
    builtin_int_to_string_func
};

static unsigned
builtin_free_func(Stack_Value *stack, unsigned sp)
{
    free(stack[sp--].ptr);
    return sp;
}

Imported_Proc builtin_free = {
    BUILTIN_FOREIGN,
    builtin_free_func
};


unsigned pz_fast_word_size = PZ_FAST_INTEGER_WIDTH / 8;


/*
 * Instructions
 *
 ***************/

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
    PZT_DUP,
    PZT_DROP,
    PZT_SWAP,
    PZT_CALL,
    PZT_CJMP_32,
    PZT_RET,
    PZT_END,
    PZT_CCALL
} PZ_Instruction_Token;

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
    int             retcode;

    return_stack = malloc(sizeof(uint8_t*) * RETURN_STACK_SIZE);
    expr_stack = malloc(sizeof(Stack_Value) * EXPR_STACK_SIZE);
    expr_stack[0].u64 = 0;

    /*
     * Assemble a special procedure that exits the interpreter and put its
     * address on the call stack.
     */
    wrapper_proc = pz_code_new_proc(pz_instr_size(PZI_END));
    pz_write_instr(wrapper_proc, 0, PZI_END, 0, 0);
    return_stack[0] = wrapper_proc;

    // Set the instruction pointer and start execution.
    ip = pz_code_get_proc(pz->code, pz->entry_proc);
    retcode = 255;
    while (true) {
        PZ_Instruction_Token token = (PZ_Instruction_Token)(*ip);

        ip++;
        switch (token) {
            case PZT_NOP:
                break;
            case PZT_LOAD_IMMEDIATE_8:
                expr_stack[++esp].u8 = *ip;
                ip++;
                break;
            case PZT_LOAD_IMMEDIATE_16:
                ip = (uint8_t*)ALIGN_UP((uintptr_t)ip, 2);
                expr_stack[++esp].u16 = *(uint16_t*)ip;
                ip += 2;
                break;
            case PZT_LOAD_IMMEDIATE_32:
                ip = (uint8_t*)ALIGN_UP((uintptr_t)ip, 4);
                expr_stack[++esp].u32 = *(uint32_t*)ip;
                ip += 4;
                break;
            case PZT_LOAD_IMMEDIATE_64:
                ip = (uint8_t*)ALIGN_UP((uintptr_t)ip, 8);
                expr_stack[++esp].u64 = *(uint64_t*)ip;
                ip += 8;
                break;
            case PZT_LOAD_IMMEDIATE_DATA:
                ip = (uint8_t*)ALIGN_UP((uintptr_t)ip, MACHINE_WORD_SIZE);
                expr_stack[++esp].uptr = *(uintptr_t*)ip;
                ip += MACHINE_WORD_SIZE;
                break;
            case PZT_ZE_8_16:
                expr_stack[esp].u16 = expr_stack[esp].u8;
                break;
            case PZT_ZE_8_32:
                expr_stack[esp].u32 = expr_stack[esp].u8;
                break;
            case PZT_ZE_8_64:
                expr_stack[esp].u64 = expr_stack[esp].u8;
                break;
            case PZT_ZE_16_32:
                expr_stack[esp].u32 = expr_stack[esp].u16;
                break;
            case PZT_ZE_16_64:
                expr_stack[esp].u64 = expr_stack[esp].u16;
                break;
            case PZT_ZE_32_64:
                expr_stack[esp].u64 = expr_stack[esp].u32;
                break;
            case PZT_SE_8_16:
                expr_stack[esp].s16 = expr_stack[esp].s8;
                break;
            case PZT_SE_8_32:
                expr_stack[esp].s32 = expr_stack[esp].s8;
                break;
            case PZT_SE_8_64:
                expr_stack[esp].s64 = expr_stack[esp].s8;
                break;
            case PZT_SE_16_32:
                expr_stack[esp].s32 = expr_stack[esp].s16;
                break;
            case PZT_SE_16_64:
                expr_stack[esp].s64 = expr_stack[esp].s16;
                break;
            case PZT_SE_32_64:
                expr_stack[esp].s64 = expr_stack[esp].s32;
                break;
            case PZT_TRUNC_64_32:
                expr_stack[esp].u32 = expr_stack[esp].u64 & 0xFFFFFFFFu;
                break;
            case PZT_TRUNC_64_16:
                expr_stack[esp].u16 = expr_stack[esp].u64 & 0xFFFF;
                break;
            case PZT_TRUNC_64_8:
                expr_stack[esp].u8 = expr_stack[esp].u64 & 0xFF;
                break;
            case PZT_TRUNC_32_16:
                expr_stack[esp].u16 = expr_stack[esp].u32 & 0xFFFF;
                break;
            case PZT_TRUNC_32_8:
                expr_stack[esp].u8 = expr_stack[esp].u32 & 0xFF;
                break;
            case PZT_TRUNC_16_8:
                expr_stack[esp].u8 = expr_stack[esp].u16 & 0xFF;
                break;

#define PZ_RUN_ARITHMETIC(opcode_base, width, signedness, operator) \
            case opcode_base ## _ ## width: \
                expr_stack[esp-1].signedness ## width = \
                    (expr_stack[esp-1].signedness ## width \
                        operator expr_stack[esp].signedness ## width); \
                esp--; \
                break

            PZ_RUN_ARITHMETIC(PZT_ADD, 8, s, +);
            PZ_RUN_ARITHMETIC(PZT_ADD, 16, s, +);
            PZ_RUN_ARITHMETIC(PZT_ADD, 32, s, +);
            PZ_RUN_ARITHMETIC(PZT_ADD, 64, s, +);
            PZ_RUN_ARITHMETIC(PZT_SUB, 8, s, -);
            PZ_RUN_ARITHMETIC(PZT_SUB, 16, s, -);
            PZ_RUN_ARITHMETIC(PZT_SUB, 32, s, -);
            PZ_RUN_ARITHMETIC(PZT_SUB, 64, s, -);
            PZ_RUN_ARITHMETIC(PZT_MUL, 8, s, *);
            PZ_RUN_ARITHMETIC(PZT_MUL, 16, s, *);
            PZ_RUN_ARITHMETIC(PZT_MUL, 32, s, *);
            PZ_RUN_ARITHMETIC(PZT_MUL, 64, s, *);
            PZ_RUN_ARITHMETIC(PZT_DIV, 8, s, /);
            PZ_RUN_ARITHMETIC(PZT_DIV, 16, s, /);
            PZ_RUN_ARITHMETIC(PZT_DIV, 32, s, /);
            PZ_RUN_ARITHMETIC(PZT_DIV, 64, s, /);
            PZ_RUN_ARITHMETIC(PZT_LT_U, 8, u, <);
            PZ_RUN_ARITHMETIC(PZT_LT_U, 16, u, <);
            PZ_RUN_ARITHMETIC(PZT_LT_U, 32, u, <);
            PZ_RUN_ARITHMETIC(PZT_LT_U, 64, u, <);
            PZ_RUN_ARITHMETIC(PZT_LT_S, 8, s, <);
            PZ_RUN_ARITHMETIC(PZT_LT_S, 16, s, <);
            PZ_RUN_ARITHMETIC(PZT_LT_S, 32, s, <);
            PZ_RUN_ARITHMETIC(PZT_LT_S, 64, s, <);
            PZ_RUN_ARITHMETIC(PZT_GT_U, 8, u, >);
            PZ_RUN_ARITHMETIC(PZT_GT_U, 16, u, >);
            PZ_RUN_ARITHMETIC(PZT_GT_U, 32, u, >);
            PZ_RUN_ARITHMETIC(PZT_GT_U, 64, u, >);
            PZ_RUN_ARITHMETIC(PZT_GT_S, 8, s, >);
            PZ_RUN_ARITHMETIC(PZT_GT_S, 16, s, >);
            PZ_RUN_ARITHMETIC(PZT_GT_S, 32, s, >);
            PZ_RUN_ARITHMETIC(PZT_GT_S, 64, s, >);

#undef PZ_RUN_ARITHMETIC

            case PZT_DUP:
                esp++;
                expr_stack[esp] = expr_stack[esp-1];
                break;
            case PZT_DROP:
                esp--;
                break;
            case PZT_SWAP: {
                Stack_Value temp;
                temp = expr_stack[esp];
                expr_stack[esp] = expr_stack[esp-1];
                expr_stack[esp-1] = temp;
                break;
            }
            case PZT_CALL:
                ip = (uint8_t*)ALIGN_UP((uintptr_t)ip, MACHINE_WORD_SIZE);
                return_stack[++rsp] = (ip + MACHINE_WORD_SIZE);
                ip = *(uint8_t**)ip;
                break;
            case PZT_CJMP_32:
                ip = (uint8_t*)ALIGN_UP((uintptr_t)ip, MACHINE_WORD_SIZE);
                if (expr_stack[esp--].u32) {
                    ip = *(uint8_t**)ip;
                } else {
                    ip += MACHINE_WORD_SIZE;
                }
                break;
            case PZT_RET:
                ip = return_stack[rsp--];
                break;
            case PZT_END:
                retcode = expr_stack[esp].s32;
                goto finish;
            case PZT_CCALL:
            {
                ccall_func callee;
                ip = (uint8_t*)ALIGN_UP((uintptr_t)ip, MACHINE_WORD_SIZE);
                callee = *(ccall_func*)ip;
                esp = callee(expr_stack, esp);
                ip += MACHINE_WORD_SIZE;
                break;
            }
        }
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

unsigned
pz_immediate_alignment(Immediate_Type imt, unsigned offset)
{
    return ALIGN_UP(offset, pz_immediate_size(imt));
}

unsigned
pz_immediate_size(Immediate_Type imt)
{
    switch (imt) {
        case IMT_NONE:
            return 0;
        case IMT_8:
            // return ROUND_UP(1, MACHINE_WORD_SIZE)/MACHINE_WORD_SIZE;
            return 1;
        case IMT_16:
            return 2;
        case IMT_32:
            return 4;
        case IMT_64:
            return 8;
        case IMT_DATA_REF:
        case IMT_CODE_REF:
        case IMT_LABEL_REF:
            return MACHINE_WORD_SIZE;
    }
    abort();
}

unsigned
pz_instr_size(Opcode opcode)
{
    return 1;
}

static Operand_Width
pz_normalize_operand_width(Operand_Width w)
{
    if (w == PZOW_FAST) {
        switch (PZ_FAST_INTEGER_WIDTH) {
            case 32: return PZOW_32;
            case 64: return PZOW_64;
            default:
                fprintf(stderr,
                    "PZ_FAST_INTEGER_WIDTH has unanticipated value\n");
                abort();
        }
    } else {
        return w;
    }
}

void
pz_write_instr(uint8_t *proc, unsigned offset, Opcode opcode,
    Operand_Width width1, Operand_Width width2)
{
    PZ_Instruction_Token token;

    width1 = pz_normalize_operand_width(width1);
    width2 = pz_normalize_operand_width(width2);

#define PZ_WRITE_INSTR_0(code, tok) \
    do { \
        if (opcode == (code)) { \
            token = (tok); \
            goto end; \
        } \
    } while (0)
#define PZ_WRITE_INSTR_1(code, w1, tok) \
    do { \
        if (opcode == (code) && width1 == (w1)) { \
            token = (tok); \
            goto end; \
        } \
    } while (0)
#define PZ_WRITE_INSTR_2(code, w1, w2, tok) \
    do { \
        if (opcode == (code) && width1 == (w1) && width2 == (w2)) { \
            token = (tok); \
            goto end; \
        } \
    } while (0)

    PZ_WRITE_INSTR_1(PZI_LOAD_IMMEDIATE_NUM, PZOW_8, PZT_LOAD_IMMEDIATE_8);
    PZ_WRITE_INSTR_1(PZI_LOAD_IMMEDIATE_NUM, PZOW_16, PZT_LOAD_IMMEDIATE_16);
    PZ_WRITE_INSTR_1(PZI_LOAD_IMMEDIATE_NUM, PZOW_32, PZT_LOAD_IMMEDIATE_32);
    PZ_WRITE_INSTR_1(PZI_LOAD_IMMEDIATE_NUM, PZOW_64, PZT_LOAD_IMMEDIATE_64);
    // TODO: PZ_WRITE_INSTR_1(PZI_LOAD_IMMEDIATE_NUM, PZOW_PTR, ...);

    PZ_WRITE_INSTR_0(PZI_LOAD_IMMEDIATE_DATA, PZT_LOAD_IMMEDIATE_DATA);

    PZ_WRITE_INSTR_2(PZI_ZE, PZOW_8, PZOW_8, PZT_NOP);
    PZ_WRITE_INSTR_2(PZI_ZE, PZOW_8, PZOW_16, PZT_ZE_8_16);
    PZ_WRITE_INSTR_2(PZI_ZE, PZOW_8, PZOW_32, PZT_ZE_8_32);
    PZ_WRITE_INSTR_2(PZI_ZE, PZOW_8, PZOW_64, PZT_ZE_8_64);
    PZ_WRITE_INSTR_2(PZI_ZE, PZOW_16, PZOW_16, PZT_NOP);
    PZ_WRITE_INSTR_2(PZI_ZE, PZOW_16, PZOW_32, PZT_ZE_16_32);
    PZ_WRITE_INSTR_2(PZI_ZE, PZOW_16, PZOW_64, PZT_ZE_16_64);
    PZ_WRITE_INSTR_2(PZI_ZE, PZOW_32, PZOW_32, PZT_NOP);
    PZ_WRITE_INSTR_2(PZI_ZE, PZOW_32, PZOW_64, PZT_ZE_32_64);

    PZ_WRITE_INSTR_2(PZI_SE, PZOW_8, PZOW_8, PZT_NOP);
    PZ_WRITE_INSTR_2(PZI_SE, PZOW_8, PZOW_16, PZT_SE_8_16);
    PZ_WRITE_INSTR_2(PZI_SE, PZOW_8, PZOW_32, PZT_SE_8_32);
    PZ_WRITE_INSTR_2(PZI_SE, PZOW_8, PZOW_64, PZT_SE_8_64);
    PZ_WRITE_INSTR_2(PZI_SE, PZOW_16, PZOW_16, PZT_NOP);
    PZ_WRITE_INSTR_2(PZI_SE, PZOW_16, PZOW_32, PZT_SE_16_32);
    PZ_WRITE_INSTR_2(PZI_SE, PZOW_16, PZOW_64, PZT_SE_16_64);
    PZ_WRITE_INSTR_2(PZI_SE, PZOW_32, PZOW_32, PZT_NOP);
    PZ_WRITE_INSTR_2(PZI_SE, PZOW_32, PZOW_64, PZT_SE_32_64);

    PZ_WRITE_INSTR_2(PZI_TRUNC, PZOW_8, PZOW_8, PZT_NOP);
    PZ_WRITE_INSTR_2(PZI_TRUNC, PZOW_16, PZOW_16, PZT_NOP);
    PZ_WRITE_INSTR_2(PZI_TRUNC, PZOW_16, PZOW_8, PZT_TRUNC_16_8);
    PZ_WRITE_INSTR_2(PZI_TRUNC, PZOW_32, PZOW_32, PZT_NOP);
    PZ_WRITE_INSTR_2(PZI_TRUNC, PZOW_32, PZOW_16, PZT_TRUNC_32_16);
    PZ_WRITE_INSTR_2(PZI_TRUNC, PZOW_32, PZOW_8, PZT_TRUNC_32_8);
    PZ_WRITE_INSTR_2(PZI_TRUNC, PZOW_64, PZOW_64, PZT_NOP);
    PZ_WRITE_INSTR_2(PZI_TRUNC, PZOW_64, PZOW_32, PZT_TRUNC_64_32);
    PZ_WRITE_INSTR_2(PZI_TRUNC, PZOW_64, PZOW_16, PZT_TRUNC_64_16);
    PZ_WRITE_INSTR_2(PZI_TRUNC, PZOW_64, PZOW_8, PZT_TRUNC_64_8);

    PZ_WRITE_INSTR_1(PZI_ADD, PZOW_8, PZT_ADD_8);
    PZ_WRITE_INSTR_1(PZI_ADD, PZOW_16, PZT_ADD_16);
    PZ_WRITE_INSTR_1(PZI_ADD, PZOW_32, PZT_ADD_32);
    PZ_WRITE_INSTR_1(PZI_ADD, PZOW_64, PZT_ADD_64);

    PZ_WRITE_INSTR_1(PZI_SUB, PZOW_8, PZT_SUB_8);
    PZ_WRITE_INSTR_1(PZI_SUB, PZOW_16, PZT_SUB_16);
    PZ_WRITE_INSTR_1(PZI_SUB, PZOW_32, PZT_SUB_32);
    PZ_WRITE_INSTR_1(PZI_SUB, PZOW_64, PZT_SUB_64);

    PZ_WRITE_INSTR_1(PZI_MUL, PZOW_8, PZT_MUL_8);
    PZ_WRITE_INSTR_1(PZI_MUL, PZOW_16, PZT_MUL_16);
    PZ_WRITE_INSTR_1(PZI_MUL, PZOW_32, PZT_MUL_32);
    PZ_WRITE_INSTR_1(PZI_MUL, PZOW_64, PZT_MUL_64);

    PZ_WRITE_INSTR_1(PZI_DIV, PZOW_8, PZT_DIV_8);
    PZ_WRITE_INSTR_1(PZI_DIV, PZOW_16, PZT_DIV_16);
    PZ_WRITE_INSTR_1(PZI_DIV, PZOW_32, PZT_DIV_32);
    PZ_WRITE_INSTR_1(PZI_DIV, PZOW_64, PZT_DIV_64);

    PZ_WRITE_INSTR_1(PZI_LT_U, PZOW_8, PZT_LT_U_8);
    PZ_WRITE_INSTR_1(PZI_LT_U, PZOW_16, PZT_LT_U_16);
    PZ_WRITE_INSTR_1(PZI_LT_U, PZOW_32, PZT_LT_U_32);
    PZ_WRITE_INSTR_1(PZI_LT_U, PZOW_64, PZT_LT_U_64);

    PZ_WRITE_INSTR_1(PZI_LT_S, PZOW_8, PZT_LT_S_8);
    PZ_WRITE_INSTR_1(PZI_LT_S, PZOW_16, PZT_LT_S_16);
    PZ_WRITE_INSTR_1(PZI_LT_S, PZOW_32, PZT_LT_S_32);
    PZ_WRITE_INSTR_1(PZI_LT_S, PZOW_64, PZT_LT_S_64);

    PZ_WRITE_INSTR_1(PZI_GT_U, PZOW_8, PZT_GT_U_8);
    PZ_WRITE_INSTR_1(PZI_GT_U, PZOW_16, PZT_GT_U_16);
    PZ_WRITE_INSTR_1(PZI_GT_U, PZOW_32, PZT_GT_U_32);
    PZ_WRITE_INSTR_1(PZI_GT_U, PZOW_64, PZT_GT_U_64);

    PZ_WRITE_INSTR_1(PZI_GT_S, PZOW_8, PZT_GT_S_8);
    PZ_WRITE_INSTR_1(PZI_GT_S, PZOW_16, PZT_GT_S_16);
    PZ_WRITE_INSTR_1(PZI_GT_S, PZOW_32, PZT_GT_S_32);
    PZ_WRITE_INSTR_1(PZI_GT_S, PZOW_64, PZT_GT_S_64);

    PZ_WRITE_INSTR_0(PZI_DUP, PZT_DUP);
    PZ_WRITE_INSTR_0(PZI_DROP, PZT_DROP);
    PZ_WRITE_INSTR_0(PZI_SWAP, PZT_SWAP);

    PZ_WRITE_INSTR_0(PZI_CALL, PZT_CALL);

    // TODO: PZ_WRITE_INSTR_1(PZI_CJMP, PZOW_8, PZTCJMPS_8);
    // TODO: PZ_WRITE_INSTR_1(PZI_CJMP, PZOW_16, PZT_CJMP_16);
    PZ_WRITE_INSTR_1(PZI_CJMP, PZOW_32, PZT_CJMP_32);
    // TODO: PZ_WRITE_INSTR_1(PZI_CJMP, PZOW_64, PZT_CJMP_64);

    PZ_WRITE_INSTR_0(PZI_RET, PZT_RET);
    PZ_WRITE_INSTR_0(PZI_END, PZT_END);
    PZ_WRITE_INSTR_0(PZI_CCALL, PZT_CCALL);

#undef PZ_WRITE_INSTR_2
#undef PZ_WRITE_INSTR_1
#undef PZ_WRITE_INSTR_0

    fprintf(stderr, "Bad or unimplemented instruction\n");
    abort();

end:
    *((uint8_t*)(&proc[offset])) = token;
    return;
}

void
pz_write_imm8(uint8_t *proc, unsigned offset, uint8_t val)
{
    *((uint8_t*)(&proc[offset])) = (uintptr_t)val;
}

void
pz_write_imm16(uint8_t *proc, unsigned offset, uint16_t val)
{
    *((uint16_t*)(&proc[offset])) = (uintptr_t)val;
}

void
pz_write_imm32(uint8_t *proc, unsigned offset, uint32_t val)
{
    *((uint32_t*)(&proc[offset])) = (uintptr_t)val;
}

void
pz_write_imm64(uint8_t *proc, unsigned offset, uint64_t val)
{
    *((uint64_t*)(&proc[offset])) = val;
}

void
pz_write_imm_word(uint8_t *proc, unsigned offset, uintptr_t val)
{
    *((uintptr_t*)(&proc[offset])) = val;
}

