/*
 * Plasma bytecode exection (generic portable version)
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2015 Paul Bone
 * Distributed under the terms of the MIT license, see ../LICENSE.runtime
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

/*
 * XXX: there are a number of improvements that need to be made to make this
 * more modular.
 * XXX: immediate data is not aligned and therefore this code is not
 * portable.
 */

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
 * Imported procedures
 *
 **********************/

typedef unsigned (*ccall_func)(Stack_Value*, unsigned);

static unsigned
builtin_print_func(Stack_Value *stack, unsigned sp)
{
    char *string = (char*)(stack[sp--].uptr);
    printf(string);
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
    // XXX: memory leak.
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


unsigned pz_fast_word_size = 4;


/*
 * Instructions
 *
 ***************/

/*
 * Tokens for the token-oriented execution.
 */
typedef enum {
    PZT_LOAD_IMMEDIATE_8,
    PZT_LOAD_IMMEDIATE_16,
    PZT_LOAD_IMMEDIATE_32,
    PZT_LOAD_IMMEDIATE_64,
    PZT_LOAD_IMMEDIATE_DATA,
    PZT_ADD_32,
    PZT_SUB_32,
    PZT_MUL_32,
    PZT_DIV_32,
    PZT_DUP_32,
    PZT_SWAP_32_32,
    PZT_CALL,
    PZT_RETURN,
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
            case PZT_ADD_32:
                expr_stack[esp-1].s32 += expr_stack[esp].s32;
                esp--;
                break;
            case PZT_SUB_32:
                expr_stack[esp-1].s32 -= expr_stack[esp].s32;
                esp--;
                break;
            case PZT_MUL_32:
                expr_stack[esp-1].s32 *= expr_stack[esp].s32;
                esp--;
                break;
            case PZT_DIV_32:
                expr_stack[esp-1].s32 /= expr_stack[esp].s32;
                esp--;
                break;
            case PZT_DUP_32:
                esp++;
                expr_stack[esp].u32 = expr_stack[esp-1].u32;
                break;
            case PZT_SWAP_32_32: {
                uint32_t temp;
                temp = expr_stack[esp].u32;
                expr_stack[esp].u32 = expr_stack[esp-1].u32;
                expr_stack[esp-1].u32 = temp;
                break;
            }
            case PZT_CALL:
                ip = (uint8_t*)ALIGN_UP((uintptr_t)ip, MACHINE_WORD_SIZE);
                return_stack[++rsp] = (ip + MACHINE_WORD_SIZE);
                ip = *(uint8_t**)ip;
                break;
            case PZT_RETURN:
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
            default:
                fprintf(stderr, "Unknown opcode\n");
                abort();
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
            return MACHINE_WORD_SIZE;
    }
    abort();
}

unsigned
pz_instr_size(Opcode opcode)
{
    return 1;
}

void
pz_write_instr(uint8_t *proc, unsigned offset, Opcode opcode,
    Operand_Width width1, Operand_Width width2)
{
    PZ_Instruction_Token token;

    /* XXX: Try to do this with arrays */
    switch (opcode) {
        case PZI_LOAD_IMMEDIATE_NUM:
            switch (width1) {
                case PZOW_8:
                    token = PZT_LOAD_IMMEDIATE_8;
                    break;
                case PZOW_16:
                    token = PZT_LOAD_IMMEDIATE_16;
                    break;
                case PZOW_32:
                case PZOW_FAST:
                    token = PZT_LOAD_IMMEDIATE_32;
                    break;
                case PZOW_64:
                    token = PZT_LOAD_IMMEDIATE_64;
                    break;
                case PZOW_PTR:
                    fprintf(stderr,
                        "Unimplemented poinder width immedate load\n");
                    abort();
            }
            break;
        case PZI_LOAD_IMMEDIATE_DATA:
            token = PZT_LOAD_IMMEDIATE_DATA;
            break;
        case PZI_ZE:
            fprintf(stderr, "Unimplemented eero extend\n");
            abort();
        case PZI_SE:
            fprintf(stderr, "Unimplemented sign extend\n");
            abort();
        case PZI_TRUNC:
            fprintf(stderr, "Unimplemented trucate\n");
            abort();
        case PZI_ADD:
            switch (width1) {
                case PZOW_32:
                case PZOW_FAST:
                    token = PZT_ADD_32;
                    break;
                default:
                    fprintf(stderr, "Unimplemented add other width\n");
                    abort();
            }
            break;
        case PZI_SUB:
            switch (width1) {
                case PZOW_32:
                case PZOW_FAST:
                    token = PZT_SUB_32;
                    break;
                default:
                    fprintf(stderr, "Unimplemented sub other width\n");
                    abort();
            }
            break;
        case PZI_MUL:
            switch (width1) {
                case PZOW_32:
                case PZOW_FAST:
                    token = PZT_MUL_32;
                    break;
                default:
                    fprintf(stderr, "Unimplemented mul other width\n");
                    abort();
            }
            break;
        case PZI_DIV:
            switch (width1) {
                case PZOW_32:
                case PZOW_FAST:
                    token = PZT_DIV_32;
                    break;
                default:
                    fprintf(stderr, "Unimplemented div other width\n");
                    abort();
            }
            break;
        case PZI_DUP:
            switch (width1) {
                case PZOW_32:
                case PZOW_FAST:
                    token = PZT_DUP_32;
                    break;
                default:
                    fprintf(stderr, "Unimplemented dup other width\n");
                    abort();
            }
            break;
        case PZI_SWAP:
            /* XXX: width2 */
            /* XXX: Alignment issues if the stack is packed */
            switch (width1) {
                case PZOW_32:
                case PZOW_FAST:
                    token = PZT_SWAP_32_32;
                    break;
                default:
                    fprintf(stderr, "Unimplemented swap other width\n");
                    abort();
            }
            break;
        case PZI_CALL:
            token = PZT_CALL;
            break;
        case PZI_RETURN:
            token = PZT_RETURN;
            break;
        case PZI_END:
            token = PZT_END;
            break;
        case PZI_CCALL:
            token = PZT_CCALL;
            break;
    }

    *((uint8_t*)(&proc[offset])) = token;
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

