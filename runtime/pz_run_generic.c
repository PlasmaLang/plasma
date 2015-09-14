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

#define sdefault s32
#define udefault u32

typedef union stack_value {
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
} stack_value;


/*
 * Imported procedures
 *
 **********************/

typedef unsigned (*ccall_func)(stack_value*, unsigned);

static unsigned
builtin_print_func(stack_value* stack, unsigned sp)
{
    char* string = (char*)(stack[sp--].uptr);
    printf(string);
    return sp;
}

imported_proc builtin_print = {
    BUILTIN_FOREIGN,
    builtin_print_func
};

/*
 * Long enough for a 32 bit value, plus a sign, plus a null termination
 * byte.
 */
#define INT_TO_STRING_BUFFER_SIZE 11

static unsigned
builtin_int_to_string_func(stack_value* stack, unsigned sp)
{
    char *string;
    int32_t num;
    int result;

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

imported_proc builtin_int_to_string = {
    BUILTIN_FOREIGN,
    builtin_int_to_string_func
};

static unsigned
builtin_free_func(stack_value* stack, unsigned sp)
{
    free(stack[sp--].ptr);
    return sp;
}

imported_proc builtin_free = {
    BUILTIN_FOREIGN,
    builtin_free_func
};


unsigned
pz_fast_word_size = 4;


/*
 * Instructions
 *
 ***************/

/*
 * For the generic interpreter the in-memory words are identical to the
 * on-disk words.
 */
uint8_t pz_instruction_words[] = {
    PZI_LOAD_IMMEDIATE_8,
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
    PZI_RETURN,
    PZI_END,
    PZI_CCALL
};

/*
 * Run the program
 *
 ******************/

int pz_run(pz* pz) {
    uint8_t**       return_stack;
    unsigned        rsp = 0;
    stack_value*    expr_stack;
    unsigned        esp = 0;
    uint8_t*        ip;
    uint8_t*        wrapper_proc;
    int             retcode;

    return_stack = malloc(sizeof(uint8_t*) * RETURN_STACK_SIZE);
    expr_stack = malloc(sizeof(stack_value) * EXPR_STACK_SIZE);
    expr_stack[0].u64 = 0;

    /*
     * Assemble a special procedure that exits the interpreter and put its
     * address on the call stack.
     */
    wrapper_proc = pz_code_new_proc(pz_instr_size(PZI_END));
    pz_write_instr(wrapper_proc, 0, PZI_END);
    return_stack[0] = wrapper_proc;

    // Set the instruction pointer.
    ip = pz_code_get_proc(pz->code, pz->entry_proc);
    retcode = 255;
    while (true) {
        uint8_t*        last_ip = ip;
        ccall_func      callee;

        ip++;
        switch (*last_ip) {
            case PZI_LOAD_IMMEDIATE_8:
                expr_stack[++esp].u8 = *ip;
                ip++;
                break;
            case PZI_LOAD_IMMEDIATE_16:
                ip = (uint8_t*)ALIGN_UP((uintptr_t)ip, 2);
                expr_stack[++esp].u16 = *(uint16_t*)ip;
                ip += 2;
                break;
            case PZI_LOAD_IMMEDIATE_32:
                ip = (uint8_t*)ALIGN_UP((uintptr_t)ip, 4);
                expr_stack[++esp].u32 = *(uint32_t*)ip;
                ip += 4;
                break;
            case PZI_LOAD_IMMEDIATE_64:
                ip = (uint8_t*)ALIGN_UP((uintptr_t)ip, 8);
                expr_stack[++esp].u64 = *(uint64_t*)ip;
                ip += 8;
                break;
            case PZI_LOAD_IMMEDIATE_DATA:
                ip = (uint8_t*)ALIGN_UP((uintptr_t)ip, MACHINE_WORD_SIZE);
                expr_stack[++esp].uptr = *(uintptr_t*)ip;
                ip += MACHINE_WORD_SIZE;
                break;
            case PZI_ZE_8_16:
                expr_stack[esp].u16 = expr_stack[esp].u8;
                break;
            case PZI_ZE_16_32:
                expr_stack[esp].u32 = expr_stack[esp].u16;
                break;
            case PZI_ZE_32_64:
                expr_stack[esp].u64 = expr_stack[esp].u32;
                break;
            case PZI_SE_8_16:
                expr_stack[esp].s16 = expr_stack[esp].s8;
                break;
            case PZI_SE_16_32:
                expr_stack[esp].s32 = expr_stack[esp].s16;
                break;
            case PZI_SE_32_64:
                expr_stack[esp].s64 = expr_stack[esp].s64;
                break;
            case PZI_TRUNC_64_32:
                expr_stack[esp].u32 =
                    (uint32_t)(0xFFFFFFFF & expr_stack[esp].u64);
                break;
            case PZI_TRUNC_32_16:
                expr_stack[esp].u16 =
                    (uint16_t)(0xFFFF & expr_stack[esp].u32);
                break;
            case PZI_TRUNC_16_8:
                expr_stack[esp].u8 =
                    (uint8_t)(0xFF & expr_stack[esp].u16);
                break;
            case PZI_ZE_32_FAST:
            case PZI_SE_32_FAST:
            case PZI_TRUNC_FAST_32:
                break;
            case PZI_ADD:
                expr_stack[esp-1].sdefault += expr_stack[esp].sdefault;
                esp--;
                break;
            case PZI_SUB:
                expr_stack[esp-1].sdefault -= expr_stack[esp].sdefault;
                esp--;
                break;
            case PZI_MUL:
                expr_stack[esp-1].sdefault *= expr_stack[esp].sdefault;
                esp--;
                break;
            case PZI_DIV:
                expr_stack[esp-1].sdefault /= expr_stack[esp].sdefault;
                esp--;
                break;
            case PZI_DUP:
                esp++;
                expr_stack[esp] = expr_stack[esp-1];
                break;
            case PZI_SWAP: {
                stack_value temp;
                temp = expr_stack[esp];
                expr_stack[esp] = expr_stack[esp-1];
                expr_stack[esp-1] = temp;
                break;
            }
            case PZI_CALL:
                ip = (uint8_t*)ALIGN_UP((uintptr_t)ip, MACHINE_WORD_SIZE);
                return_stack[++rsp] = (ip + MACHINE_WORD_SIZE);
                ip = *(uint8_t**)ip;
                break;
            case PZI_RETURN:
                ip = return_stack[rsp--];
                break;
            case PZI_END:
                retcode = expr_stack[esp].sdefault;
                goto finish;
            case PZI_CCALL:
                ip = (uint8_t*)ALIGN_UP((uintptr_t)ip, MACHINE_WORD_SIZE);
                callee = *(ccall_func*)ip;
                esp = callee(expr_stack, esp);
                ip += MACHINE_WORD_SIZE;
                break;
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
pz_immediate_alignment(enum immediate_type imt, unsigned offset)
{
    return ALIGN_UP(offset, pz_immediate_size(imt));
}

unsigned
pz_immediate_size(enum immediate_type imt)
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
pz_instr_size(opcode opcode)
{
    return 1;
}

void
pz_write_instr(uint8_t* proc, unsigned offset, opcode opcode)
{
    *((uint8_t*)(&proc[offset])) = pz_instruction_words[opcode];
}

void
pz_write_imm8(uint8_t* proc, unsigned offset, uint8_t val)
{
    *((uint8_t*)(&proc[offset])) = (uintptr_t)val;
}

void
pz_write_imm16(uint8_t* proc, unsigned offset, uint16_t val)
{
    *((uint16_t*)(&proc[offset])) = (uintptr_t)val;
}

void
pz_write_imm32(uint8_t* proc, unsigned offset, uint32_t val)
{
    *((uint32_t*)(&proc[offset])) = (uintptr_t)val;
}

void
pz_write_imm64(uint8_t* proc, unsigned offset, uint64_t val)
{
    *((uint64_t*)(&proc[offset])) = val;
}

void
pz_write_imm_word(uint8_t* proc, unsigned offset, uintptr_t val)
{
    *((uintptr_t*)(&proc[offset])) = val;
}

