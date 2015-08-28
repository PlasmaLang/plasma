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

typedef union stack_value {
    uint8_t     u8;
    uint16_t    u16;
    uint32_t    u32;
    uint64_t    u64;
    uintptr_t   uptr;
} stack_value;


/*
 * Imported procedures
 *
 **********************/

typedef unsigned (*ccall_func)(stack_value*, unsigned);

static unsigned
builtin_print_func(stack_value* stack, unsigned sp)
{
    char* string = (char*)(stack[--sp].uptr);
    printf(string);
    return sp;
}

imported_proc builtin_print = {
    BUILTIN_FOREIGN,
    builtin_print_func
};


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
    uint32_t        wrapper_proc_size;
    unsigned        offset;

    return_stack = malloc(sizeof(uint8_t*) * RETURN_STACK_SIZE);
    expr_stack = malloc(sizeof(stack_value) * EXPR_STACK_SIZE);

    /*
     * Assemble a special procedure that calls the entry procedure and then
     * upon return exits the interpreter
     */
    wrapper_proc_size = pz_instr_size(PZI_CALL);
    wrapper_proc_size =
        pz_immediate_alignment(IMT_CODE_REF, wrapper_proc_size);
    wrapper_proc_size += pz_immediate_size(IMT_CODE_REF) +
        pz_instr_size(PZI_END);
    wrapper_proc = pz_code_new_proc(wrapper_proc_size);
    offset = 0;
    pz_write_instr(wrapper_proc, offset, PZI_CALL);
    offset += pz_instr_size(PZI_CALL);
    // Write the address of the entry procedure.
    offset = pz_immediate_alignment(IMT_CODE_REF, offset);
    pz_write_imm_word(wrapper_proc, offset,
        (uintptr_t)pz_code_get_proc(pz->code, pz->entry_proc));
    offset += pz_immediate_size(IMT_CODE_REF);
    pz_write_instr(wrapper_proc, offset, PZI_END);

    // Set the instruction pointer.
    ip = wrapper_proc;

    while (true) {
        uint8_t*        last_ip = ip;
        ccall_func      callee;

        ip++;
        switch (*last_ip) {
            case PZI_LOAD_IMMEDIATE_8:
                expr_stack[esp++].u8 = *ip;
                ip++;
                break;
            case PZI_LOAD_IMMEDIATE_16:
                ip = (uint8_t*)ALIGN_UP((uintptr_t)ip, 2);
                expr_stack[esp++].u16 = *(uint16_t*)ip;
                ip += 2;
                break;
            case PZI_LOAD_IMMEDIATE_32:
                ip = (uint8_t*)ALIGN_UP((uintptr_t)ip, 4);
                expr_stack[esp++].u32 = *(uint32_t*)ip;
                ip += 4;
                break;
            case PZI_LOAD_IMMEDIATE_64:
                ip = (uint8_t*)ALIGN_UP((uintptr_t)ip, 8);
                expr_stack[esp++].u64 = *(uint64_t*)ip;
                ip += 8;
                break;
            case PZI_LOAD_IMMEDIATE_DATA:
                ip = (uint8_t*)ALIGN_UP((uintptr_t)ip, MACHINE_WORD_SIZE);
                expr_stack[esp++].uptr = *(uintptr_t*)ip;
                ip += MACHINE_WORD_SIZE;
                break;
            case PZI_CALL:
                ip = (uint8_t*)ALIGN_UP((uintptr_t)ip, MACHINE_WORD_SIZE);
                return_stack[rsp++] = (ip + MACHINE_WORD_SIZE);
                ip = *(uint8_t**)ip;
                break;
            case PZI_RETURN:
                ip = return_stack[--rsp];
                break;
            case PZI_END:
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

    return 0;
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

