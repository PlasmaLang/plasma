/*
 * Plasma bytecode code structures and functions
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2015 Paul Bone
 * Distributed under the terms of the MIT license, see ../LICENSE.runtime
 */

#include <stdlib.h>
#include <string.h>

#include "pz_common.h"
#include "pz_code.h"
#include "pz_util.h"

pz_code* pz_code_init(uint_fast32_t num_procs)
{
    pz_code* code;

    code = malloc(sizeof(pz_code));
    code->num_procs = num_procs;
    code->procs = malloc(sizeof(uintptr_t*) * num_procs);
    memset(code->procs, 0, sizeof(uintptr_t*) * num_procs);
    code->total_size = 0;

    return code;
}

void pz_code_free(pz_code* code)
{
    for (uint32_t i = 0; i < code->num_procs; i++) {
        if (code->procs[i] != NULL) {
            free(code->procs[i]);
        }
    }
    free(code->procs);

    free(code);
}

uint8_t*
pz_code_new_proc(uint32_t proc_size)
{
    return malloc(sizeof(uint8_t) * proc_size);
}

uint8_t*
pz_code_get_proc(pz_code* code, uint32_t index)
{
    return code->procs[index];
}

/*
 * Instruction encoding
 *
 *************************/

enum immediate_type
pz_code_immediate(opcode opcode)
{
    switch (opcode) {
        case PZI_LOAD_IMMEDIATE_8:
            return IMT_8;
        case PZI_LOAD_IMMEDIATE_16:
            return IMT_16;
        case PZI_LOAD_IMMEDIATE_32:
            return IMT_32;
        case PZI_LOAD_IMMEDIATE_64:
            return IMT_64;
        case PZI_LOAD_IMMEDIATE_DATA:
            return IMT_DATA_REF;
        case PZI_CALL:
            return IMT_CODE_REF;
        case PZI_RETURN:
            return IMT_NONE;
    }
    abort();
}

unsigned
pz_code_immediate_size(enum immediate_type imt)
{
    switch (imt) {
        case IMT_NONE:
            return 0;
        case IMT_8:
            return ROUND_UP(1, MACHINE_WORD_SIZE)/MACHINE_WORD_SIZE;
        case IMT_16:
            return ROUND_UP(2, MACHINE_WORD_SIZE)/MACHINE_WORD_SIZE;
        case IMT_32:
            return ROUND_UP(4, MACHINE_WORD_SIZE)/MACHINE_WORD_SIZE;
        case IMT_64:
            return ROUND_UP(8, MACHINE_WORD_SIZE)/MACHINE_WORD_SIZE;
        case IMT_DATA_REF:
        case IMT_CODE_REF:
            return MACHINE_WORD_SIZE;
    }
    abort();
}

unsigned
pz_code_instr_size(opcode opcode)
{
    return MACHINE_WORD_SIZE;
}

#include <stdio.h>

void
pz_code_write_instr(uint8_t* proc, unsigned offset, opcode opcode)
{
    fprintf(stderr, "NIY\n");
    abort();
}

void
pz_code_write_imm8(uint8_t* proc, unsigned offset, uint8_t val)
{
    *((uintptr_t*)(&proc[offset])) = (uintptr_t)val;
}

void
pz_code_write_imm16(uint8_t* proc, unsigned offset, uint16_t val)
{
    *((uintptr_t*)(&proc[offset])) = (uintptr_t)val;
}

void
pz_code_write_imm32(uint8_t* proc, unsigned offset, uint32_t val)
{
    *((uintptr_t*)(&proc[offset])) = (uintptr_t)val;
}

void
pz_code_write_imm64(uint8_t* proc, unsigned offset, uint64_t val)
{
    *((uint64_t*)(&proc[offset])) = val;
}

void
pz_code_write_imm_word(uint8_t* proc, unsigned offset, uintptr_t val)
{
    *((uintptr_t*)(&proc[offset])) = val;
}

