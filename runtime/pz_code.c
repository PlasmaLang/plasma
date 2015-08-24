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
#include "pz_run.h"
#include "pz_util.h"

pz_code* pz_code_init(unsigned num_imported_procs,
    ccall_func* imported_procs, unsigned num_procs)
{
    pz_code* code;

    code = malloc(sizeof(pz_code));
    code->imported_procs = imported_procs;
    code->num_imported_procs = num_imported_procs;
    code->procs = malloc(sizeof(uintptr_t*) * num_procs);
    memset(code->procs, 0, sizeof(uintptr_t*) * num_procs);
    code->num_procs = num_procs;
    code->total_size = 0;

    return code;
}

void pz_code_free(pz_code* code)
{
    for (unsigned i = 0; i < code->num_procs; i++) {
        if (code->procs[i] != NULL) {
            free(code->procs[i]);
        }
    }
    free(code->procs);

    free(code->imported_procs);

    free(code);
}

uint8_t*
pz_code_new_proc(uint32_t proc_size)
{
    return malloc(sizeof(uint8_t) * proc_size);
}

void*
pz_code_get_proc(pz_code* code, unsigned id)
{
    if (id < code->num_imported_procs) {
        return code->imported_procs[id];
    } else {
        return code->procs[id - code->num_imported_procs];
    }
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
        case PZI_CCALL:
            /*
             * Not really a code reference, but this type has the correct
             * size
             */
            return IMT_CODE_REF;
        case PZI_RETURN:
        case PZI_END:
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
pz_code_instr_size(opcode opcode)
{
    return 1;
}

void
pz_code_write_instr(uint8_t* proc, unsigned offset, opcode opcode)
{
    *((uint8_t*)(&proc[offset])) = pz_instruction_words[opcode];
}

void
pz_code_write_imm8(uint8_t* proc, unsigned offset, uint8_t val)
{
    *((uint8_t*)(&proc[offset])) = (uintptr_t)val;
}

void
pz_code_write_imm16(uint8_t* proc, unsigned offset, uint16_t val)
{
    *((uint16_t*)(&proc[offset])) = (uintptr_t)val;
}

void
pz_code_write_imm32(uint8_t* proc, unsigned offset, uint32_t val)
{
    *((uint32_t*)(&proc[offset])) = (uintptr_t)val;
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

