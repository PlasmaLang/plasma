/*
 * Plasma bytecode code structures and functions
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2015 Paul Bone
 * Distributed under the terms of the MIT license, see ../LICENSE.runtime
 */

#include <stdlib.h>

#include "pz_common.h"
#include "pz_code.h"
#include "pz_util.h"

pz_code* pz_code_init(uint_fast32_t num_procs)
{
    pz_code* code;

    code = malloc(sizeof(pz_code));
    code->num_procs = num_procs;
    code->proc_offsets = malloc(sizeof(uint_fast32_t) * num_procs);
    code->total_size = 0;
    code->code = NULL;

    return code;
}

void pz_code_free(pz_code* code)
{
    if (code->proc_offsets != NULL) {
        free(code->proc_offsets);
    }
    if (code->code) {
        free(code->code);
    }

    free(code);
}

void pz_code_set_proc_size(pz_code* code, uint_fast32_t proc_num,
    uint_fast32_t size)
{
    uint_fast32_t offset;

    offset = code->proc_offsets[proc_num] + size;
    if (proc_num == (code->num_procs - 1)) {
        code->total_size = offset;
    } else {
        code->proc_offsets[proc_num + 1] = offset;
    }
}

/*
 * Instruction encoding
 *
 *************************/

uint_fast32_t pz_code_immediate_size(opcode opcode)
{
    switch (opcode) {
        case PZI_LOAD_IMMEDIATE_8:
            return ROUND_UP(1, MACHINE_WORD_SIZE);
        case PZI_LOAD_IMMEDIATE_16:
            return ROUND_UP(2, MACHINE_WORD_SIZE);
        case PZI_LOAD_IMMEDIATE_32:
            return ROUND_UP(4, MACHINE_WORD_SIZE);
        case PZI_LOAD_IMMEDIATE_64:
            return ROUND_UP(8, MACHINE_WORD_SIZE);
        case PZI_LOAD_IMMEDIATE_MWORD:
        case PZI_LOAD_IMMEDIATE_DATA:
        case PZI_CALL:
            return MACHINE_WORD_SIZE;
    }
    abort();
}

