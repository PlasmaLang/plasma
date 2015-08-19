/*
 * Plasma bytecode code structures and functions
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2015 Paul Bone
 * Distributed under the terms of the MIT license, see ../LICENSE.runtime
 */

#ifndef PZ_CODE_H
#define PZ_CODE_H

#include "pz_format.h"

/*
 * Code layout in memory
 *
 *************************/

struct pz_code {
    uint_fast32_t   num_procs;
    uint_fast32_t   *proc_offsets;

    /* Total size in words */
    uint_fast32_t   total_size;
    uintptr_t       *code;
};

typedef struct pz_code pz_code;

pz_code* pz_code_init(uint_fast32_t num_procs);

void pz_code_free(pz_code* code);

/*
 * Set the size of a procedure.  Procedure sizes must be set in order as
 * internally they are represented as offsets from one-another.
 */
void pz_code_set_proc_size(pz_code* code, uint_fast32_t proc_num,
    uint_fast32_t size);

/*
 * Instruction encoding
 *
 *************************/

/*
 * Get the in-memory size of any immediate value following the instruction
 * opcde, 0 if there is none.  Measured in machine words.
 */
uint_fast32_t pz_code_immediate_size(opcode opcode);

/*
 * Get the on-disk size of any immediate value following the instruction
 * opcde, 0 if there is none.
 */
uint_fast32_t pz_code_immediate_encoded_size(opcode opcode);

#endif /* ! PZ_CODE_H */
