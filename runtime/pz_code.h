/*
 * Plasma bytecode code structures and functions
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2015 Paul Bone
 * Distributed under the terms of the MIT license, see ../LICENSE.runtime
 */

#ifndef PZ_CODE_H
#define PZ_CODE_H

#include "pz_instructions.h"
#include "pz_run.h"

/*
 * Code layout in memory
 *
 *************************/

struct pz_code {
    ccall_func*     imported_procs;
    unsigned        num_imported_procs;

    uint8_t**       procs;
    unsigned        num_procs;

    /* Total size in words */
    uint_fast32_t   total_size;
};

typedef struct pz_code pz_code;

pz_code* pz_code_init(unsigned num_imported_procs,
    ccall_func* imported_procs, unsigned num_procs);

void pz_code_free(pz_code* code);

/*
 * Create a new proc, the size is in bytes.
 */
uint8_t*
pz_code_new_proc(uint32_t proc_size);

/*
 * Return a pointer to the procedure with the given ID.
 */
void*
pz_code_get_proc(pz_code* code, unsigned id);

/*
 * Instruction encoding
 *
 *************************/

enum immediate_type {
    IMT_NONE,
    IMT_8,
    IMT_16,
    IMT_32,
    IMT_64,
    IMT_CODE_REF,
    IMT_DATA_REF
};

/*
 * Get the immediate type following the instruction opcode.
 */
enum immediate_type
pz_code_immediate(opcode opcode);

/*
 * Get the in-memory size of the immediate value.
 */
unsigned
pz_code_immediate_size(enum immediate_type imm);

/*
 * Return the size of the given instruction, exlucing any immediate value.
 */
unsigned
pz_code_instr_size(opcode opcode);

/*
 * Write the instruction into the procedure at the given offset.
 */
void
pz_code_write_instr(uint8_t* proc, unsigned offset, opcode opcode);

/*
 * Write the immediate value (of various sizes) into the procedure at the
 * given offset.
 */
void
pz_code_write_imm8(uint8_t* proc, unsigned offset, uint8_t val);

void
pz_code_write_imm16(uint8_t* proc, unsigned offset, uint16_t val);

void
pz_code_write_imm32(uint8_t* proc, unsigned offset, uint32_t val);

void
pz_code_write_imm64(uint8_t* proc, unsigned offset, uint64_t val);

/*
 * TODO: Handle relative addressing and maybe PIC.
 */
void
pz_code_write_imm_word(uint8_t* proc, unsigned offset, uintptr_t val);

#endif /* ! PZ_CODE_H */
