/*
 * Plasma bytecode exection
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2015-2016 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#ifndef PZ_RUN_H
#define PZ_RUN_H

#include "pz.h"
#include "pz_format.h"
#include "pz_instructions.h"

/*
 * Imported foreign builtins.
 *
 * The exact meaning of the parameters depends upon implementation details
 * within pz_run_*.c.
 *
 ******************************/

unsigned
builtin_print_func(void *stack, unsigned sp);

unsigned
builtin_int_to_string_func(void *stack, unsigned sp);

unsigned
builtin_setenv_func(void *stack, unsigned sp);

unsigned
builtin_free_func(void *stack, unsigned sp);

unsigned
builtin_gettimeofday_func(void *void_stack, unsigned sp);

unsigned
builtin_concat_string_func(void *stack, unsigned sp);

unsigned
builtin_die_func(void *stack, unsigned sp);

/*
 * The size of "fast" integers in bytes.
 */
extern const unsigned pz_fast_word_size;

/*
 * The number of tag bits made available by the runtime.
 * Guarenteed to match or exceed ptag_bits from src/core_to_pz.data.m
 */
extern const unsigned  pz_num_tag_bits;
extern const uintptr_t pz_tag_bits;

/*
 * Run the program.
 *
 ******************/

int
pz_run(PZ *pz);

/*
 * Build the raw code of the program.
 *
 ************************************/

/*
 * Write the instruction into the procedure at the given offset.
 * Returns the new offset within the procedure for the next instruction.
 * If proc is NULL then nothing is written but a new offset is computed,
 * this can be used in a first pass to calculate the required size of the
 * procedure.
 */
unsigned
pz_write_instr(uint8_t        *proc,
               unsigned        offset,
               Opcode          opcode,
               Width           width1,
               Width           width2,
               Immediate_Type  imm_type,
               Immediate_Value imm);

#endif /* ! PZ_RUN_H */
