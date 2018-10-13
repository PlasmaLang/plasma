/*
 * Plasma bytecode exection
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2015-2016, 2018 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#ifndef PZ_RUN_H
#define PZ_RUN_H

#include "pz.h"
#include "pz_format.h"
#include "pz_gc.h"
#include "pz_instructions.h"

#ifdef __cplusplus
extern "C" {
#endif

/*
 * Imported foreign builtins.
 *
 * The exact meaning of the parameters depends upon implementation details
 * within pz_run_*.c.
 *
 ******************************/

typedef struct PZ_Closure_S PZ_Closure;

unsigned
builtin_print_func(void *stack, unsigned sp, PZ_Heap *heap);

unsigned
builtin_int_to_string_func(void *stack, unsigned sp, PZ_Heap *heap);

unsigned
builtin_setenv_func(void *stack, unsigned sp, PZ_Heap *heap);

unsigned
builtin_gettimeofday_func(void *void_stack, unsigned sp, PZ_Heap *heap);

unsigned
builtin_concat_string_func(void *stack, unsigned sp, PZ_Heap *heap);

unsigned
builtin_die_func(void *stack, unsigned sp, PZ_Heap *heap);

unsigned
builtin_set_parameter_func(void *stack, unsigned sp, PZ_Heap *heap);

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
 *
 * If the immediate value needs extending to the operation width it will be
 * zero-extended.
 */
unsigned
pz_write_instr(uint8_t           *proc,
               unsigned           offset,
               PZ_Opcode          opcode,
               PZ_Width           width1,
               PZ_Width           width2,
               PZ_Immediate_Type  imm_type,
               PZ_Immediate_Value imm);

PZ_Closure *
pz_init_closure(uint8_t *code, void *data);

void
pz_closure_free(PZ_Closure *closure);

#ifdef __cplusplus
} // extern "C"
#endif

#endif /* ! PZ_RUN_H */
