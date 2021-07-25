/*
 * Plasma bytecode exection
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2015-2016, 2018-2019, 2021 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#ifndef PZ_INTERP_H
#define PZ_INTERP_H

#include "pz.h"
#include "pz_format.h"
#include "pz_gc.h"
#include "pz_instructions.h"
#include "pz_option.h"

/*
 * Run the program.
 *
 ******************/

namespace pz {

int run(PZ & pz, const Options & options);

/*
 * Imported foreign builtins.
 *
 * The exact meaning of the parameters depends upon implementation details
 * within pz_run_*.c.
 *
 ******************************/

typedef unsigned (*pz_builtin_c_func)(void * stack, unsigned sp);

typedef unsigned (*pz_builtin_c_alloc_func)(void * stack, unsigned sp,
                                            AbstractGCTracer & gc_trace);

typedef unsigned (*pz_builtin_c_special_func)(void * stack, unsigned sp,
                                              PZ & pz);

unsigned pz_builtin_print_func(void * stack, unsigned sp);

unsigned pz_builtin_readline_func(void * stack, unsigned sp,
                                  AbstractGCTracer & gc_trace);

unsigned pz_builtin_int_to_string_func(void * stack, unsigned sp,
                                       AbstractGCTracer & gc_trace);

unsigned pz_builtin_setenv_func(void * stack, unsigned sp);

unsigned pz_builtin_gettimeofday_func(void * void_stack, unsigned sp);

unsigned pz_builtin_string_concat_func(void * stack, unsigned sp,
                                       AbstractGCTracer & gc_trace);

unsigned pz_builtin_die_func(void * stack, unsigned sp);

unsigned pz_builtin_set_parameter_func(void * stack, unsigned sp, PZ & pz);

unsigned pz_builtin_get_parameter_func(void * stack, unsigned sp, PZ & pz);

unsigned pz_builtin_char_class(void * stack, unsigned sp);
unsigned pz_builtin_strpos_forward(void * stack, unsigned sp,
        AbstractGCTracer & gc);
unsigned pz_builtin_strpos_backward(void * stack, unsigned sp,
        AbstractGCTracer & gc);
unsigned pz_builtin_strpos_next_char(void * stack, unsigned sp,
        AbstractGCTracer & gc);
unsigned pz_builtin_strpos_prev_char(void * stack, unsigned sp,
        AbstractGCTracer & gc);
unsigned pz_builtin_string_begin(void * stack, unsigned sp,
        AbstractGCTracer & gc);
unsigned pz_builtin_string_end(void * stack, unsigned sp,
        AbstractGCTracer & gc);
unsigned pz_builtin_string_substring(void * stack, unsigned sp,
        AbstractGCTracer & gc);
unsigned pz_builtin_string_equals(void * stack, unsigned sp);

/*
 * The size of "fast" integers in bytes.
 */
extern const unsigned fast_word_size;

/*
 * The number of tag bits made available by the runtime.
 * Guarenteed to match or exceed ptag_bits from src/core_to_pz.data.m
 */
extern const unsigned  num_tag_bits;
extern const uintptr_t tag_bits;

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
write_instr(uint8_t           *proc,
            unsigned           offset,
            PZ_Opcode          opcode);

unsigned
write_instr(uint8_t           *proc,
            unsigned           offset,
            PZ_Opcode          opcode,
            ImmediateType      imm_type,
            ImmediateValue     imm);

unsigned
write_instr(uint8_t           *proc,
            unsigned           offset,
            PZ_Opcode          opcode,
            PZ_Width           width1);

unsigned
write_instr(uint8_t           *proc,
            unsigned           offset,
            PZ_Opcode          opcode,
            PZ_Width           width1,
            ImmediateType      imm_type,
            ImmediateValue     imm);

unsigned
write_instr(uint8_t           *proc,
            unsigned           offset,
            PZ_Opcode          opcode,
            PZ_Width           width1,
            PZ_Width           width2);

}

#endif /* ! PZ_INTERP_H */
