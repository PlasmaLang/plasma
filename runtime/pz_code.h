/*
 * Plasma bytecode code structures and functions
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2015-2016 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#ifndef PZ_CODE_H
#define PZ_CODE_H

/*
 * Code layout in memory
 *
 *************************/

typedef enum {
    PZ_BUILTIN_BYTECODE,
    PZ_BUILTIN_C_FUNC
} PZ_Import_Type;

typedef struct {
    PZ_Import_Type  type;
    union {
        uint8_t     *bytecode;
        unsigned    (*c_func)(void *stack, unsigned sp);
    } proc;
} PZ_Proc_Symbol;

typedef struct PZ_Proc_Struct {
    unsigned        code_offset;
    unsigned        code_size;
} PZ_Proc;

typedef struct PZ_Code_Struct PZ_Code;

PZ_Code *
pz_code_init(unsigned num_procs);

void
pz_code_free(PZ_Code *code);

unsigned
pz_code_num_procs(PZ_Code *code);

unsigned
pz_code_total_size(PZ_Code *code);

/*
 * Create a new proc, the offset is the number of bytes from the start of
 * the code section.
 */
PZ_Proc *
pz_code_new_proc(PZ_Code *code, unsigned i, unsigned offset,
    unsigned size);

PZ_Proc *
pz_code_get_proc(PZ_Code *code, unsigned num);

/*
 * Allocate the memory to store all the procedures.
 */
uint8_t *
pz_code_allocate_memory(unsigned size, PZ_Code *code);

/*
 * Return a pointer to the procedure with the given ID.
 */
uint8_t *
pz_code_get_proc_code(PZ_Code *code, unsigned id);

#endif /* ! PZ_CODE_H */
