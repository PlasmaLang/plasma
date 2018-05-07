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

typedef struct PZ_Proc_Symbol_Struct {
    PZ_Import_Type  type;
    union {
        uint8_t     *bytecode;
        unsigned    (*c_func)(void *stack, unsigned sp);
    } proc;
    bool            need_free;
} PZ_Proc_Symbol;

void
pz_proc_symbol_free(void *proc);

typedef struct PZ_Proc_Struct PZ_Proc;

/*
 * Create a new proc.
 */
PZ_Proc *
pz_proc_init(unsigned size);

/*
 * Free the proc.
 */
void
pz_proc_free(PZ_Proc *proc);

uint8_t *
pz_proc_get_code(PZ_Proc *proc);

unsigned
pz_proc_get_size(PZ_Proc *proc);

#endif /* ! PZ_CODE_H */
