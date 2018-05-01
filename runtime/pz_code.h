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

typedef struct PZ_Proc_Struct {
    unsigned code_offset;
    unsigned code_size;
} PZ_Proc;

/*
 * Create a new proc, the offset is the number of bytes from the start of
 * the code section.
 */
void
pz_proc_init(PZ_Proc *proc, unsigned offset, unsigned size);

#endif /* ! PZ_CODE_H */
