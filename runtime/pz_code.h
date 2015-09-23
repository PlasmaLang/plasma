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

typedef struct PZ_Code_Struct {
    Imported_Proc   **imported_procs;
    unsigned        num_imported_procs;

    uint8_t         **procs;
    unsigned        num_procs;

    /* Total size in words */
    uint_fast32_t   total_size;
} PZ_Code;

PZ_Code *pz_code_init(unsigned num_imported_procs,
    Imported_Proc **imported_procs, unsigned num_procs);

void pz_code_free(PZ_Code *code);

/*
 * Create a new proc, the size is in bytes.
 */
uint8_t *pz_code_new_proc(uint32_t proc_size);

/*
 * Return a pointer to the procedure with the given ID.
 */
void *pz_code_get_proc(PZ_Code *code, unsigned id);

/*
 * Return true if the given procedure needs a CCALL rather than a CALL
 * instruction.
 */
bool pz_code_proc_needs_ccall(PZ_Code *code, unsigned id);

#endif /* ! PZ_CODE_H */
