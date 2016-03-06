/*
 * Plasma bytecode code structures and functions
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2015 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "pz_common.h"
#include "pz_code.h"
#include "pz_run.h"
#include "pz_util.h"

PZ_Code *
pz_code_init(unsigned num_imported_procs,
    Imported_Proc **imported_procs, unsigned num_procs)
{
    PZ_Code *code;

    code = malloc(sizeof(struct PZ_Code_Struct));
    code->imported_procs = imported_procs;
    code->num_imported_procs = num_imported_procs;

    code->code = NULL;
    code->procs = malloc(sizeof(PZ_Proc*) * num_procs);
    memset(code->procs, 0, sizeof(PZ_Proc *) * num_procs);
    code->num_procs = num_procs;
    code->total_size = 0;

    return code;
}

void
pz_code_free(PZ_Code *code)
{
    for (unsigned i = 0; i < code->num_procs; i++) {
        if (code->procs[i] != NULL) {
            free(code->procs[i]);
        }
    }
    free(code->procs);

    if (code->code) {
        free(code->code);
    }

    free(code->imported_procs);

    free(code);
}

PZ_Proc *
pz_code_new_proc(PZ_Code *code, unsigned i, unsigned offset, unsigned size)
{
    PZ_Proc *proc;

    proc = malloc(sizeof(PZ_Proc));
    proc->code_offset = offset;
    proc->code_size = size;

    code->procs[i] = proc;

    return proc;
}

void
pz_code_allocate_memory(unsigned size, PZ_Code *code)
{
    code->total_size = size;
    code->code = malloc(code->total_size);
}

void*
pz_code_get_proc_code(PZ_Code *code, unsigned id)
{
    if (id < code->num_imported_procs) {
        return code->imported_procs[id]->proc;
    } else {
        PZ_Proc *proc;

        proc = code->procs[id - code->num_imported_procs];
        if (proc != NULL) {
            return &code->code[proc->code_offset];
        } else {
            return NULL;
        }
    }
}

bool
pz_code_proc_needs_ccall(PZ_Code *code, unsigned id)
{
    if (id < code->num_imported_procs) {
        return code->imported_procs[id]->type == BUILTIN_FOREIGN;
    } else {
        return false;
    }
}

