/*
 * Plasma bytecode code structures and functions
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2015 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#include <string.h>

#include "pz_common.h"
#include "pz_code.h"

struct PZ_Code_Struct {
    uint8_t             *code;
    PZ_Proc             **procs;
    unsigned            num_procs;

    /* Total size in words */
    uint_fast32_t       total_size;
};

PZ_Code *
pz_code_init(unsigned num_procs)
{
    PZ_Code *code;

    code = malloc(sizeof(struct PZ_Code_Struct));

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

    free(code);
}

unsigned
pz_code_num_procs(PZ_Code *code)
{
    return code->num_procs;
}

unsigned
pz_code_total_size(PZ_Code *code)
{
    return code->total_size;
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

PZ_Proc *
pz_code_get_proc(PZ_Code *code, unsigned i)
{
    return code->procs[i];
}

uint8_t *
pz_code_allocate_memory(unsigned size, PZ_Code *code)
{
    code->total_size = size;
    code->code = malloc(code->total_size);

    return code->code;
}

uint8_t *
pz_code_get_proc_code(PZ_Code *code, unsigned id)
{
    PZ_Proc *proc;

    assert(id < code->num_procs);

    if (code->procs) {
        proc = code->procs[id];
        if (proc != NULL) {
            return &code->code[proc->code_offset];
        } else {
            return NULL;
        }
    } else {
        return NULL;
    }
}

