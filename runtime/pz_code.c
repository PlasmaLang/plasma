/*
 * Plasma bytecode code structures and functions
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2015 Paul Bone
 * Distributed under the terms of the MIT license, see ../LICENSE.runtime
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "pz_common.h"
#include "pz_code.h"
#include "pz_run.h"
#include "pz_util.h"

pz_code* pz_code_init(unsigned num_imported_procs,
    imported_proc** imported_procs, unsigned num_procs)
{
    pz_code* code;

    code = malloc(sizeof(pz_code));
    code->imported_procs = imported_procs;
    code->num_imported_procs = num_imported_procs;
    code->procs = malloc(sizeof(uintptr_t*) * num_procs);
    memset(code->procs, 0, sizeof(uintptr_t*) * num_procs);
    code->num_procs = num_procs;
    code->total_size = 0;

    return code;
}

void pz_code_free(pz_code* code)
{
    for (unsigned i = 0; i < code->num_procs; i++) {
        if (code->procs[i] != NULL) {
            free(code->procs[i]);
        }
    }
    free(code->procs);

    free(code->imported_procs);

    free(code);
}

uint8_t*
pz_code_new_proc(uint32_t proc_size)
{
    return malloc(sizeof(uint8_t) * proc_size);
}

void*
pz_code_get_proc(pz_code* code, unsigned id)
{
    if (id < code->num_imported_procs) {
        return code->imported_procs[id]->proc;
    } else {
        return code->procs[id - code->num_imported_procs];
    }
}

bool
pz_code_proc_needs_ccall(pz_code* code, unsigned id)
{
    if (id < code->num_imported_procs) {
        return code->imported_procs[id]->type == BUILTIN_FOREIGN;
    } else {
        fprintf(stderr,
          "pz_code_proc_needs_ccall currently only works for imported procs");
        abort();
    }
}

