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

void
pz_proc_symbol_free(void *proc_void)
{
    PZ_Proc_Symbol *proc = proc_void;

    if (proc->need_free) {
        switch (proc->type) {
            case PZ_BUILTIN_BYTECODE:
                free(proc->proc.bytecode);
                break;
            case PZ_BUILTIN_C_FUNC:
                break;
        }
        free(proc);
    }
}

void
pz_proc_init(PZ_Proc *proc, unsigned offset, unsigned size)
{
    proc->code_offset = offset;
    proc->code_size = size;
}

