/*
 * Plasma in-memory representation
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2015 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#ifndef PZ_H
#define PZ_H

#include "pz_common.h"
#include "pz_code.h"
#include "pz_data.h"

typedef struct PZ_Struct PZ;

typedef struct PZ_Module_Struct PZ_Module;

/*
 * PZ Programs
 *************/

PZ *
pz_init(void);

void
pz_free(PZ *pz);

/*
 * Add a module to the program.
 *
 * The entry module is not added in this way.
 *
 * The name will be strdup'd and so the caller is responsible for freeing it
 * after this call. The module will be freed by pz_free().
 */
void
pz_add_module(PZ *pz, const char *name, PZ_Module *module);

PZ_Module *
pz_get_module(PZ *pz, const char *name);

void
pz_add_entry_module(PZ *pz, PZ_Module *module);

PZ_Module *
pz_get_entry_module(PZ *pz);

/*
 * PZ Modules
 ************/

PZ_Module *
pz_module_init_empty(void);

PZ_Module *
pz_module_init_loaded(PZ_Structs *structs, PZ_Data *data, PZ_Code *code,
        int32_t entry_proc);

void
pz_module_free(PZ_Module *module);

int32_t
pz_module_get_entry_proc(PZ_Module *module);

/*
 * Used while running the program and may be used during procedure calls.
 * Therefore this is a candidate for optimisation.
 */
PZ_Code *
pz_module_get_code(PZ_Module *module);

void
pz_module_add_proc_symbol(PZ_Module *module, const char *name,
        Imported_Proc *proc);

Imported_Proc *
pz_module_lookup_proc(PZ_Module *module, const char *name);

#endif /* ! PZ_H */
