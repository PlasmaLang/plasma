/*
 * Plasma in-memory representation (modules)
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2015-2016, 2018 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#ifndef PZ_MODULE_H
#define PZ_MODULE_H

#include "pz_common.h"

#include "pz_closure.h"
#include "pz_code.h"
#include "pz_data.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef struct PZ_Module_S PZ_Module;

PZ_Module *
pz_module_init(unsigned num_structs,
               unsigned num_data,
               unsigned num_procs,
               unsigned num_closures,
               unsigned num_exports,
               int entry_closure);

void
pz_module_free(PZ_Module *module);

PZ_Struct *
pz_module_get_struct(PZ_Module *module, unsigned struct_id);

void
pz_module_set_struct(PZ_Module *module, unsigned id, PZ_Struct *struct_);

void
pz_module_set_data(PZ_Module *module, unsigned id, void *data);

void *
pz_module_get_data(PZ_Module *module, unsigned id);

void
pz_module_set_proc(PZ_Module *module, unsigned id, PZ_Proc *proc);

PZ_Proc *
pz_module_get_proc(PZ_Module *module, unsigned id);

int32_t
pz_module_get_entry_closure(PZ_Module *module);

struct PZ_Closure_S *
pz_module_get_closure(PZ_Module *module, unsigned id);

void
pz_module_set_closure(PZ_Module           *module,
                      unsigned             id,
                      struct PZ_Closure_S *closure);

void
pz_module_add_symbol(PZ_Module           *module,
                     const char          *name,
                     struct PZ_Closure_S *closure);

/*
 * Returns the ID of the closure in the exports struct.  -1 if not found.
 */
int
pz_module_lookup_symbol(PZ_Module *module, const char *name);

struct PZ_Closure_S **
pz_module_get_exports(PZ_Module *module);

/*
 * Return a pointer to the code for the procedure with the given ID.
 */
uint8_t *
pz_module_get_proc_code(PZ_Module *module, unsigned id);

void
pz_module_print_loaded_stats(PZ_Module *module);

#ifdef __cplusplus
} // extern "C"
#endif

#endif // ! PZ_MODULE_H
