/*
 * Plasma in-memory representation
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2015-2016, 2018 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#include "pz_common.h"

#include "pz.h"
#include "pz_code.h"
#include "pz_data.h"
#include "pz_radix_tree.h"
#include "pz_radix_tree.template.h"
#include "pz_run.h"

#include <stdio.h>
#include <string.h>

/*
 * PZ Programs
 *************/

struct PZ_S {
    pz::RadixTree<PZ_Module*> *modules;
    PZ_Module                 *entry_module;
};

PZ *
pz_init(void)
{
    PZ *pz;

    pz = malloc(sizeof(PZ));

    pz->modules = new pz::RadixTree<PZ_Module*>();
    pz->entry_module = NULL;

    return pz;
}

void
pz_free(PZ *pz)
{
    delete pz->modules;

    if (NULL != pz->entry_module) {
        pz_module_free(pz->entry_module);
    }
    free(pz);
}

void
pz_add_module(PZ *pz, const char *name, PZ_Module *module)
{
    pz->modules->insert(name, module);
}

PZ_Module *
pz_get_module(PZ *pz, const char *name)
{
    return pz->modules->lookup(name).value();
}

void
pz_add_entry_module(PZ *pz, PZ_Module *module)
{
    assert(!(pz->entry_module));

    pz->entry_module = module;
}

PZ_Module *
pz_get_entry_module(PZ *pz)
{
    return pz->entry_module;
}


/*
 * PZ Modules
 ************/

struct PZ_Module_S {
    unsigned      num_structs;
    PZ_Struct    *structs;

    unsigned      num_datas;
    void        **data;

    PZ_Proc     **procs;
    unsigned      num_procs;
    unsigned      total_code_size;

    PZ_Closure  **closures;
    unsigned      num_closures;

    PZ_Closure  **exports;
    unsigned      num_exports;
    unsigned      next_export;
    pz::RadixTree<unsigned> *symbols;

    int32_t       entry_closure;
};

PZ_Module *
pz_module_init(unsigned num_structs,
               unsigned num_data,
               unsigned num_procs,
               unsigned num_closures,
               unsigned num_exports,
               int entry_closure)
{
    PZ_Module *module;

    module = malloc(sizeof(PZ_Module));
    module->num_structs = num_structs;
    if (num_structs > 0) {
        module->structs = malloc(sizeof(PZ_Struct) * num_structs);
        memset(module->structs, 0, sizeof(PZ_Struct) * num_structs);
    } else {
        module->structs = NULL;
    }

    module->num_datas = num_data;
    if (num_data > 0) {
        module->data = malloc(sizeof(int8_t *) * num_data);
        memset(module->data, 0, sizeof(uint8_t *) * num_data);
    } else {
        module->data = NULL;
    }

    if (num_procs > 0) {
        module->procs = malloc(sizeof(PZ_Proc*) * num_procs);
        memset(module->procs, 0, sizeof(PZ_Proc*) * num_procs);
    } else {
        module->procs = NULL;
    }
    module->num_procs = num_procs;
    module->total_code_size = 0;

    module->num_closures = num_closures;
    if (num_closures > 0) {
        module->closures = malloc(sizeof(PZ_Closure*) * num_closures);
        memset(module->closures, 0, sizeof(PZ_Closure*) * num_closures);
    } else {
        module->closures = NULL;
    }

    module->symbols = NULL;

    module->num_exports = num_exports;
    module->next_export = 0;
    if (num_exports > 0) {
        module->exports = malloc(sizeof(PZ_Closure*) * num_exports);
        memset(module->exports, 0, sizeof(PZ_Closure*) * num_exports);
    } else {
        module->exports = NULL;
    }

    module->entry_closure = entry_closure;

    return module;
}

void
pz_module_free(PZ_Module *module)
{
    unsigned i;

    if (module->structs != NULL) {
        for (i = 0; i < module->num_structs; i++) {
            pz_struct_free(&(module->structs[i]));
        }
        free(module->structs);
    }

    if (module->data != NULL) {
        for (unsigned i = 0; i < module->num_datas; i++) {
            if (module->data[i] != NULL) {
                pz_data_free(module->data[i]);
            }
        }
        free(module->data);
    }

    if (module->procs != NULL) {
        for (unsigned i = 0; i < module->num_procs; i++) {
            if (module->procs[i]) {
                pz_proc_free(module->procs[i]);
            }
        }

        free(module->procs);
    }

    if (module->closures != NULL) {
        for (unsigned i = 0; i < module->num_closures; i++) {
            if (module->closures[i]) {
                pz_closure_free(module->closures[i]);
            }
        }

        free(module->closures);
    }

    if (module->symbols != NULL) {
        delete module->symbols;
    }
    if (module->exports != NULL) {
        // Don't free individual exports since they are in the closures
        // array above.
        free(module->exports);
    }

    free(module);
}

PZ_Struct *
pz_module_get_struct(PZ_Module *module, unsigned id)
{
    return &(module->structs[id]);
}

void
pz_module_set_data(PZ_Module *module, unsigned id, void *data)
{
    module->data[id] = data;
}

void *
pz_module_get_data(PZ_Module *module, unsigned id)
{
    return module->data[id];
}

void
pz_module_set_proc(PZ_Module *module, unsigned id, PZ_Proc *proc)
{
    assert(NULL == module->procs[id]);
    module->procs[id] = proc;
    module->total_code_size += pz_proc_get_size(proc);
}

PZ_Proc *
pz_module_get_proc(PZ_Module *module, unsigned id)
{
    return module->procs[id];
}

int32_t
pz_module_get_entry_closure(PZ_Module *module)
{
    return module->entry_closure;
}

PZ_Closure *
pz_module_get_closure(PZ_Module *module, unsigned id)
{
    return module->closures[id];
}

void
pz_module_set_closure(PZ_Module *module, unsigned id, PZ_Closure *closure)
{
    module->closures[id] = closure;
}

void
pz_module_add_symbol(PZ_Module     *module,
                     const char    *name,
                     PZ_Closure    *closure)
{
    if (NULL == module->symbols) {
        module->symbols = new pz::RadixTree<unsigned>();
    }
    unsigned id = module->next_export++;
    module->symbols->insert(name, id + 1);
    module->exports[id] = closure;
}

int
pz_module_lookup_symbol(PZ_Module *module, const char *name)
{
    if (NULL == module->symbols) {
        return -1;
    } else {
        Optional<unsigned> result = module->symbols->lookup(name);
        if (result.hasValue()) {
            return int(result.value()) - 1;
        } else {
            return -1;
        }
    }
}

PZ_Closure **
pz_module_get_exports(PZ_Module *module)
{
    return module->exports;
}

uint8_t *
pz_module_get_proc_code(PZ_Module *module, unsigned id)
{
    assert(id < module->num_procs);

    return pz_proc_get_code(module->procs[id]);
}

void
pz_module_print_loaded_stats(PZ_Module *module)
{
    printf("Loaded %d procedures with a total of %d bytes.\n",
           module->num_procs, module->total_code_size);
}

