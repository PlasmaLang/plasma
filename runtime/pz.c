/*
 * Plasma in-memory representation
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2015 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#include "pz_common.h"
#include "pz.h"
#include "pz_code.h"
#include "pz_data.h"
#include "pz_radix_tree.h"

/*
 * PZ Programs
 *************/

struct PZ_Struct {
    PZ_RadixTree                *modules;
    PZ_Module                   *entry_module;
};

PZ *
pz_init(void)
{
    PZ *pz;

    pz = malloc(sizeof(PZ));

    pz->modules = pz_radix_init();
    pz->entry_module = NULL;

    return pz;
}

void
pz_free(PZ *pz)
{
    pz_radix_free(pz->modules, (free_fn)pz_module_free);
    if (NULL != pz->entry_module) {
        pz_module_free(pz->entry_module);
    }
    free(pz);
}

void
pz_add_module(PZ *pz, const char *name, PZ_Module *module)
{
    pz_radix_insert(pz->modules, name, module);
}

PZ_Module *
pz_get_module(PZ *pz, const char *name)
{
    return pz_radix_lookup(pz->modules, name);
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

struct PZ_Module_Struct {
    struct PZ_Structs_Struct    *structs;
    struct PZ_Data_Struct       *data;
    struct PZ_Code_Struct       *code;

    PZ_RadixTree                *symbols;

    /*
     * TODO: Move this field to PZ_Struct
     */
    int32_t                     entry_proc;
};

PZ_Module *
pz_module_init_empty(void)
{
    PZ_Module *module;

    module = malloc(sizeof(PZ_Module));
    module->structs = NULL;
    module->data = NULL;
    module->code = NULL;
    module->symbols = NULL;
    module->entry_proc = -1;

    return module;
}

PZ_Module *
pz_module_init_loaded(PZ_Structs *structs, PZ_Data *data, PZ_Code *code,
        int32_t entry_proc)
{
    PZ_Module *module;

    module = malloc(sizeof(PZ_Module));
    module->structs = structs;
    module->data = data;
    module->code = code;
    module->symbols = NULL;
    module->entry_proc = entry_proc;

    return module;
}

void pz_module_free(PZ_Module *module)
{
    if (module->structs) {
        pz_structs_free(module->structs);
    }
    if (module->data) {
        pz_data_free(module->data);
    }
    if (module->code) {
        pz_code_free(module->code);
    }
    if (module->symbols) {
        pz_radix_free(module->symbols, NULL);
    }
    free(module);
}

int32_t
pz_module_get_entry_proc(PZ_Module *module)
{
    return module->entry_proc;
}

PZ_Code *
pz_module_get_code(PZ_Module *module)
{
    return module->code;
}

void
pz_module_add_proc_symbol(PZ_Module *module, const char *name,
        PZ_Proc_Symbol *proc)
{
    if (NULL == module->symbols) {
        module->symbols = pz_radix_init();
    }

    pz_radix_insert(module->symbols, name, proc);
}

PZ_Proc_Symbol *
pz_module_lookup_proc(PZ_Module *module, const char *name)
{
    if (NULL == module->symbols) {
        return NULL;
    } else {
        return pz_radix_lookup(module->symbols, name);
    }
}

