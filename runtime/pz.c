/*
 * Plasma bytecode in-memory representation
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
pz_module_init(void)
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

