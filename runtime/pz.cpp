/*
 * Plasma in-memory representation
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2015-2016, 2018 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#include "pz_common.h"

#include <stdio.h>
#include <string.h>

#include "pz_code.h"
#include "pz_data.h"
#include "pz_interp.h"
#include "pz_radix_tree.h"

#include "pz.h"

#include "pz_radix_tree.template.h"

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
        delete pz->entry_module;
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

