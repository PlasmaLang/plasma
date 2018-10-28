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

namespace pz {

/*
 * PZ Programs
 *************/

PZ::PZ() :
    modules(new RadixTree<pz::Module*>()),
    entry_module_(nullptr) {}

PZ::~PZ()
{
    delete modules;

    if (NULL != entry_module_) {
        delete entry_module_;
    }
}

void
PZ::add_module(const char *name, Module *module)
{
    modules->insert(name, module);
}

Module *
PZ::lookup_module(const char *name)
{
    return modules->lookup(name).value();
}

void
PZ::add_entry_module(Module *module)
{
    assert(nullptr == entry_module_);
    entry_module_ = module;
}

} // namespace pz

