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
    entry_module_(nullptr), heap_(nullptr) {}

PZ::~PZ() {
    if (heap_) {
        pz_gc_free(heap_);
    }
}

bool
PZ::init()
{
    assert(!heap_);
    heap_ = pz_gc_init();

    return heap_ != nullptr;
}

void
PZ::add_module(const std::string &name, Module *module)
{
    modules.insert(name, module);
}

Module *
PZ::lookup_module(const std::string &name)
{
    return modules.lookup(name).value();
}

void
PZ::add_entry_module(Module *module)
{
    assert(nullptr == entry_module_);
    entry_module_ = std::unique_ptr<pz::Module>(module);
}

} // namespace pz

