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

#include "pz.h"

namespace pz {

static void
static_trace_for_gc(PZ_Heap_Mark_State *marker, void *pz);

/*
 * PZ Programs
 *************/

PZ::PZ(const Options &options_) :
    options(options_), entry_module_(nullptr), heap_(nullptr) {}

PZ::~PZ() {
    delete heap_;
}

bool
PZ::init()
{
    assert(!heap_);
    heap_ = new Heap(options, static_trace_for_gc, this);
    if (!heap_->init()) return false;

    return true;
}

bool
PZ::finalise()
{
    return heap_->finalise();
}

void
PZ::add_module(const std::string &name, Module *module)
{
    modules[name] = module;
}

Module *
PZ::lookup_module(const std::string &name)
{
    auto iter = modules.find(name);

    if (iter != modules.end()) {
        return iter->second;
    } else {
        return nullptr;
    }
}

void
PZ::add_entry_module(Module *module)
{
    assert(nullptr == entry_module_);
    entry_module_ = std::unique_ptr<pz::Module>(module);
}

static void
static_trace_for_gc(PZ_Heap_Mark_State *marker, void *pz)
{
    ((const PZ*)pz)->trace_for_gc(marker);
}

void
PZ::trace_for_gc(PZ_Heap_Mark_State *marker) const
{
    for (auto m : modules) {
        m.second->trace_for_gc(marker);
    }
    if (entry_module_) {
        entry_module_->trace_for_gc(marker);
    }
}

} // namespace pz

