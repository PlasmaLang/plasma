/*
 * Plasma in-memory representation
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2015-2016, 2018-2019 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#include "pz_common.h"

#include <stdio.h>
#include <string.h>

#include "pz_code.h"
#include "pz_data.h"
#include "pz_interp.h"

#include "pz.h"

#include "pz_gc.impl.h" // required for Heap destructor.

namespace pz {

/*
 * PZ Programs
 *************/

PZ::PZ(const Options &options) :
        m_options(options),
        m_entry_module(nullptr),
        m_heap(new Heap(options, *this))
{
    set_heap(heap());
}

PZ::~PZ() {
    for (auto module : m_modules) {
        delete module.second;
    }
}

bool
PZ::init()
{
    if (!heap()->init()) return false;

    return true;
}

bool
PZ::finalise()
{
    return heap()->finalise();
}

Module *
PZ::new_module(const std::string &name)
{
    assert(!m_modules[name]);
    // TODO: Maybe make this GC allocated.
    m_modules[name] = new Module(heap());
    return m_modules[name];
}

void
PZ::add_module(const std::string &name, Module *module)
{
    assert(!m_modules[name]);
    m_modules[name] = module;
}

Module *
PZ::lookup_module(const std::string &name)
{
    auto iter = m_modules.find(name);

    if (iter != m_modules.end()) {
        return iter->second;
    } else {
        return nullptr;
    }
}

void
PZ::add_entry_module(Module *module)
{
    assert(nullptr == m_entry_module);
    m_entry_module = std::unique_ptr<pz::Module>(module);
}

void
PZ::do_trace(HeapMarkState *marker) const
{
    for (auto m : m_modules) {
        m.second->do_trace(marker);
    }
    if (m_entry_module) {
        m_entry_module->do_trace(marker);
    }
}

} // namespace pz

