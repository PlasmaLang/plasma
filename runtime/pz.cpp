/*
 * Plasma in-memory representation
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2015-2016, 2018-2020 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#include "pz_common.h"

#include <stdio.h>
#include <string.h>

#include "pz_code.h"
#include "pz_data.h"
#include "pz_interp.h"

#include "pz.h"

#include "pz_gc.impl.h"  // required for Heap destructor.

namespace pz {
/*
 * PZ Programs
 *************/

PZ::PZ(const Options & options)
    : m_options(options)
    , m_program(nullptr)
    , m_heap(new Heap(options, *this))
{
    set_heap(heap());
}

// Defined here rather than the header even though it's a default destructor
// so that it can access the heap destructor.
PZ::~PZ() {}

bool PZ::init()
{
    if (!heap()->init()) return false;

    return true;
}

bool PZ::finalise()
{
    return heap()->finalise();
}

Library * PZ::new_library(const std::string & name)
{
    assert(!m_libraries[name]);
    m_libraries[name] = new (*this) Library(name);
    return m_libraries[name];
}

void PZ::add_library(const std::string & name, Library * library)
{
    assert(!m_libraries[name]);
    m_libraries[name] = library;
}

Library * PZ::lookup_library(const std::string & name)
{
    auto iter = m_libraries.find(name);

    if (iter != m_libraries.end()) {
        return iter->second;
    } else {
        return nullptr;
    }
}

void PZ::add_program_lib(Library * program)
{
    assert(nullptr == m_program);
    m_program = program;
}

void PZ::do_trace(HeapMarkState * marker) const
{
    for (auto m : m_libraries) {
        marker->mark_root(m.second);
    }
    if (m_program) {
        marker->mark_root(m_program);
    }
}

}  // namespace pz
