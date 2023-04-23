/*
 * Plasma in-memory representation
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#include "pz_common.h"

#include <stdio.h>
#include <string.h>
#include <utility>

#include "pz_closure.h"
#include "pz_util.h"

#include "pz_library.h"

namespace pz {

/*
 * LibraryLoading class
 **********************/

LibraryLoading::LibraryLoading(unsigned num_structs,
                               unsigned num_data, unsigned num_procs,
                               unsigned num_closures, NoGCScope & no_gc)
    : m_total_code_size(0)
{
    m_structs.reserve(num_structs);
    m_datas.reserve(num_data);
    m_procs.reserve(num_procs);
    m_closures.reserve(num_closures);
    for (unsigned i = 0; i < num_closures; i++) {
        m_closures.push_back(new (no_gc) Closure());
    }
}

Struct * LibraryLoading::new_struct(unsigned       num_fields,
                                    GCCapability & gc_cap)
{
    NoGCScope nogc(gc_cap);

    Struct * struct_ = new (nogc) Struct(nogc, num_fields);
    if (nogc.is_oom()) return nullptr;

    m_structs.push_back(struct_);
    return struct_;
}

void LibraryLoading::add_data(void * data)
{
    m_datas.push_back(data);
}

Proc * LibraryLoading::new_proc(String name, unsigned size, bool is_builtin,
                                GCCapability & gc_cap)
{
    // Either the proc object, or the code area within it are untracable
    // while the proc is constructed.
    NoGCScope no_gc(gc_cap);

    Proc * proc = new (no_gc) Proc(no_gc, name, is_builtin, size);
    if (no_gc.is_oom()) return nullptr;

    m_procs.push_back(proc);
    m_total_code_size += proc->size();
    return proc;
}

void LibraryLoading::add_symbol(String name, Closure * closure)
{
    m_symbols.insert(std::make_pair(name, closure));
}

void LibraryLoading::print_loaded_stats() const
{
    printf("Loaded %d procedures with a total of %d bytes.\n",
           num_procs(),
           m_total_code_size);
}

void LibraryLoading::do_trace(HeapMarkState * marker) const
{
    /*
     * This is needed in case we GC during loading, we want to keep this
     * module until we know we're done loading it.
     */
    for (Struct * s : m_structs) {
        marker->mark_root(s);
    }

    for (void * d : m_datas) {
        marker->mark_root(d);
    }

    for (void * p : m_procs) {
        marker->mark_root(p);
    }

    for (void * c : m_closures) {
        marker->mark_root(c);
    }

    for (auto symbol : m_symbols) {
        marker->mark_root(symbol.first.ptr());
        marker->mark_root(symbol.second);
    }
}

/*
 * Library class
 ***************/

Library::Library() : m_entry_closure(nullptr) {}

Library::Library(LibraryLoading & loading)
    : m_symbols(loading.m_symbols)
    , m_entry_closure(nullptr)
{}

void Library::add_symbol(String name, Closure * closure)
{
    m_symbols.insert(std::make_pair(name, closure));
}

Optional<Closure *> Library::lookup_symbol(String name) const
{
    auto iter = m_symbols.find(name);

    if (iter != m_symbols.end()) {
        return iter->second;
    } else {
        return Optional<Closure *>::Nothing();
    }
}

void Library::do_trace(HeapMarkState * marker) const
{
    for (auto symbol : m_symbols) {
        marker->mark_root(symbol.first.ptr());
        marker->mark_root(symbol.second);
    }

    marker->mark_root(m_entry_closure);
}

}  // namespace pz
