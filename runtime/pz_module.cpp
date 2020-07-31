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
#include <utility>

#include "pz_closure.h"
#include "pz_util.h"

#include "pz_module.h"

namespace pz {

/*
 * Export class
 ***************/

Export::Export(pz::Closure *closure, unsigned export_id) :
    m_closure(closure),
    m_export_id(export_id) {}

/*
 * ModuleLoading class
 **********************/

ModuleLoading::ModuleLoading(unsigned num_structs,
                             unsigned num_data,
                             unsigned num_procs,
                             unsigned num_closures,
                             NoGCScope &no_gc) :
        AbstractGCTracer(no_gc.heap()),
        m_total_code_size(0),
        m_next_export(0)
{
    m_structs.reserve(num_structs);
    m_datas.reserve(num_data);
    m_procs.reserve(num_procs);
    m_closures.reserve(num_closures);
    for (unsigned i = 0; i < num_closures; i++) {
        m_closures.push_back(new(no_gc) Closure());
    }
}

Struct *
ModuleLoading::new_struct(unsigned num_fields, const GCCapability &gc_cap)
{
    NoGCScope nogc(&gc_cap);

    Struct *struct_ = new(nogc) Struct(nogc, num_fields);
    if (nogc.is_oom()) return nullptr;

    m_structs.push_back(struct_);
    return struct_;
}

void
ModuleLoading::add_data(void *data)
{
    m_datas.push_back(data);
}

Proc *
ModuleLoading::new_proc(unsigned size, bool is_builtin,
        const GCCapability &gc_cap)
{
    // Either the proc object, or the code area within it are untracable
    // while the proc is constructed.
    NoGCScope no_gc(&gc_cap);

    Proc *proc = new(no_gc) Proc(no_gc, nullptr, is_builtin, size);
    if (no_gc.is_oom()) return nullptr;

    m_procs.push_back(proc);
    m_total_code_size += proc->size();
    return proc;
}

void
ModuleLoading::add_symbol(const std::string &name, Closure *closure)
{
    unsigned id = m_next_export++;
    m_symbols.insert(make_pair(name, Export(closure, id)));
}

Optional<unsigned>
ModuleLoading::lookup_symbol(const std::string &name) const
{
    auto iter = m_symbols.find(name);

    if (iter != m_symbols.end()) {
        return iter->second.id();
    } else {
        return Optional<unsigned>::Nothing();
    }
}

void
ModuleLoading::print_loaded_stats() const
{
    printf("Loaded %d procedures with a total of %d bytes.\n",
           num_procs(), m_total_code_size);
}

void
ModuleLoading::do_trace(HeapMarkState *marker) const
{
    /*
     * This is needed in case we GC during loading, we want to keep this
     * module until we know we're done loading it.
     */
    for (Struct *s : m_structs) {
        marker->mark_root(s);
    }

    for (void *d : m_datas) {
        marker->mark_root(d);
    }

    for (void *p : m_procs) {
        marker->mark_root(p);
    }

    for (void *c : m_closures) {
        marker->mark_root(c);
    }

    for (auto symbol : m_symbols) {
        marker->mark_root(symbol.second.closure());
    }
}


/*
 * Module class
 ***************/

Module::Module(Heap *heap) :
    AbstractGCTracer(heap),
    m_entry_closure(nullptr) {}

Module::Module(Heap *heap, ModuleLoading &loading) :
    AbstractGCTracer(heap),
    m_symbols(loading.m_symbols),
    m_entry_closure(nullptr) {}

void
Module::add_symbol(const std::string &name, Closure *closure,
    unsigned export_id)
{
    m_symbols.insert(make_pair(name, Export(closure, export_id)));
}

Optional<Export>
Module::lookup_symbol(const std::string& name) const
{
    auto iter = m_symbols.find(name);

    if (iter != m_symbols.end()) {
        return iter->second;
    } else {
        return Optional<Export>::Nothing();
    }
}

void
Module::do_trace(HeapMarkState *marker) const
{
    for (auto symbol : m_symbols) {
        marker->mark_root(symbol.second.closure());
    }

    marker->mark_root(m_entry_closure);
}

} // namespace pz
