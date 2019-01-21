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

#include "pz_closure.h"
#include "pz_util.h"

#include "pz_module.h"

namespace pz {

/*
 * ModuleLoading class
 **********************/

ModuleLoading::ModuleLoading() :
        m_total_code_size(0),
        m_next_export(0) {}

ModuleLoading::ModuleLoading(unsigned num_structs,
                             unsigned num_data,
                             unsigned num_procs,
                             unsigned num_closures,
                             unsigned num_exports) :
        m_total_code_size(0),
        m_next_export(0)
{
    m_structs.reserve(num_structs);
    m_datas.reserve(num_data);
    m_procs.reserve(num_procs);
    m_closures.reserve(num_closures);
    m_exports.reserve(num_exports);
}

Struct&
ModuleLoading::new_struct(unsigned num_fields)
{
    m_structs.emplace_back(num_fields);
    return m_structs.back();
}

void
ModuleLoading::add_data(void *data)
{
    m_datas.push_back(data);
}

Proc &
ModuleLoading::new_proc(Heap &heap, unsigned size)
{
    m_procs.emplace_back(&heap, *this, size);
    Proc &proc = m_procs.back();
    m_total_code_size += proc.size();
    return proc;
}

void
ModuleLoading::set_closure(PZ_Closure *closure)
{
    m_closures.push_back(closure);
}

void
ModuleLoading::add_symbol(const std::string &name, PZ_Closure *closure)
{
    unsigned id = m_next_export++;
    m_symbols[name] = id;
    m_exports.push_back(closure);
}

Optional<unsigned>
ModuleLoading::lookup_symbol(const std::string &name) const
{
    auto iter = m_symbols.find(name);

    if (iter != m_symbols.end()) {
        return iter->second;
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
ModuleLoading::do_trace(PZ_Heap_Mark_State *marker) const
{
    /*
     * This is needed in case we GC during loading, we want to keep this
     * module until we know we're done loading it.
     */
    for (void *d : m_datas) {
        pz_gc_mark_root(marker, d);
    }

    for (const Proc & p : m_procs) {
        pz_gc_mark_root(marker, p.code());
    }

    for (PZ_Closure *c : m_closures) {
        pz_gc_mark_root(marker, c);
    }

    for (PZ_Closure *e : m_exports) {
        pz_gc_mark_root(marker, e);
    }
}


/*
 * Module class
 ***************/

Module::Module() : entry_closure_(nullptr) {}

Module::Module(ModuleLoading &loading, PZ_Closure *entry_closure) :
    exports(loading.m_exports),
    symbols(loading.m_symbols),
    entry_closure_(entry_closure) {}

void
Module::add_symbol(const std::string &name, struct PZ_Closure_S *closure)
{
    exports.push_back(closure);
    symbols[name] = exports.size() - 1;
}

Optional<unsigned>
Module::lookup_symbol(const std::string& name) const
{
    auto iter = symbols.find(name);

    if (iter != symbols.end()) {
        return iter->second;
    } else {
        return Optional<unsigned>::Nothing();
    }
}

void
Module::trace_for_gc(PZ_Heap_Mark_State *marker) const
{
    for (auto c : exports) {
        pz_gc_mark_root(marker, c);
    }
}

} // namespace pz
