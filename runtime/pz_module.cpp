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

#include "pz_closure.h"
#include "pz_util.h"

#include "pz_module.h"

namespace pz {

/*
 * ModuleLoading class
 **********************/

ModuleLoading::ModuleLoading() :
        total_code_size(0),
        next_export(0) {}

ModuleLoading::ModuleLoading(unsigned num_structs,
                             unsigned num_data,
                             unsigned num_procs,
                             unsigned num_closures,
                             unsigned num_exports) :
        total_code_size(0),
        next_export(0)
{
    structs.reserve(num_structs);
    datas.reserve(num_data);
    procs.reserve(num_procs);
    closures.reserve(num_closures);
    exports.reserve(num_exports);
}

Struct&
ModuleLoading::new_struct(unsigned num_fields)
{
    structs.emplace_back(num_fields);
    return structs.back();
}

void
ModuleLoading::add_data(void *data)
{
    datas.push_back(data);
}

Proc &
ModuleLoading::new_proc(Heap &heap, unsigned size)
{
    procs.emplace_back(&heap, *this, size);
    Proc &proc = procs.back();
    total_code_size += proc.size();
    return proc;
}

void
ModuleLoading::set_closure(PZ_Closure *closure)
{
    closures.push_back(closure);
}

void
ModuleLoading::add_symbol(const std::string &name, PZ_Closure *closure)
{
    unsigned id = next_export++;
    symbols[name] = id;
    exports.push_back(closure);
}

Optional<unsigned>
ModuleLoading::lookup_symbol(const std::string &name) const
{
    auto iter = symbols.find(name);

    if (iter != symbols.end()) {
        return iter->second;
    } else {
        return Optional<unsigned>::Nothing();
    }
}

void
ModuleLoading::print_loaded_stats() const
{
    printf("Loaded %d procedures with a total of %d bytes.\n",
           num_procs(), total_code_size);
}

void
ModuleLoading::do_trace(PZ_Heap_Mark_State *marker) const
{
    /*
     * This is needed in case we GC during loading, we want to keep this
     * module until we know we're done loading it.
     */
    for (void *d : datas) {
        pz_gc_mark_root(marker, d);
    }

    for (const Proc & p : procs) {
        pz_gc_mark_root(marker, p.code());
    }

    for (PZ_Closure *c : closures) {
        pz_gc_mark_root(marker, c);
    }

    for (PZ_Closure *e : exports) {
        pz_gc_mark_root(marker, e);
    }
}


/*
 * Module class
 ***************/

Module::Module() : entry_closure_(nullptr) {}

Module::Module(ModuleLoading &loading, PZ_Closure *entry_closure) :
    exports(loading.exports),
    symbols(loading.symbols),
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
