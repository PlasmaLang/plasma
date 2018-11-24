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

Module::Module() :
        total_code_size(0),
        next_export(0),
        entry_closure_(-1) {}

Module::Module(unsigned num_structs,
               unsigned num_data,
               unsigned num_procs,
               unsigned num_closures,
               unsigned num_exports,
               int entry_closure) :
        total_code_size(0),
        next_export(0),
        entry_closure_(entry_closure)
{
    structs.reserve(num_structs);
    datas.reserve(num_data);
    procs.reserve(num_procs);
    closures.reserve(num_closures);
    exports.reserve(num_exports);
}

Module::~Module()
{
    for (void* data : datas) {
        assert(data != NULL);
        data_free(data);
    }

    for (auto closure : closures) {
        if (closure) {
            pz_closure_free(closure);
        }
    }
    // Note that we don't free exports since they are pointers to items in
    // the closures vector.
}

Struct&
Module::new_struct(unsigned num_fields)
{
    structs.emplace_back(num_fields);
    return structs.back();
}

void
Module::add_data(void *data)
{
    datas.push_back(data);
}

Proc &
Module::new_proc(unsigned size)
{
    procs.emplace_back(size);
    Proc &proc = procs.back();
    total_code_size += proc.size();
    return proc;
}

void
Module::set_closure(PZ_Closure *closure)
{
    closures.push_back(closure);
}

void
Module::add_symbol(const std::string &name, PZ_Closure *closure)
{
    unsigned id = next_export++;
    symbols[name] = id;
    exports.push_back(closure);
}

Optional<unsigned>
Module::lookup_symbol(const std::string &name)
{
    auto iter = symbols.find(name);

    if (iter != symbols.end()) {
        return iter->second;
    } else {
        return Optional<unsigned>::Nothing();
    }
}

void
Module::print_loaded_stats() const
{
    printf("Loaded %d procedures with a total of %d bytes.\n",
           num_procs(), total_code_size);
}

} // namespace pz
