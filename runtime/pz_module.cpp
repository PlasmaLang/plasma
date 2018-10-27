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
#include "pz_radix_tree.h"
#include "pz_util.h"

#include "pz_module.h"

#include "pz_radix_tree.template.h"

namespace pz {

Module::Module(unsigned num_structs,
               unsigned num_data,
               unsigned num_procs,
               unsigned num_closures,
               unsigned num_exports,
               int entry_closure) :
        total_code_size(0),
        num_closures(num_closures),
        symbols(NULL),
        num_exports(num_exports),
        next_export(0),
        entry_closure_(entry_closure)
{
    structs.reserve(num_structs);
    datas.reserve(num_data);
    procs.reserve(num_procs);

    if (num_closures > 0) {
        closures = malloc(sizeof(PZ_Closure*) * num_closures);
        memset(closures, 0, sizeof(PZ_Closure*) * num_closures);
    } else {
        closures = NULL;
    }

    if (num_exports > 0) {
        exports = malloc(sizeof(PZ_Closure*) * num_exports);
        memset(exports, 0, sizeof(PZ_Closure*) * num_exports);
    } else {
        exports = NULL;
    }
}

Module::~Module()
{
    for (void* data : datas) {
        assert(data != NULL);
        pz_data_free(data);
    }

    if (closures != NULL) {
        for (unsigned i = 0; i < num_closures; i++) {
            if (closures[i]) {
                pz_closure_free(closures[i]);
            }
        }

        free(closures);
    }

    if (symbols != NULL) {
        delete symbols;
    }
    if (exports != NULL) {
        // Don't free individual exports since they are in the closures
        // array above.
        free(exports);
    }
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
Module::add_symbol(const char *name, PZ_Closure *closure)
{
    if (NULL == symbols) {
        symbols = new RadixTree<unsigned>();
    }
    unsigned id = next_export++;
    symbols->insert(name, id + 1);
    exports[id] = closure;
}

int
Module::lookup_symbol(const char *name)
{
    if (NULL == symbols) {
        return -1;
    } else {
        Optional<unsigned> result = symbols->lookup(name);
        if (result.hasValue()) {
            return int(result.value()) - 1;
        } else {
            return -1;
        }
    }
}

void
Module::print_loaded_stats() const
{
    printf("Loaded %d procedures with a total of %d bytes.\n",
           num_procs(), total_code_size);
}

} // namespace pz
