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
        num_structs(num_structs),
        num_datas(num_data),
        num_procs(num_procs),
        total_code_size(0),
        num_closures(num_closures),
        symbols(NULL),
        num_exports(num_exports),
        next_export(0),
        entry_closure_(entry_closure)
{
    if (num_structs > 0) {
        structs = malloc(sizeof(Struct*) * num_structs);
        memset(structs, 0, sizeof(Struct*) * num_structs);
    } else {
        structs = NULL;
    }

    if (num_data > 0) {
        datas = malloc(sizeof(int8_t *) * num_data);
        memset(datas, 0, sizeof(uint8_t *) * num_data);
    } else {
        datas = NULL;
    }

    if (num_procs > 0) {
        procs = malloc(sizeof(Proc*) * num_procs);
        memset(procs, 0, sizeof(Proc*) * num_procs);
    } else {
        procs = NULL;
    }
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
    unsigned i;

    if (structs != NULL) {
        for (i = 0; i < num_structs; i++) {
            delete structs[i];
        }
        free(structs);
    }

    if (datas != NULL) {
        for (unsigned i = 0; i < num_datas; i++) {
            if (datas[i] != NULL) {
                pz_data_free(datas[i]);
            }
        }
        free(datas);
    }

    if (procs != NULL) {
        for (unsigned i = 0; i < num_procs; i++) {
            if (procs[i]) {
                delete procs[i];
            }
        }

        free(procs);
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
           num_procs, total_code_size);
}

} // namespace pz
