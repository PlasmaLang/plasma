/*
 * Plasma bytecode in-memory representation
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2015 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#ifndef PZ_H
#define PZ_H

#include "pz_common.h"
#include "pz_radix_tree.h"

typedef struct PZ_Struct PZ;

typedef struct PZ_Module_Struct PZ_Module;

struct PZ_Struct {
    PZ_RadixTree                *modules;
    PZ_Module                   *entry_module;
};

PZ *
pz_init(void);

void
pz_free(PZ *pz);

/*
 * Add a module to the program.
 *
 * The entry module is not added in this way.
 *
 * The name will be strdup'd and so the caller is responsible for freeing it
 * after this call. The module will be freed by pz_free().
 */
void
pz_add_module(PZ *pz, const char *name, PZ_Module *module);


struct PZ_Module_Struct {
    struct PZ_Structs_Struct    *structs;
    struct PZ_Data_Struct       *data;
    struct PZ_Code_Struct       *code;

    PZ_RadixTree                *symbols;

    /*
     * TODO: Move this field to PZ_Struct
     */
    uint32_t                    entry_proc;
};

PZ_Module *
pz_module_init(void);

void
pz_module_free(PZ_Module *module);

#endif /* ! PZ_H */
