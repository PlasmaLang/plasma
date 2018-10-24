/*
 * Plasma in-memory representation
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2015-2016, 2018 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#ifndef PZ_H
#define PZ_H

#include "pz_common.h"

#include "pz_module.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef struct PZ_S PZ;

/*
 * PZ Programs
 *************/

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

PZ_Module *
pz_get_module(PZ *pz, const char *name);

void
pz_add_entry_module(PZ *pz, PZ_Module *module);

PZ_Module *
pz_get_entry_module(PZ *pz);

#ifdef __cplusplus
} // extern "C"
#endif

#endif /* ! PZ_H */
