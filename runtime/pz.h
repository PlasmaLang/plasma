/*
 * Plasma bytecode in-memory representation
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2015 Paul Bone
 * Distributed under the terms of the MIT license, see ../LICENSE.runtime
 */

#ifndef PZ_H
#define PZ_H

typedef struct pz {
    struct pz_data*     data;
    struct pz_code*     code;
    uint32_t            entry_proc;
} pz;

void pz_free(pz*);

#endif /* ! PZ_H */
