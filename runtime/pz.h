/*
 * Plasma bytecode in-memory representation
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2015 Paul Bone
 * Distributed under the terms of the MIT license, see ../LICENSE.runtime
 */

#ifndef PZ_H
#define PZ_H

#include "pz_data.h"

typedef struct {
    pz_data*            data;
} pz;

void pz_free(pz*);

/*
 * Run the program.
 */
int run(pz*);

#endif /* ! PZ_H */
