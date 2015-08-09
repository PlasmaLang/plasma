/*
 * Plasma bytecode in-memory representation
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2015 Paul Bone
 * Distributed under the terms of the MIT license, see ../LICENSE.runtime
 */

#include <stdint.h>
#include <stdlib.h>

#include "pz.h"
#include "pz_data.h"

void pz_free(pz* pz)
{
    if (pz->data) {
        pz_data_free(pz->data);
    }
    free(pz);
}

int run(pz* pz) {
    return 0;
}

