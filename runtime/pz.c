/*
 * Plasma bytecode in-memory representation
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2015 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#include <stdlib.h>

#include "pz_common.h"
#include "pz.h"
#include "pz_code.h"
#include "pz_data.h"

void pz_free(PZ *pz)
{
    if (pz->structs) {
        pz_structs_free(pz->structs);
    }
    if (pz->data) {
        pz_data_free(pz->data);
    }
    if (pz->code) {
        pz_code_free(pz->code);
    }
    free(pz);
}

