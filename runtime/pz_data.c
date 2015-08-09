/*
 * Plasma bytecode data and types loading and runtime
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2015 Paul Bone
 * Distributed under the terms of the MIT license, see ../LICENSE.runtime
 */

#include <stdint.h>
#include <stdlib.h>

#include "pz_data.h"

pz_data* pz_data_init(uint_fast32_t num_datas, uint_fast32_t* data_offsets,
    uint_fast32_t total_size)
{
    pz_data* data;

    data = malloc(sizeof(pz_data));
    data->num_datas = num_datas;
    data->data_offsets = data_offsets;
    data->total_size = total_size;
    data->data = malloc(total_size);

    return data;
}

void pz_data_free(pz_data* data)
{
    if (data->data_offsets) {
        free(data->data_offsets);
    }
    if (data->data) {
        free(data->data);
    }
    free(data);
}

