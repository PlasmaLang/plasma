/*
 * Plasma bytecode data and types loading and runtime
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2015 Paul Bone
 * Distributed under the terms of the MIT license, see ../LICENSE.runtime
 */

#include <stdlib.h>

#include "pz_common.h"
#include "pz_data.h"

pz_data* pz_data_init(uint_fast32_t num_datas)
{
    pz_data* data;

    data = malloc(sizeof(pz_data));
    data->num_datas = num_datas;
    data->data_offsets = malloc(sizeof(uint_fast32_t)*num_datas);
    data->total_size = 0;
    data->data = NULL;

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

void pz_data_set_entry_size(pz_data* data, uint_fast32_t data_num,
    uint_fast32_t size)
{
    uint_fast32_t offset;

    offset = data->data_offsets[data_num] + size;
    if (data_num == (data->num_datas - 1)) {
        data->total_size = offset;
    } else {
        data->data_offsets[data_num + 1] = offset;
    }
}

