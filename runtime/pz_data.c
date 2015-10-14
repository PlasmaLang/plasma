/*
 * Plasma bytecode data and types loading and runtime
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2015 Paul Bone
 * Distributed under the terms of the MIT license, see ../LICENSE.runtime
 */

#include <stdlib.h>
#include <string.h>

#include "pz_common.h"
#include "pz_data.h"
#include "pz_util.h"

PZ_Data *
pz_data_init(uint_fast32_t num_datas)
{
    PZ_Data *data;

    data = malloc(sizeof(struct PZ_Data_Struct));
    data->num_datas = num_datas;
    data->data = malloc(sizeof(uint8_t*)*num_datas);
    memset(data->data, 0, sizeof(uint8_t*)*num_datas);

    return data;
}

void
pz_data_free(PZ_Data *data)
{
    if (data->data) {
        for (uint32_t i = 0; i < data->num_datas; i++) {
            if (data->data[i] != NULL) {
                free(data->data[i]);
            }
        }
        free(data->data);
    }
    free(data);
}

void *
pz_data_new_basic_data(unsigned raw_width)
{
    if (raw_width == 0) {
        return malloc(MACHINE_WORD_SIZE);
    } else {
        return malloc(raw_width);
    }
}

void *
pz_data_new_array_data(unsigned raw_width, uint32_t num_elements)
{
    if (raw_width == 0) {
        return malloc(MACHINE_WORD_SIZE * num_elements);
    } else {
        return malloc(raw_width * num_elements);
    }
}

void *
pz_data_get_data(PZ_Data *data, uint32_t id)
{
    return data->data[id];
}

/*
 * Functions for storing data in memory
 ***************************************/

void pz_data_write_normal_uint8(void *dest, uint8_t value)
{
    *((uint8_t*)dest) = value;
}

void pz_data_write_normal_uint16(void *dest, uint16_t value)
{
    *((uint16_t*)dest) = value;
}

void pz_data_write_normal_uint32(void *dest, uint32_t value)
{
    *((uint32_t*)dest) = value;
}

void pz_data_write_normal_uint64(void *dest, uint64_t value)
{
    *((uint64_t*)dest) = value;
}

void pz_data_write_fast_from_int32(void *dest, int32_t value)
{
    *((PZ_FAST_INTEGER_TYPE*)dest) = (PZ_FAST_INTEGER_TYPE)value;
}

void pz_data_write_wptr(void *dest, intptr_t value)
{
    *((intptr_t*)dest) = value;
}

