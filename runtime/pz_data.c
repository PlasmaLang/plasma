/*
 * Plasma bytecode data and types loading and runtime
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2015 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#include <string.h>

#include "pz_common.h"
#include "pz_data.h"
#include "pz_util.h"

/*
 * Structs
 *
 **********/

void
pz_struct_init(PZ_Struct *s, unsigned num_fields)
{
    s->num_fields = num_fields;
    s->field_widths = malloc(sizeof(Width) * num_fields);
}

void
pz_struct_free(PZ_Struct *s)
{
    free(s->field_widths);
}

/*
 * Data
 *
 **********/

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

void
pz_data_free(void *data)
{
    free(data);
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

