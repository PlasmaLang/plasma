/*
 * Plasma bytecode data and types loading and runtime
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2015-2017 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#include <stdio.h>
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
    s->field_offsets = malloc(sizeof(uint16_t) * num_fields);
}

void
pz_struct_free(PZ_Struct *s)
{
    free(s->field_widths);
    free(s->field_offsets);
}

void
pz_struct_calculate_layout(PZ_Struct *s)
{
    unsigned total_size = 0;

    for (unsigned i = 0; i < s->num_fields; i++) {
        unsigned field_size = pz_width_to_bytes(s->field_widths[i]);

        total_size = ALIGN_UP(total_size, field_size);
        s->field_offsets[i] = total_size;
        total_size += field_size;
    }
    s->total_size = total_size;
}

/*
 * Data
 *
 **********/

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

void
pz_data_write_normal_uint8(void *dest, uint8_t value)
{
    *((uint8_t *)dest) = value;
}

void
pz_data_write_normal_uint16(void *dest, uint16_t value)
{
    *((uint16_t *)dest) = value;
}

void
pz_data_write_normal_uint32(void *dest, uint32_t value)
{
    *((uint32_t *)dest) = value;
}

void
pz_data_write_normal_uint64(void *dest, uint64_t value)
{
    *((uint64_t *)dest) = value;
}

void
pz_data_write_fast_from_int32(void *dest, int32_t value)
{
    *((PZ_FAST_INTEGER_TYPE *)dest) = (PZ_FAST_INTEGER_TYPE)value;
}

void
pz_data_write_wptr(void *dest, intptr_t value)
{
    *((intptr_t *)dest) = value;
}

Width
pz_normalize_width(Width w)
{
    switch (w) {
        case PZW_FAST:
            switch (PZ_FAST_INTEGER_WIDTH) {
                case 32: return PZW_32;
                case 64: return PZW_64;
                default:
                    fprintf(
                      stderr,
                      "PZ_FAST_INTEGER_WIDTH has unanticipated value\n");
                    abort();
            }
            break;
        case PZW_PTR:
            switch (sizeof(intptr_t)) {
                case 4: return PZW_32;
                case 8: return PZW_64;
                default:
                    fprintf(stderr, "Unknown pointer width\n");
                    abort();
            }
            break;
        default:
            return w;
    }
}

unsigned
pz_width_to_bytes(Width width)
{
    width = pz_normalize_width(width);
    switch (width) {
        case PZW_8:
            return 1;
        case PZW_16:
            return 2;
        case PZW_32:
            return 4;
        case PZW_64:
            return 8;
        default:
            fprintf(stderr, "Width should have been normalized");
            abort();
    }
}
