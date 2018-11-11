/*
 * Plasma bytecode data and types loading and runtime
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2015-2018 Plasma Team
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

namespace pz {

void
Struct::calculate_layout()
{
#ifdef PZ_DEV
    assert(!layout_calculated);
    layout_calculated = true;
#endif
    unsigned size = 0;

    for (unsigned i = 0; i < num_fields(); i++) {
        unsigned field_size = width_to_bytes(fields[i].width);

        size = ALIGN_UP(size, field_size);
        fields[i].offset = size;
        size += field_size;
    }
    total_size_ = size;
}

/*
 * Data
 *
 **********/

void *
data_new_array_data(PZ_Width width, uint32_t num_elements)
{
    return malloc(width_to_bytes(width) * num_elements);
}

void *
data_new_struct_data(uintptr_t size)
{
    // TODO: Make this allocate via the GC, then use it during execution of 
    // PZT_ALLOC.
    return malloc(size);
}

void
data_free(void *data)
{
    free(data);
}

/*
 * Functions for storing data in memory
 ***************************************/

void
data_write_normal_uint8(void *dest, uint8_t value)
{
    *((uint8_t *)dest) = value;
}

void
data_write_normal_uint16(void *dest, uint16_t value)
{
    *((uint16_t *)dest) = value;
}

void
data_write_normal_uint32(void *dest, uint32_t value)
{
    *((uint32_t *)dest) = value;
}

void
data_write_normal_uint64(void *dest, uint64_t value)
{
    *((uint64_t *)dest) = value;
}

void
data_write_fast_from_int32(void *dest, int32_t value)
{
    *((PZ_FAST_INTEGER_TYPE *)dest) = (PZ_FAST_INTEGER_TYPE)value;
}

void
data_write_wptr(void *dest, intptr_t value)
{
    *((intptr_t *)dest) = value;
}

Optional<PZ_Width>
width_from_int(uint8_t w) {
    switch (w) {
        case PZW_8:
            return PZW_8;
        case PZW_16:
            return PZW_16;
        case PZW_32:
            return PZW_32;
        case PZW_64:
            return PZW_64;
        case PZW_FAST:
            return PZW_FAST;
        case PZW_PTR:
            return PZW_PTR;
        default:
            return Optional<PZ_Width>::Nothing();
    }
}

PZ_Width
width_normalize(PZ_Width width)
{
    switch (width) {
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
            return width;
    }
}

unsigned
width_to_bytes(PZ_Width width)
{
    width = width_normalize(width);
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

} // namespace pz
