/*
 * TLV reader.
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2015 Paul Bone
 * Distributed under the terms of the MIT license, see ../LICENSE.runtime
 */

#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>

#include "tlv_read.h"

bool tlv_read_tag_length(FILE* stream, uint16_t* tag, uint16_t* length)
{
    if (!tlv_read_uint16(stream, tag)) {
        return false;
    }
    return tlv_read_uint16(stream, length);
}

bool tlv_read_uint8(FILE *stream, uint8_t *value)
{
    return (1 == fread(value, sizeof(uint8_t), 1, stream));
}

bool tlv_read_uint16(FILE *stream, uint16_t *value)
{
    uint8_t     bytes[2];

    if (!fread(bytes, sizeof(uint8_t), 2, stream)) {
        return false;
    }

    *value = (bytes[0] << 8) | bytes[1];

    return true;
}

char* tlv_read_string(FILE* stream, int16_t len)
{
    char* buffer;

    buffer = malloc(sizeof(char)*(len+1));
    if (len != fread(buffer, sizeof(char), len, stream)) {
        free(buffer);
        return NULL;
    }
    buffer[len] = 0;

    return buffer;
}

