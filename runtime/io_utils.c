/*
 * IO Utils.
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2015 Paul Bone
 * Distributed under the terms of the MIT license, see ../LICENSE.runtime
 */

#include <stdlib.h>
#include <stdio.h>

#include "pz_common.h"

#include "io_utils.h"

bool
read_uint8(FILE *stream, uint8_t *value)
{
    return (1 == fread(value, sizeof(uint8_t), 1, stream));
}

bool
read_uint16(FILE *stream, uint16_t *value)
{
    uint8_t     bytes[2];

    if (!fread(bytes, sizeof(uint8_t), 2, stream)) {
        return false;
    }

    *value = ((uint16_t)bytes[0] << 8) | (uint16_t)bytes[1];

    return true;
}

bool
read_uint32(FILE *stream, uint32_t *value)
{
    uint8_t     bytes[4];

    if (!fread(bytes, sizeof(uint8_t), 4, stream)) {
        return false;
    }

    *value = ((uint32_t)bytes[0] << 24) | ((uint32_t)bytes[1] << 16) |
        ((uint32_t)bytes[2] << 8) | (uint32_t)bytes[3];

    return true;
}

bool
read_uint64(FILE *stream, uint64_t *value)
{
    uint8_t     bytes[4];

    if (!fread(bytes, sizeof(uint8_t), 8, stream)) {
        return false;
    }

    *value =
        ((uint64_t)bytes[0] << 56) | ((uint64_t)bytes[1] << 48) |
        ((uint64_t)bytes[2] << 40) | ((uint64_t)bytes[3] << 32) |
        ((uint64_t)bytes[4] << 24) | ((uint64_t)bytes[5] << 16) |
        ((uint64_t)bytes[6] << 8) | (uint64_t)bytes[7];

    return true;
}

char*
read_len_string(FILE *stream)
{
    uint16_t    len;

    if (!read_uint16(stream, &len)) {
        return NULL;
    }
    return read_string(stream, len);
}

char*
read_string(FILE *stream, int16_t len)
{
    char *buffer;

    buffer = malloc(sizeof(char)*(len+1));
    if (len != fread(buffer, sizeof(char), len, stream)) {
        free(buffer);
        return NULL;
    }
    buffer[len] = 0;

    return buffer;
}

