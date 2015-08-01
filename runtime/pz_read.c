/*
 * Plasma bytecode reader
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2015 Paul Bone
 * Distributed under the terms of the MIT license, see ../LICENSE.runtime
 */

#include <errno.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include "pz.h"
#include "pz_read.h"
#include "tlv_read.h"

#define PZ_MAGIC_TAG            0x505A
#define PZ_MAGIC_STRING_PART    "Plasma abstract machine bytecode"
#define PZ_FORMAT_VERSION       0

pz* read_pz(const char *filename, bool verbose)
{
    FILE        *file;
    uint16_t    tag, len, version;
    char*       string;

    file = fopen(filename, "rb");
    if (file == NULL) {
        perror(filename);
        return NULL;
    }

    if (!tlv_read_tag_length(file, &tag, &len)) goto error;
    if (tag != PZ_MAGIC_TAG) {
        fprintf(stderr, "%s: bad magic value, is this a PZ file?\n", filename);
        goto error;
    }

    string = tlv_read_string(file, len - 2);
    if (string == NULL) goto error;
    if (0 != strncmp(string, PZ_MAGIC_STRING_PART,
            strlen(PZ_MAGIC_STRING_PART)))
    {
        fprintf(stderr, "%s: bad version string, is this a PZ file?\n",
            filename);
        goto error;
    }
    free(string);
    string = NULL;
    if (!tlv_read_uint16(file, &version)) goto error;
    if (version != PZ_FORMAT_VERSION) {
        fprintf(stderr, "Incorrect PZ version, found %d, expecting %d\n",
            version, PZ_FORMAT_VERSION);
        goto error;
    }

    fclose(file);
    return malloc(sizeof(pz));

error:
    if (ferror(file)) {
        perror(filename);
    } else if (feof(file)) {
        fprintf(stderr, "%s: Unexpected end of file.\n", filename);
    }
    fclose(file);
    return NULL;
}

