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
#include "pz_data.h"
#include "pz_format.h"
#include "pz_read.h"
#include "io_utils.h"


static bool
read_options(FILE *file, const char* filename,
    uint32_t *entry_proc);

static pz_data*
read_data_first_pass(FILE *file, const char* filename);

static uint_fast32_t
read_data_first_pass_basic(FILE *file);

static uint_fast32_t
read_data_first_pass_array(FILE *file);


pz* read_pz(const char *filename, bool verbose)
{
    FILE*           file;
    uint16_t        magic, version;
    char*           string;
    long            file_pos;
    uint32_t        entry_proc = -1;
    pz_data*        data = NULL;
    pz*             pz;

    file = fopen(filename, "rb");
    if (file == NULL) {
        perror(filename);
        return NULL;
    }

    if (!read_uint16(file, &magic)) goto error;
    if (magic != PZ_MAGIC_NUMBER) {
        fprintf(stderr, "%s: bad magic value, is this a PZ file?\n", filename);
        goto error;
    }

    string = read_len_string(file);
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
    if (!read_uint16(file, &version)) goto error;
    if (version != PZ_FORMAT_VERSION) {
        fprintf(stderr, "Incorrect PZ version, found %d, expecting %d\n",
            version, PZ_FORMAT_VERSION);
        goto error;
    }

    if (!read_options(file, filename, &entry_proc)) goto error;

    /*
     * read the file in two passes.  During the first pass we calculate the
     * sizes of datas and procedures and therefore calculating the addresses
     * where each individual entry begins.  Then in the second pass we fill
     * read the bytecode and data, resolving any intra-module references.
     */
    file_pos = ftell(file);
    if (file_pos == -1) goto error;

    data = read_data_first_pass(file, filename);
    if (data == NULL) goto error;

    if (verbose) {
        printf("Loaded %d data entries with a total of 0x%.8x bytes\n",
            (unsigned)data->num_datas, (unsigned)data->total_size);
    }

    fclose(file);
    pz = malloc(sizeof(pz));
    pz->data = data;
    return pz;

error:
    if (ferror(file)) {
        perror(filename);
    } else if (feof(file)) {
        fprintf(stderr, "%s: Unexpected end of file.\n", filename);
    }
    fclose(file);
    if (data) {
        pz_data_free(data);
    }
    return NULL;
}

static bool
read_options(FILE *file, const char* filename,
    uint32_t *entry_proc)
{
    uint16_t    num_options;
    uint16_t    type, len;

    if (!read_uint16(file, &num_options)) return false;

    for (unsigned i = 0; i < num_options; i++) {
        if (!read_uint16(file, &type)) return false;
        if (!read_uint16(file, &len)) return false;

        switch (type) {
            case PZ_OPT_ENTRY_PROC:
                if (len != 4) {
                    fprintf(stderr, "%s: Corrupt file while reading options",
                        filename);
                    return false;
                }
                read_uint32(file, entry_proc);
                break;
            default:
                fseek(file, len, SEEK_CUR);
                break;
        }
    }

    return true;
}

static pz_data*
read_data_first_pass(FILE *file, const char* filename)
{
    uint32_t        num_datas;
    pz_data*        data = NULL;

    if (!read_uint32(file, &num_datas)) goto error;
    data = pz_data_init(num_datas);

    for (uint32_t i = 0; i < num_datas; i++) {
        uint8_t         data_type_id;
        uint_fast32_t   data_width;

        if (!read_uint8(file, &data_type_id)) goto error;
        switch (data_type_id) {
            case PZ_DATA_BASIC:
                data_width = read_data_first_pass_basic(file);
                break;
            case PZ_DATA_ARRAY:
                data_width = read_data_first_pass_array(file);
                break;
            case PZ_DATA_STRUCT:
                fprintf(stderr, "structs not implemented yet");
                abort();
        }

        if (data_width == 0) goto error;
        pz_data_set_entry_size(data, i, data_width);
    }

    return data;

    error:
        if (data != NULL) {
            pz_data_free(data);
        }
        return NULL;
}

static uint_fast32_t
read_data_first_pass_basic(FILE *file)
{
    uint8_t data_width;

    if (!read_uint8(file, &data_width)) return 0;
    if (data_width == 0) {
        // Data is a pointer.  Seek over the 32bit reference.
        if (0 != fseek(file, 4, SEEK_CUR)) return 0;
        return sizeof(void*);
    } else {
        if (0 != fseek(file, data_width, SEEK_CUR)) return 0;
        return data_width;
    }
}

static uint_fast32_t
read_data_first_pass_array(FILE *file)
{
    uint16_t    num_elements;
    uint8_t     data_width;

    if (!read_uint16(file, &num_elements)) return 0;
    if (!read_uint8(file, &data_width)) return 0;
    if (data_width == 0) {
        // Data is a pointer.  Seek over the 32bit reference.
        if (0 != fseek(file, 4*num_elements, SEEK_CUR)) return 0;
        return sizeof(void*) * num_elements;
    } else {
        if (0 != fseek(file, data_width*num_elements, SEEK_CUR)) return 0;
        return data_width*num_elements;
    }
}

