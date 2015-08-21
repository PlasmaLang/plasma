/*
 * Plasma bytecode reader
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2015 Paul Bone
 * Distributed under the terms of the MIT license, see ../LICENSE.runtime
 */

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "pz_common.h"
#include "pz.h"
#include "pz_code.h"
#include "pz_data.h"
#include "pz_format.h"
#include "pz_read.h"
#include "pz_util.h"
#include "io_utils.h"


static bool
read_options(FILE *file, const char* filename,
    uint32_t *entry_proc);

static bool
read_imported_data(FILE *file, const char* filename);

static uintptr_t*
read_imported_procs(FILE *file, const char* filename,
    uint32_t* num_imported_procs);

static pz_data*
read_data(FILE *file, const char* filename, bool verbose);

static bool
read_data_slot(FILE* file, pz_data* data, uint8_t raw_width, void* dest);

static pz_code*
read_code_first_pass(FILE *file, const char* filename);

static uint_fast32_t
read_proc_first_pass(FILE *file);

pz* read_pz(const char *filename, bool verbose)
{
    FILE*           file;
    uint16_t        magic, version;
    char*           string;
    long            file_pos;
    uint32_t        entry_proc = -1;
    uint32_t        num_imported_procs;
    uintptr_t*      imported_proc_offsets;
    pz_code*        code = NULL;
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

    if (!read_imported_data(file, filename)) goto error;
    imported_proc_offsets =
        read_imported_procs(file, filename, &num_imported_procs);
    if (imported_proc_offsets == NULL) goto error;

    /*
     * read the file in two passes.  During the first pass we calculate the
     * sizes of datas and procedures and therefore calculating the addresses
     * where each individual entry begins.  Then in the second pass we fill
     * read the bytecode and data, resolving any intra-module references.
     */
    file_pos = ftell(file);
    if (file_pos == -1) goto error;

    data = read_data(file, filename, verbose);
    if (data == NULL) goto error;

    code = read_code_first_pass(file, filename);
    if (code == NULL) goto error;
    if (verbose) {
        printf("Loaded %d procedures with a total of 0x%.8x words "
            "(0x%.8x bytes)\n",
            (unsigned)code->num_procs, (unsigned)code->total_size,
            (unsigned)(code->total_size * sizeof(uintptr_t)));
    }

    fclose(file);
    free(imported_proc_offsets);
    pz = malloc(sizeof(struct pz));
    pz->data = data;
    pz->code = code;
    return pz;

error:
    if (ferror(file)) {
        perror(filename);
    } else if (feof(file)) {
        fprintf(stderr, "%s: Unexpected end of file.\n", filename);
    }
    fclose(file);
    if (code) {
        pz_code_free(code);
    }
    if (data) {
        pz_data_free(data);
    }
    if (imported_proc_offsets) {
        free(imported_proc_offsets);
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

static bool
read_imported_data(FILE *file, const char* filename)
{
    uint32_t    num_data_entries;

    if (!read_uint32(file, &num_data_entries)) return false;

    if (num_data_entries != 0) {
        fprintf(stderr, "Imported data entries are not yet supported.\n");
        abort();
    }

    return true;
}

static uintptr_t*
read_imported_procs(FILE *file, const char* filename,
    uint32_t* num_imported_procs_ret)
{
    uint32_t    num_imported_procs;
    uintptr_t*  procs;

    if (!read_uint32(file, &num_imported_procs)) return false;
    procs = malloc(sizeof(uintptr_t) * num_imported_procs);

    for (uint32_t i = 0; i < num_imported_procs; i++) {
        char* module;
        char* name;

        module = read_len_string(file);
        if (module == NULL) return false;
        name = read_len_string(file);
        if (name == NULL) return false;

        /*
         * Currently we don't support linking, only the builtin
         * pseudo-module is recognised.
         */
        // XXX: make this faster, use a BST or trie or something.
        if (strcmp("builtin", module) != 0) {
            fprintf(stderr, "Linking is not supported.\n");
        }
        if (strcmp("print", name) == 0) {
            // procs[i] = builtin_print;
            procs[i] = 0;
        } else {
            fprintf(stderr, "Procedure not found: %s.%s\n",
                module, name);
            free(module);
            free(name);
            return false;
        }
        free(module);
        free(name);
    }

    *num_imported_procs_ret = num_imported_procs;
    return procs;
}

static pz_data*
read_data(FILE *file, const char* filename, bool verbose)
{
    uint32_t        num_datas;
    unsigned        total_size = 0;
    pz_data*        data = NULL;

    if (!read_uint32(file, &num_datas)) goto error;
    data = pz_data_init(num_datas);

    for (uint32_t i = 0; i < num_datas; i++) {
        uint8_t         data_type_id;
        uint8_t         raw_data_width;
        unsigned        data_width;
        uint16_t        num_elements;
        void*           data_ptr;

        if (!read_uint8(file, &data_type_id)) goto error;
        switch (data_type_id) {
            case PZ_DATA_BASIC:
                if (!read_uint8(file, &raw_data_width)) goto error;
                if (raw_data_width == 0) {
                    data_width = MACHINE_WORD_SIZE;
                } else {
                    data_width = raw_data_width;
                }
                data->data[i] = pz_data_new_basic_data(data_width);
                if (!read_data_slot(file, data, raw_data_width,
                        data->data[i]))
                    goto error;
                total_size += data_width;

                break;
            case PZ_DATA_ARRAY:
                if (!read_uint16(file, &num_elements)) return 0;
                if (!read_uint8(file, &raw_data_width)) return 0;
                if (raw_data_width == 0) {
                    // Data is references.
                    data_width = MACHINE_WORD_SIZE;
                } else {
                    data_width = raw_data_width;
                }
                data->data[i] = pz_data_new_array_data(data_width,
                    num_elements);
                data_ptr = data->data[i];
                for (int i = 0; i < num_elements; i++) {
                    if (!read_data_slot(file, data, raw_data_width,
                            data_ptr))
                        goto error;
                    data_ptr += data_width;
                }
                total_size += data_width * num_elements;
                break;
            case PZ_DATA_STRUCT:
                fprintf(stderr, "structs not implemented yet");
                abort();
        }
    }

    if (verbose) {
        printf("Loaded %d data entries with a total of 0x%.8x bytes\n",
            (unsigned)data->num_datas, total_size);
    }

    return data;

    error:
        if (data != NULL) {
            pz_data_free(data);
        }
        return NULL;
}

static bool
read_data_slot(FILE* file, pz_data* data, uint8_t raw_width, void *dest) {
    switch (raw_width) {
        case 0:
            {
                uint32_t ref;
                void** dest_ = (void**)dest;

                // Data is a reference, link in the correct information.
                if (!read_uint32(file, &ref)) return false;
                if (data->data[ref] != NULL) {
                    *dest_ = data->data[ref];
                } else {
                    fprintf(stderr,
                        "forward references arn't yet supported.\n");
                    abort();
                }
                return true;
            }
        case 1:
            if (!read_uint8(file, dest)) return false;
            return true;
        case 2:
            if (!read_uint16(file, dest)) return false;
            return true;
        case 4:
            if (!read_uint32(file, dest)) return false;
            return true;
        case 8:
            if (!read_uint64(file, dest)) return false;
            return true;
        default:
            fprintf(stderr, "Unexpected data width %d.\n", raw_width);
            return false;
    }
}

static pz_code*
read_code_first_pass(FILE *file, const char* filename)
{
    //uint_fast32_t   total_code_size = 0; /* in words */
    uint32_t        num_procs;
    pz_code*        code = NULL;

    if (!read_uint32(file, &num_procs)) goto error;
    code = pz_code_init(num_procs);

    for (uint32_t i = 0; i < num_procs; i++) {
        unsigned proc_size;

        proc_size = read_proc_first_pass(file);
        if (proc_size == 0) goto error;
        pz_code_set_proc_size(code, i, proc_size);
    }

    return code;

error:
    if (code != NULL) {
        pz_code_free(code);
    }
    return NULL;
}

static uint_fast32_t
read_proc_first_pass(FILE *file)
{
    uint32_t        num_instructions;
    uint_fast32_t   proc_size = 0;

    /*
     * XXX: Signatures currently aren't written into the bytecode, but
     * here's where they might appear.
     */

    /*
     * TODO: handle code blocks, procedure preludes and epilogues.
     */
    if (!read_uint32(file, &num_instructions)) return 0;
    for (uint32_t i = 0; i < num_instructions; i++) {
        uint8_t opcode;
        uint_fast32_t imm_encoded_size;

        if (!read_uint8(file, &opcode)) return 0;
        imm_encoded_size = pz_code_immediate_encoded_size(opcode);
        if (imm_encoded_size > 0) {
            if (0 != fseek(file, imm_encoded_size, SEEK_CUR)) return 0;
        }
        proc_size += 1 + pz_code_immediate_size(opcode);
    }

    // Space for the return instruction.
    proc_size++;

    return proc_size;
}

