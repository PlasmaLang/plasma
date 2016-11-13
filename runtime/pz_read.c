/*
 * Plasma bytecode reader
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2015 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
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
#include "pz_run.h"
#include "pz_util.h"
#include "io_utils.h"


static bool
read_options(FILE *file, const char *filename,
    uint32_t *entry_proc);

static bool
read_imported_data(FILE *file, const char *filename);

static Imported_Proc**
read_imported_procs(FILE *file, const char *filename,
    uint32_t *num_imported_procs);

static PZ_Structs*
read_structs(FILE *file, const char *filename, bool verbose);

static PZ_Data*
read_data(FILE *file, const char *filename, bool verbose);

static bool
read_data_width(FILE *file, unsigned *mem_width);

static bool
read_data_slot(FILE *file, PZ_Data *data, void *dest);

static bool
read_code(FILE *file, const char *filename, bool verbose, PZ_Data *data,
    PZ_Code *code, unsigned num_procs);

static unsigned
read_proc(FILE *file, PZ_Data *data, PZ_Code *code, uint8_t *proc_code,
    unsigned proc_offset, unsigned **block_offsets);

PZ *read_pz(const char *filename, bool verbose)
{
    FILE            *file;
    uint16_t        magic, version;
    char            *string;
    uint32_t        entry_proc = -1;
    uint32_t        num_imported_procs;
    Imported_Proc   **imported_procs = NULL;
    uint32_t        num_procs;
    PZ_Code         *code = NULL;
    PZ_Data         *data = NULL;
    PZ_Structs      *structs = NULL;
    PZ              *pz;

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
    imported_procs =
        read_imported_procs(file, filename, &num_imported_procs);
    if (imported_procs == NULL) goto error;

    structs = read_structs(file, filename, verbose);
    if (structs == NULL) goto error;

    /*
     * read the file in two passes.  During the first pass we calculate the
     * sizes of datas and procedures and therefore calculating the addresses
     * where each individual entry begins.  Then in the second pass we fill
     * read the bytecode and data, resolving any intra-module references.
     */
    data = read_data(file, filename, verbose);
    if (data == NULL) goto error;

    if (!read_uint32(file, &num_procs)) goto error;
    code = pz_code_init(num_imported_procs, imported_procs, num_procs);
    if (!read_code(file, filename, verbose, data, code, num_procs))
        goto error;

    fclose(file);
    pz = malloc(sizeof(struct PZ_Struct));
    pz->structs = structs;
    pz->data = data;
    pz->code = code;
    pz->entry_proc = entry_proc;
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
    if (structs) {
        pz_structs_free(structs);
    }
    if (imported_procs) {
        free(imported_procs);
    }
    return NULL;
}

static bool
read_options(FILE *file, const char *filename,
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
read_imported_data(FILE *file, const char *filename)
{
    uint32_t    num_data_entries;

    if (!read_uint32(file, &num_data_entries)) return false;

    if (num_data_entries != 0) {
        fprintf(stderr, "Imported data entries are not yet supported.\n");
        abort();
    }

    return true;
}

static Imported_Proc**
read_imported_procs(FILE *file, const char *filename,
    uint32_t* num_imported_procs_ret)
{
    uint32_t        num_imported_procs;
    Imported_Proc   **procs = NULL;

    if (!read_uint32(file, &num_imported_procs)) goto error;
    procs = malloc(sizeof(Imported_Proc*) * num_imported_procs);

    for (uint32_t i = 0; i < num_imported_procs; i++) {
        char *module;
        char *name;

        module = read_len_string(file);
        if (module == NULL) goto error;
        name = read_len_string(file);
        if (name == NULL) goto error;

        /*
         * Currently we don't support linking, only the builtin
         * pseudo-module is recognised.
         */
        // XXX: make this faster, use a BST or trie or something.
        if (strcmp("builtin", module) != 0) {
            fprintf(stderr, "Linking is not supported.\n");
        }
        if (strcmp("print", name) == 0) {
            procs[i] = &builtin_print;
        } else if (strcmp("int_to_string", name) == 0) {
            procs[i] = &builtin_int_to_string;
        } else if (strcmp("free", name) == 0) {
            procs[i] = &builtin_free;
        } else if (strcmp("concat_string", name) == 0) {
            procs[i] = &builtin_concat_string;
        } else if (strcmp("die", name) == 0) {
            procs[i] = &builtin_die;
        } else {
            fprintf(stderr, "Procedure not found: %s.%s\n",
                module, name);
            free(module);
            free(name);
            goto error;
        }
        free(module);
        free(name);
    }

    *num_imported_procs_ret = num_imported_procs;
    return procs;
error:
    if (procs != NULL) {
        free(procs);
    }
    return NULL;
}

static PZ_Structs *
read_structs(FILE *file, const char *filename, bool verbose)
{
    PZ_Structs  *structs = NULL;
    uint32_t    num_structs;

    if (!read_uint32(file, &num_structs)) goto error;
    structs = pz_structs_init(num_structs);

    for (unsigned i = 0; i < num_structs; i++) {
        uint32_t    num_fields;
        Width       *field_widths;

        if (!read_uint32(file, &num_fields)) goto error;
        field_widths = pz_new_struct(structs, i, num_fields);

        for (unsigned j = 0; j < num_fields; j++) {
            uint8_t v;
            if (!read_uint8(file, &v)) goto error;
            field_widths[j] = v;
        }
    }

    return structs;

error:
    if (structs) {
        pz_structs_free(structs);
    }
    return NULL;
}

static PZ_Data *
read_data(FILE *file, const char *filename, bool verbose)
{
    uint32_t        num_datas;
    unsigned        total_size = 0;
    PZ_Data         *data = NULL;

    if (!read_uint32(file, &num_datas)) goto error;
    data = pz_data_init(num_datas);

    for (uint32_t i = 0; i < num_datas; i++) {
        uint8_t         data_type_id;
        unsigned        mem_width;
        uint16_t        num_elements;
        void            *data_ptr;

        if (!read_uint8(file, &data_type_id)) goto error;
        switch (data_type_id) {
            case PZ_DATA_BASIC:
                if (!read_data_width(file, &mem_width)) goto error;
                data->data[i] = pz_data_new_basic_data(mem_width);
                if (!read_data_slot(file, data, data->data[i]))
                    goto error;
                total_size += mem_width;

                break;
            case PZ_DATA_ARRAY:
                if (!read_uint16(file, &num_elements)) return 0;
                if (!read_data_width(file, &mem_width)) goto error;
                data->data[i] = pz_data_new_array_data(mem_width,
                    num_elements);
                data_ptr = data->data[i];
                for (int i = 0; i < num_elements; i++) {
                    if (!read_data_slot(file, data, data_ptr))
                        goto error;
                    data_ptr += mem_width;
                }
                total_size += mem_width * num_elements;
                break;
            case PZ_DATA_STRUCT:
                fprintf(stderr, "structs not implemented yet");
                abort();
        }
    }

    if (verbose) {
        printf("Loaded %d data entries with a total of %d bytes\n",
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
read_data_width(FILE *file, unsigned *mem_width)
{
    uint8_t raw_width;
    Width width;

    if (!read_uint8(file, &raw_width)) return false;
    width = raw_width;
    *mem_width = pz_width_to_bytes(width);

    return true;
}

static bool
read_data_slot(FILE *file, PZ_Data *data, void *dest)
{
    uint8_t enc_width, raw_enc;
    enum pz_data_enc_type type;

    if (!read_uint8(file, &raw_enc)) return false;
    type = PZ_DATA_ENC_TYPE(raw_enc);

    switch (type) {
        case pz_data_enc_type_normal:
            enc_width = PZ_DATA_ENC_BYTES(raw_enc);
            switch (enc_width) {
                case 1:
                    {
                        uint8_t value;
                        if (!read_uint8(file, &value)) return false;
                        pz_data_write_normal_uint8(dest, value);
                        return true;
                    }
                case 2:
                    {
                        uint16_t value;
                        if (!read_uint16(file, &value)) return false;
                        pz_data_write_normal_uint16(dest, value);
                        return true;
                    }
                case 4:
                    {
                        uint32_t value;
                        if (!read_uint32(file, &value)) return false;
                        pz_data_write_normal_uint32(dest, value);
                        return true;
                    }
                case 8:
                    {
                        uint64_t value;
                        if (!read_uint64(file, &value)) return false;
                        pz_data_write_normal_uint64(dest, value);
                        return true;
                    }
                default:
                    fprintf(stderr, "Unexpected data encoding %d.\n", raw_enc);
                    return false;
            }
        case pz_data_enc_type_ptr:
            {
                uint32_t ref;
                void **dest_ = (void**)dest;

                // Data is a reference, link in the correct information.
                // XXX: support non-data references, such as proc
                // references.
                if (!read_uint32(file, &ref)) return false;
                if (data->data[ref] != NULL) {
                    *dest_ = pz_data_get_data(data, ref);
                } else {
                    fprintf(stderr,
                        "forward references arn't yet supported.\n");
                    abort();
                }
            }
            return true;
        case pz_data_enc_type_fast:
            {
                uint32_t    i32;

                /*
                 * For these width types the encoded width is 32bit.
                 */
                if (!read_uint32(file, &i32)) return false;
                pz_data_write_fast_from_int32(dest, i32);
                return true;
            }
        case pz_data_enc_type_wptr:
            {
                int32_t     i32;

                /*
                 * For these width types the encoded width is 32bit.
                 */
                if (!read_uint32(file, (uint32_t*)&i32)) return false;
                pz_data_write_wptr(dest, (uintptr_t)i32);
                return true;
            }
        default:
            // GCC is having trouble recognising this complete switch.
            fprintf(stderr, "Internal error.\n");
            abort();
    }
}

static bool
read_code(FILE *file, const char *filename, bool verbose, PZ_Data *data,
    PZ_Code *code, unsigned num_procs)
{
    bool            result = false;
    long            file_pos;
    unsigned        **block_offsets = malloc(sizeof(unsigned*) * num_procs);
    unsigned        offset;

    memset(block_offsets, 0, sizeof(unsigned*) * num_procs);

    /*
     * We read procedures in two phases, once to calculate their sizes, and
     * label offsets, allocating memory for each one.  Then the we read them
     * for real in the second phase when memory locations are known.
     */
    if (verbose) {
        fprintf(stderr, "Reading procs first pass\n");
    }
    file_pos = ftell(file);
    if (file_pos == -1) goto end;

    offset = 0;
    for (unsigned i = 0; i < num_procs; i++) {
        unsigned proc_size;
        unsigned new_offset;

        if (verbose) {
            fprintf(stderr, "Reading proc %d\n", i);
        }

        new_offset = read_proc(file, data, code, NULL, offset,
            &block_offsets[i]);
        if (new_offset == 0) goto end;
        proc_size = new_offset - offset;
        pz_code_new_proc(code, i, offset, proc_size);
        offset = new_offset;
    }
    pz_code_allocate_memory(offset, code);

    if (verbose) {
        fprintf(stderr, "Reading procs second pass.\n");
    }
    if (0 != fseek(file, file_pos, SEEK_SET)) goto end;

    for (unsigned i = 0; i < num_procs; i++) {
        if (verbose) {
            fprintf(stderr, "Reading proc %d\n", i);
        }
        if (0 ==
            read_proc(file, data, code, code->code,
                code->procs[i]->code_offset, &block_offsets[i]))
        {
            goto end;
        }
    }

    if (verbose) {
        printf("Loaded %d procedures with a total of %d bytes.\n",
            (unsigned)code->num_procs, (unsigned)code->total_size);
    }
    result = true;

end:
    if (block_offsets != NULL) {
        for (unsigned i = 0; i < num_procs; i++) {
            if (block_offsets[i] != NULL) {
                free(block_offsets[i]);
            }
        }
        free(block_offsets);
    }
    return result;
}

static unsigned
read_proc(FILE *file, PZ_Data *data, PZ_Code *code, uint8_t *proc_code,
    unsigned proc_offset, unsigned **block_offsets)
{
    uint32_t        num_blocks;
    bool            first_pass = (proc_code == NULL);

    /*
     * XXX: Signatures currently aren't written into the bytecode, but
     * here's where they might appear.
     */

    if (!read_uint32(file, &num_blocks)) return 0;
    if (first_pass) {
        /*
         * This is the first pass - set up the block offsets array.
         */
        *block_offsets = malloc(sizeof(unsigned)*num_blocks);
    }

    for (unsigned i = 0; i < num_blocks; i++) {
        uint32_t    num_instructions;

        if (first_pass) {
            /*
             * Fill in the block_offsets array
             */
            (*block_offsets)[i] = proc_offset;
        }

        if (!read_uint32(file, &num_instructions)) return 0;
        for (uint32_t j = 0; j < num_instructions; j++) {
            uint8_t byte;
            Opcode opcode;
            Width width1 = 0, width2 = 0;
            Immediate_Type immediate_type;
            Immediate_Value immediate_value;

            /*
             * Read the opcode and the data width(s)
             */
            if (!read_uint8(file, &byte)) return 0;
            opcode = byte;
            if (instruction_info_data[opcode].ii_num_width_bytes > 0) {
                if (!read_uint8(file, &byte)) return 0;
                width1 = byte;
                if (instruction_info_data[opcode].ii_num_width_bytes > 1) {
                    if (!read_uint8(file, &byte)) return 0;
                    width2 = byte;
                }
            }

            /*
             * Read any immediate value
             */
            immediate_type = instruction_info_data[opcode].ii_immediate_type;
            switch (immediate_type) {
                case IMT_NONE:
                    memset(&immediate_value, 0, sizeof(Immediate_Value));
                    break;
                case IMT_8:
                    if (!read_uint8(file, &immediate_value.uint8)) return 0;
                    break;
                case IMT_16:
                    if (!read_uint16(file, &immediate_value.uint16))
                        return 0;
                    break;
                case IMT_32:
                    if (!read_uint32(file, &immediate_value.uint32))
                        return 0;
                    break;
                case IMT_64:
                    if (!read_uint64(file, &immediate_value.uint64))
                        return 0;
                    break;
                case IMT_CODE_REF: {
                    uint32_t imm32;
                    if (!read_uint32(file, &imm32)) return 0;
                    if (pz_code_proc_needs_ccall(code, imm32)) {
                        /*
                         * Fix up the instruction to a CCall,
                         * XXX: this is not safe if other calls are bigger
                         * than CCalls.
                         */
                        opcode = PZI_CCALL;
                    }
                    if (!first_pass) {
                        immediate_value.word =
                            (uintptr_t)pz_code_get_proc_code(code, imm32);
                    } else {
                        immediate_value.word = 0;
                    }
                }
                break;
                case IMT_LABEL_REF: {
                    uint32_t imm32;
                    if (!read_uint32(file, &imm32)) return 0;
                    if (!first_pass) {
                        immediate_value.word =
                            (uintptr_t)&proc_code[(*block_offsets)[imm32]];
                    } else {
                        immediate_value.word = 0;
                    }
                }
                break;
                case IMT_DATA_REF: {
                    uint32_t imm32;
                    if (!read_uint32(file, &imm32)) return 0;
                    immediate_value.word =
                        (uintptr_t)pz_data_get_data(data, imm32);
                }
                break;
            }

            proc_offset = pz_write_instr(proc_code, proc_offset, opcode, width1,
                width2, immediate_type, immediate_value);
        }
    }

    return proc_offset;
}

