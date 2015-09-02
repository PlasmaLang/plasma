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
#include "pz_run.h"
#include "pz_util.h"
#include "io_utils.h"


static bool
read_options(FILE *file, const char* filename,
    uint32_t *entry_proc);

static bool
read_imported_data(FILE *file, const char* filename);

static imported_proc**
read_imported_procs(FILE *file, const char* filename,
    uint32_t* num_imported_procs);

static pz_data*
read_data(FILE *file, const char* filename, bool verbose);

static bool
read_data_width(FILE* file, uint8_t* raw_width, unsigned* mem_width);

static bool
read_data_slot(FILE* file, pz_data* data, uint8_t raw_width, void* dest);

static bool
read_code(FILE *file, const char* filename, bool verbose, pz_data *data,
    pz_code* code, unsigned num_procs);

static uint_fast32_t
read_proc_first_pass(FILE *file, pz_code* code);

static bool
read_proc(FILE* file, pz_data* data, pz_code* code, uint8_t* proc);

/*
 * Get the on-disk size of the immediate type.
 */
static unsigned
immediate_encoded_size(enum immediate_type imt);

pz* read_pz(const char *filename, bool verbose)
{
    FILE*           file;
    uint16_t        magic, version;
    char*           string;
    uint32_t        entry_proc = -1;
    uint32_t        num_imported_procs;
    imported_proc** imported_procs;
    uint32_t        num_procs;
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
    imported_procs =
        read_imported_procs(file, filename, &num_imported_procs);
    if (imported_procs == NULL) goto error;

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
    pz = malloc(sizeof(struct pz));
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
    if (imported_procs) {
        free(imported_procs);
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

static imported_proc**
read_imported_procs(FILE *file, const char* filename,
    uint32_t* num_imported_procs_ret)
{
    uint32_t        num_imported_procs;
    imported_proc** procs;

    if (!read_uint32(file, &num_imported_procs)) return false;
    procs = malloc(sizeof(imported_proc*) * num_imported_procs);

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
            procs[i] = &builtin_print;
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
        uint8_t         raw_width;
        unsigned        mem_width;
        uint16_t        num_elements;
        void*           data_ptr;

        if (!read_uint8(file, &data_type_id)) goto error;
        switch (data_type_id) {
            case PZ_DATA_BASIC:
                if (!read_data_width(file, &raw_width, &mem_width)) goto error;
                data->data[i] = pz_data_new_basic_data(mem_width);
                if (!read_data_slot(file, data, raw_width, data->data[i]))
                    goto error;
                total_size += mem_width;

                break;
            case PZ_DATA_ARRAY:
                if (!read_uint16(file, &num_elements)) return 0;
                if (!read_data_width(file, &raw_width, &mem_width)) goto error;
                data->data[i] = pz_data_new_array_data(mem_width,
                    num_elements);
                data_ptr = data->data[i];
                for (int i = 0; i < num_elements; i++) {
                    if (!read_data_slot(file, data, raw_width, data_ptr))
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
read_data_width(FILE* file, uint8_t* raw_width_ret, unsigned* mem_width)
{
    uint8_t raw_width;
    uint8_t type;

    if (!read_uint8(file, &raw_width)) return false;
    type = raw_width & PZ_DATA_WIDTH_TYPE_BITS;
    switch (type) {
        case PZ_DATA_WIDTH_TYPE_NORMAL:
            *mem_width = raw_width & ~PZ_DATA_WIDTH_TYPE_BITS;
            break;
        case PZ_DATA_WIDTH_TYPE_PTR:
        case PZ_DATA_WIDTH_TYPE_WPTR:
            *mem_width = MACHINE_WORD_SIZE;
            break;
        case PZ_DATA_WIDTH_TYPE_FAST:
            *mem_width = pz_fast_word_size;
            break;
    }

    *raw_width_ret = raw_width;
    return true;
}

static bool
read_data_slot(FILE* file, pz_data* data, uint8_t raw_width, void *dest)
{
    uint8_t enc_width, type;

    type = raw_width & PZ_DATA_WIDTH_TYPE_BITS;
    enc_width = raw_width & ~PZ_DATA_WIDTH_TYPE_BITS;
    switch (type) {
        case PZ_DATA_WIDTH_TYPE_NORMAL:
            switch (enc_width) {
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
        case PZ_DATA_WIDTH_TYPE_PTR:
            {
                uint32_t ref;
                void** dest_ = (void**)dest;

                // Data is a reference, link in the correct information.
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
        case PZ_DATA_WIDTH_TYPE_WPTR:
        case PZ_DATA_WIDTH_TYPE_FAST:
            {
                uint8_t  i8;
                uint16_t i16;
                uint32_t i32;
                int32_t tmp;

                switch (enc_width) {
                    case 1:
                        if (!read_uint8(file, &i8)) return false;
                        // Cast to signed type then sign extend to 32 bits.
                        tmp = (int8_t)i8;
                        return true;
                    case 2:
                        if (!read_uint16(file, &i16)) return false;
                        tmp = (int16_t)i16;
                        return true;
                    case 4:
                        if (pz_fast_word_size < 4) {
                            fprintf(stderr, "Unexpected fast integer size. "
                              "This PZ file requires a 32bit interpreter.\n");
                            return false;
                        }
                        if (!read_uint32(file, &i32)) return false;
                        tmp = (int32_t)i32;
                        return true;
                    default:
                        fprintf(stderr, "Unexpected data width %d.\n",
                            raw_width);
                        return false;
                }
                if (type == PZ_DATA_WIDTH_TYPE_FAST) {
                    *((int32_t*)dest) = tmp;
                } else if (type == PZ_DATA_WIDTH_TYPE_WPTR) {
                    if (MACHINE_WORD_SIZE == 4) {
                        *((int32_t*)dest) = tmp;
                    } else if (MACHINE_WORD_SIZE == 8) {
                        *((int64_t*)dest) = tmp;
                    } else {
                        fprintf(stderr, "Unsupported machine size %ld.\n",
                            MACHINE_WORD_SIZE);
                        abort();
                    }
                }
                return true;
            }
        default:
            fprintf(stderr, "Unknown data width type.\n");
            abort();
    }
}

static bool
read_code(FILE *file, const char* filename, bool verbose, pz_data* data,
    pz_code* code, unsigned num_procs)
{
    for (unsigned i = 0; i < num_procs; i++) {
        long file_pos;
        unsigned proc_size;

        /*
         * We have to read each procedure twice, once to calculate its size
         * then again to read it for real.
         */
        file_pos = ftell(file);
        if (file_pos == -1) return false;

        proc_size = read_proc_first_pass(file, code);
        if (proc_size == 0) return false;
        code->procs[i] = pz_code_new_proc(proc_size);

        if (0 != fseek(file, file_pos, SEEK_SET)) return false;
        if (!read_proc(file, data, code, code->procs[i])) return false;
        code->total_size += proc_size;
    }

    if (verbose) {
        printf("Loaded %d procedures with a total of %d bytes.\n",
            (unsigned)code->num_procs, (unsigned)code->total_size);
    }

    return true;
}

static uint_fast32_t
read_proc_first_pass(FILE *file, pz_code* code)
{
    uint32_t        num_instructions;
    unsigned        proc_size = 0;

    /*
     * XXX: Signatures currently aren't written into the bytecode, but
     * here's where they might appear.
     */

    /*
     * TODO: handle code blocks.
     */
    if (!read_uint32(file, &num_instructions)) return 0;
    for (uint32_t i = 0; i < num_instructions; i++) {
        uint8_t opcode;
        unsigned imm_encoded_size;
        enum immediate_type imt;

        if (!read_uint8(file, &opcode)) return 0;
        imt = pz_immediate(opcode);
        imm_encoded_size = immediate_encoded_size(imt);
        if (imt == IMT_CODE_REF) {
            uint32_t imm32;
            if (!read_uint32(file, &imm32)) return false;
            if (pz_code_proc_needs_ccall(code, imm32)) {
                proc_size += pz_instr_size(PZI_CCALL);
                proc_size = pz_immediate_alignment(imt, proc_size);
                proc_size += pz_immediate_size(imt);
            } else {
                if (imm_encoded_size > 0) {
                    if (0 != fseek(file, imm_encoded_size, SEEK_CUR)) return 0;
                }
                proc_size += pz_instr_size(opcode);
                proc_size = pz_immediate_alignment(imt, proc_size);
                proc_size += pz_immediate_size(imt);
            }
        } else {
            if (imm_encoded_size > 0) {
                if (0 != fseek(file, imm_encoded_size, SEEK_CUR)) return 0;
            }
            proc_size += pz_instr_size(opcode);
            proc_size = pz_immediate_alignment(imt, proc_size);
            proc_size += pz_immediate_size(imt);
        }
    }

    // Space for the return instruction.
    proc_size += pz_instr_size(PZI_RETURN);

    return proc_size;
}

static bool
read_proc(FILE* file, pz_data* data, pz_code* code, uint8_t* proc)
{
    uint32_t        num_instructions;
    unsigned        proc_offset = 0;

    /*
     * XXX: Signatures currently aren't written into the bytecode, but
     * here's where they might appear.
     */

    /*
     * TODO: handle code blocks, procedure preludes and epilogues.
     */
    if (!read_uint32(file, &num_instructions)) return false;
    for (uint32_t i = 0; i < num_instructions; i++) {
        uint8_t opcode;
        enum immediate_type immediate_type;
        uint8_t imm8;
        uint16_t imm16;
        uint32_t imm32;
        uint64_t imm64;
        unsigned last_offset;

        if (!read_uint8(file, &opcode)) return false;
        last_offset = proc_offset;
        pz_write_instr(proc, proc_offset, opcode);
        proc_offset += pz_instr_size(opcode);
        immediate_type = pz_immediate(opcode);
        proc_offset = pz_immediate_alignment(immediate_type, proc_offset);
        switch (immediate_type) {
            case IMT_NONE:
                break;
            case IMT_8:
                if (!read_uint8(file, &imm8)) return false;
                pz_write_imm8(proc, proc_offset, imm8);
                break;
            case IMT_16:
                if (!read_uint16(file, &imm16)) return false;
                pz_write_imm16(proc, proc_offset, imm16);
                break;
            case IMT_32:
                if (!read_uint32(file, &imm32)) return false;
                pz_write_imm32(proc, proc_offset, imm32);
                break;
            case IMT_64:
                if (!read_uint64(file, &imm64)) return false;
                pz_write_imm64(proc, proc_offset, imm64);
                break;
            case IMT_CODE_REF:
                if (!read_uint32(file, &imm32)) return false;
                if (pz_code_proc_needs_ccall(code, imm32)) {
                    /*
                     * Fix up the instruction to a CCall,
                     * XXX: this is not safe if other calls are bigger
                     * than CCalls.
                     */
                    proc_offset = last_offset;
                    pz_write_instr(proc, proc_offset, PZI_CCALL);
                    proc_offset += pz_instr_size(PZI_CCALL);
                    proc_offset = pz_immediate_alignment(immediate_type,
                        proc_offset);
                    pz_write_imm_word(proc, proc_offset,
                        (uintptr_t)pz_code_get_proc(code, imm32));
                } else {
                    pz_write_imm_word(proc, proc_offset,
                        (uintptr_t)pz_code_get_proc(code, imm32));
                }
                break;
            case IMT_DATA_REF:
                if (!read_uint32(file, &imm32)) return false;
                pz_write_imm_word(proc, proc_offset,
                    (uintptr_t)pz_data_get_data(data, imm32));
                break;
        }
        proc_offset += pz_immediate_size(immediate_type);
    }

    // Return instruction.
    pz_write_instr(proc, proc_offset, PZI_RETURN);

    return true;
}

static unsigned
immediate_encoded_size(enum immediate_type imt)
{
    switch (imt) {
        case IMT_NONE:
            return 0;
        case IMT_8:
            return 1;
        case IMT_16:
            return 2;
        case IMT_32:
        case IMT_DATA_REF:
        case IMT_CODE_REF:
            return 4;
        case IMT_64:
            return 8;
    }
    abort();
}

