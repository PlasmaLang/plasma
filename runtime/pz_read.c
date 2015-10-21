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
read_data_width(FILE *file, uint8_t *raw_width, unsigned *mem_width);

static bool
read_data_slot(FILE *file, PZ_Data *data, uint8_t raw_width, void *dest);

static bool
read_code(FILE *file, const char *filename, bool verbose, PZ_Data *data,
    PZ_Code *code, unsigned num_procs);

static uint_fast32_t
read_proc_first_pass(FILE *file, PZ_Code *code,
    unsigned **block_offsets_ptr);

static bool
read_proc(FILE *file, PZ_Data *data, PZ_Code *code, uint8_t *proc,
    unsigned *block_offsets);

/*
 * Get the on-disk size of the immediate type.
 */
static unsigned
immediate_encoded_size(Immediate_Type imt);

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
        uint8_t     *field_widths;

        if (!read_uint32(file, &num_fields)) goto error;
        field_widths = pz_new_struct(structs, i, num_fields);

        for (unsigned j = 0; j < num_fields; j++) {
            if (!read_uint8(file, &field_widths[j])) goto error;
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
        uint8_t         raw_width;
        unsigned        mem_width;
        uint16_t        num_elements;
        void            *data_ptr;

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
read_data_width(FILE *file, uint8_t *raw_width_ret, unsigned *mem_width)
{
    uint8_t raw_width;
    enum pz_data_width_type type;

    if (!read_uint8(file, &raw_width)) return false;
    type = PZ_DATA_WIDTH_TYPE(raw_width);
    switch (type) {
        case pz_width_type_normal:
            *mem_width = raw_width & ~PZ_DATA_WIDTH_TYPE_BITS;
            break;
        case pz_width_type_ptr:
        case pz_width_type_wptr:
            *mem_width = MACHINE_WORD_SIZE;
            break;
        case pz_width_type_fast:
            *mem_width = pz_fast_word_size;
            break;
    }

    *raw_width_ret = raw_width;
    return true;
}

static bool
read_data_slot(FILE *file, PZ_Data *data, uint8_t raw_width, void *dest)
{
    uint8_t enc_width;
    enum pz_data_width_type type;

    type = PZ_DATA_WIDTH_TYPE(raw_width);
    switch (type) {
        case pz_width_type_normal:
            enc_width = PZ_DATA_WIDTH_BYTES(raw_width);
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
                    fprintf(stderr, "Unexpected data width %d.\n", raw_width);
                    return false;
            }
        case pz_width_type_ptr:
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
        case pz_width_type_fast:
            {
                uint32_t    i32;

                /*
                 * For these width types the encoded width is 32bit.
                 */
                if (!read_uint32(file, &i32)) return false;
                pz_data_write_fast_from_int32(dest, i32);
                return true;
            }
        case pz_width_type_wptr:
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
    unsigned *block_offsets = NULL;

    for (unsigned i = 0; i < num_procs; i++) {
        long file_pos;
        unsigned proc_size;

        /*
         * We have to read each procedure twice, once to calculate its size
         * then again to read it for real.
         */
        file_pos = ftell(file);
        if (file_pos == -1) goto error;
        if (verbose) {
            fprintf(stderr, "Reading proc %d at 0x%lx\n", i, file_pos);
        }

        proc_size = read_proc_first_pass(file, code, &block_offsets);
        if (proc_size == 0) goto error;
        if (verbose) {
            fprintf(stderr, "Allocating %d bytes for proc\n", proc_size);
        }
        code->procs[i] = pz_code_new_proc(proc_size);
        code->total_size += proc_size;

        if (0 != fseek(file, file_pos, SEEK_SET)) goto error;
        if (!read_proc(file, data, code, code->procs[i], block_offsets)) {
            goto error;
        }
        free(block_offsets);
        block_offsets = NULL;
    }

    if (verbose) {
        printf("Loaded %d procedures with a total of %d bytes.\n",
            (unsigned)code->num_procs, (unsigned)code->total_size);
    }

    return true;

error:
    if (block_offsets != NULL) {
        free(block_offsets);
    }
    return false;
}

static uint_fast32_t
read_proc_first_pass(FILE *file, PZ_Code *code,
    unsigned **block_offsets_ptr)
{
    uint32_t        num_blocks;
    unsigned        proc_size = 0;

    /*
     * XXX: Signatures currently aren't written into the bytecode, but
     * here's where they might appear.
     */

    if (!read_uint32(file, &num_blocks)) return 0;
    *block_offsets_ptr = malloc(sizeof(unsigned)*num_blocks);
    for (unsigned i = 0; i < num_blocks; i++) {
        uint32_t    num_instructions;

        (*block_offsets_ptr)[i] = proc_size;
        if (!read_uint32(file, &num_instructions)) return 0;
        for (uint32_t j = 0; j < num_instructions; j++) {
            uint8_t opcode;
            unsigned imm_encoded_size;
            Immediate_Type imt;

            if (!read_uint8(file, &opcode)) return 0;
            if (0 != fseek(file,
                    instruction_info_data[opcode].ii_num_width_bytes, SEEK_CUR))
            {
                return 0;
            }
            imt = instruction_info_data[opcode].ii_immediate_type;
            imm_encoded_size = immediate_encoded_size(imt);
            switch (imt) {
                case IMT_NONE:
                    proc_size += pz_instr_size(opcode);
                    break;
                case IMT_CODE_REF:
                    {
                        uint32_t imm32;
                        if (!read_uint32(file, &imm32)) return false;
                        if (pz_code_proc_needs_ccall(code, imm32)) {
                            proc_size += pz_instr_size(PZI_CCALL);
                            proc_size = pz_immediate_alignment(imt, proc_size);
                            proc_size += pz_immediate_size(imt);
                        } else {
                            proc_size += pz_instr_size(opcode);
                            proc_size = pz_immediate_alignment(imt, proc_size);
                            proc_size += pz_immediate_size(imt);
                        }
                    }
                    break;
                default:
                    if (imm_encoded_size > 0) {
                        if (0 != fseek(file, imm_encoded_size, SEEK_CUR)) {
                            return 0;
                        }
                    }
                    proc_size += pz_instr_size(opcode);
                    proc_size = pz_immediate_alignment(imt, proc_size);
                    proc_size += pz_immediate_size(imt);
                    break;
            }
        }
    }

    return proc_size;
}

static bool
read_proc(FILE *file, PZ_Data *data, PZ_Code *code, uint8_t *proc,
    unsigned *block_offsets)
{
    uint32_t        num_blocks;
    unsigned        proc_offset = 0;

    /*
     * XXX: Signatures currently aren't written into the bytecode, but
     * here's where they might appear.
     */

    if (!read_uint32(file, &num_blocks)) return false;
    for (unsigned i = 0; i < num_blocks; i++) {
        uint32_t    num_instructions;

        if (!read_uint32(file, &num_instructions)) return false;
        for (uint32_t j = 0; j < num_instructions; j++) {
            uint8_t byte;
            Opcode opcode;
            Operand_Width width1 = 0, width2 = 0;
            Immediate_Type immediate_type;
            uint8_t imm8;
            uint16_t imm16;
            uint32_t imm32;
            uint64_t imm64;
            unsigned last_offset;

            if (!read_uint8(file, &byte)) return false;
            opcode = byte;
            last_offset = proc_offset;
            if (instruction_info_data[opcode].ii_num_width_bytes > 0) {
                if (!read_uint8(file, &byte)) return false;
                width1 = byte;
                if (instruction_info_data[opcode].ii_num_width_bytes > 1) {
                    if (!read_uint8(file, &byte)) return false;
                    width2 = byte;
                }
            }
            pz_write_instr(proc, proc_offset, opcode, width1, width2);

            proc_offset += pz_instr_size(opcode);
            immediate_type = instruction_info_data[opcode].ii_immediate_type;
            if (immediate_type != IMT_NONE) {
                proc_offset = pz_immediate_alignment(immediate_type, proc_offset);
            }
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
                        pz_write_instr(proc, proc_offset, PZI_CCALL, 0, 0);
                        proc_offset += pz_instr_size(PZI_CCALL);
                        proc_offset = ALIGN_UP(proc_offset, MACHINE_WORD_SIZE);
                        pz_write_imm_word(proc, proc_offset,
                            (uintptr_t)pz_code_get_proc(code, imm32));
                    } else {
                        pz_write_imm_word(proc, proc_offset,
                            (uintptr_t)pz_code_get_proc(code, imm32));
                    }
                    break;
                case IMT_LABEL_REF:
                    if (!read_uint32(file, &imm32)) return false;
                    pz_write_imm_word(proc, proc_offset,
                        (uintptr_t)&proc[block_offsets[imm32]]);
                    break;
                case IMT_DATA_REF:
                    if (!read_uint32(file, &imm32)) return false;
                    pz_write_imm_word(proc, proc_offset,
                        (uintptr_t)pz_data_get_data(data, imm32));
                    break;
            }
            proc_offset += pz_immediate_size(immediate_type);
        }
    }

    return true;
}

static unsigned
immediate_encoded_size(Immediate_Type imt)
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
        case IMT_LABEL_REF:
            return 4;
        case IMT_64:
            return 8;
    }
    abort();
}

