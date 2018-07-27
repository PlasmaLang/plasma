/*
 * Plasma bytecode reader
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2015-2018 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#include <errno.h>
#include <stdio.h>
#include <string.h>

#include "pz_common.h"

#include "io_utils.h"
#include "pz.h"
#include "pz_code.h"
#include "pz_data.h"
#include "pz_format.h"
#include "pz_radix_tree.h"
#include "pz_read.h"
#include "pz_run.h"

typedef struct {
    unsigned       num_imports;
    PZ_Closure   **imports;
} PZ_Imported;

static bool
read_options(FILE *file, const char *filename, int32_t *entry_closure);

static bool
read_imports(FILE        *file,
             unsigned     num_imports,
             PZ          *pz,
             PZ_Imported *imported,
             const char  *filename);

static bool
read_structs(FILE       *file,
             unsigned    num_structs,
             PZ_Module  *module,
             const char *filename,
             bool        verbose);

static bool
read_data(FILE       *file,
          unsigned    num_datas,
          PZ_Module  *module,
          const char *filename,
          bool        verbose);

static bool
read_data_width(FILE *file, unsigned *mem_width);

static bool
read_data_slot(FILE *file, void *dest, PZ_Module *module);

static bool
read_code(FILE        *file,
          unsigned     num_procs,
          PZ_Module   *module,
          PZ_Imported *imported,
          const char  *filename,
          bool         verbose);

static unsigned
read_proc(FILE         *file,
          PZ_Imported  *imported,
          PZ_Module    *module,
          uint8_t      *proc_code,
          unsigned    **block_offsets);

static bool
read_closures(FILE        *file,
              unsigned     num_closures,
              PZ_Imported *imported,
              PZ_Module   *module,
              const char  *filename,
              bool         verbose);

PZ_Module *
pz_read(PZ *pz, const char *filename, bool verbose)
{
    FILE        *file;
    uint16_t     magic, version;
    char        *string;
    int32_t      entry_closure = -1;
    uint32_t     num_imports;
    uint32_t     num_structs;
    uint32_t     num_datas;
    uint32_t     num_procs;
    uint32_t     num_closures;
    uint8_t      extra_byte;
    PZ_Module   *module = NULL;
    PZ_Imported  imported;

    imported.imports = NULL;

    file = fopen(filename, "rb");
    if (file == NULL) {
        perror(filename);
        return NULL;
    }

    if (!read_uint16(file, &magic)) goto error;
    if (magic != PZ_MAGIC_NUMBER) {
        fprintf(stderr, "%s: bad magic value, is this a PZ file?\n",
                filename);
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

    if (!read_options(file, filename, &entry_closure)) goto error;

    if (!read_uint32(file, &num_imports)) goto error;
    if (!read_uint32(file, &num_structs)) goto error;
    if (!read_uint32(file, &num_datas)) goto error;
    if (!read_uint32(file, &num_procs)) goto error;
    if (!read_uint32(file, &num_closures)) goto error;

    module = pz_module_init(num_structs, num_datas, num_procs, num_closures,
            entry_closure);

    if (!read_imports(file, num_imports, pz, &imported, filename)) goto error;

    if (!read_structs(file, num_structs, module, filename, verbose)) goto error;

    /*
     * read the file in two passes.  During the first pass we calculate the
     * sizes of datas and procedures and therefore calculating the addresses
     * where each individual entry begins.  Then in the second pass we fill
     * read the bytecode and data, resolving any intra-module references.
     */
    if (!read_data(file, num_datas, module, filename, verbose)) goto error;
    if (!read_code(file, num_procs, module, &imported, filename, verbose))
    {
        goto error;
    }

    if (!read_closures(file, num_closures, &imported, module, filename,
            verbose))
    {
        goto error;
    }

    if (imported.imports) {
        free(imported.imports);
        imported.imports = NULL;
    }

    /*
     * We should now be at the end of the file, so we should expect to get
     * an error if we read any further.
     */
    read_uint8(file, &extra_byte);
    if (0 == feof(file)) {
        fprintf(stderr, "%s: junk at end of file", filename);
        goto error;
    }

    fclose(file);
    return module;

error:
    if (ferror(file)) {
        perror(filename);
    } else if (feof(file)) {
        fprintf(stderr, "%s: Unexpected end of file.\n", filename);
    }
    fclose(file);
    if (imported.imports) {
        free(imported.imports);
    }
    if (module) {
        pz_module_free(module);
    }
    return NULL;
}

static bool
read_options(FILE *file, const char *filename, int32_t *entry_closure)
{
    uint16_t num_options;
    uint16_t type, len;
    uint32_t entry_closure_uint;

    if (!read_uint16(file, &num_options)) return false;

    for (unsigned i = 0; i < num_options; i++) {
        if (!read_uint16(file, &type)) return false;
        if (!read_uint16(file, &len)) return false;

        switch (type) {
            case PZ_OPT_ENTRY_CLOSURE:
                if (len != 4) {
                    fprintf(stderr, "%s: Corrupt file while reading options",
                            filename);
                    return false;
                }
                read_uint32(file, &entry_closure_uint);
                *entry_closure = (int32_t)entry_closure_uint;
                break;
            default:
                fseek(file, len, SEEK_CUR);
                break;
        }
    }

    return true;
}

static bool
read_imports(FILE        *file,
             unsigned     num_imports,
             PZ          *pz,
             PZ_Imported *imported,
             const char  *filename)
{
    PZ_Closure **closures = NULL;

    closures = malloc(sizeof(PZ_Closure *) * num_imports);

    for (uint32_t i = 0; i < num_imports; i++) {
        PZ_Module   *builtin_module;
        char        *module;
        char        *name;
        PZ_Closure  *closure;

        module = read_len_string(file);
        if (module == NULL) goto error;
        name = read_len_string(file);
        if (name == NULL) goto error;

        /*
         * Currently we don't support linking, only the builtin
         * pseudo-module is recognised.
         */
        if (strcmp("builtin", module) != 0) {
            fprintf(stderr, "Linking is not supported.\n");
        }
        builtin_module = pz_get_module(pz, "builtin");

        closure = pz_module_lookup_symbol(builtin_module, name);
        if (closure) {
            closures[i] = closure;
        } else {
            fprintf(stderr, "Procedure not found: %s.%s\n", module, name);
            free(module);
            free(name);
            goto error;
        }
        free(module);
        free(name);
    }

    imported->imports = closures;
    imported->num_imports = num_imports;
    return true;
error:
    if (closures != NULL) {
        free(closures);
    }
    return false;
}

static bool
read_structs(FILE       *file,
             unsigned    num_structs,
             PZ_Module  *module,
             const char *filename,
             bool        verbose)
{
    for (unsigned i = 0; i < num_structs; i++) {
        uint32_t   num_fields;
        PZ_Struct *s;

        if (!read_uint32(file, &num_fields)) return false;

        s = pz_module_get_struct(module, i);
        pz_struct_init(s, num_fields);

        for (unsigned j = 0; j < num_fields; j++) {
            uint8_t v;
            if (!read_uint8(file, &v)) return false;
            s->field_widths[j] = v;
        }

        pz_struct_calculate_layout(s);
    }

    return true;
}

static bool
read_data(FILE       *file,
          unsigned    num_datas,
          PZ_Module  *module,
          const char *filename,
          bool        verbose)
{
    unsigned  total_size = 0;
    void     *data = NULL;

    for (uint32_t i = 0; i < num_datas; i++) {
        uint8_t data_type_id;

        if (!read_uint8(file, &data_type_id)) goto error;
        switch (data_type_id) {
            case PZ_DATA_ARRAY: {
                unsigned  mem_width;
                uint16_t  num_elements;
                void     *data_ptr;
                if (!read_uint16(file, &num_elements)) goto error;
                if (!read_data_width(file, &mem_width)) goto error;
                data = pz_data_new_array_data(mem_width, num_elements);
                data_ptr = data;
                for (unsigned i = 0; i < num_elements; i++) {
                    if (!read_data_slot(file, data_ptr, module)) goto error;
                    data_ptr += mem_width;
                }
                total_size += mem_width * num_elements;
                break;
            }
            case PZ_DATA_STRUCT: {
                uint32_t   struct_id;
                PZ_Struct *struct_;

                if (!read_uint32(file, &struct_id)) goto error;
                struct_ = pz_module_get_struct(module, struct_id);
                data = pz_data_new_struct_data(struct_->total_size);
                for (unsigned f = 0; f < struct_->num_fields; f++) {
                    void *dest = data + struct_->field_offsets[f];
                    if (!read_data_slot(file, dest, module)) goto error;
                }
                break;
            }
        }

        pz_module_set_data(module, i, data);
        data = NULL;
    }

    if (verbose) {
        printf("Loaded %d data entries with a total of %d bytes\n",
               (unsigned)num_datas, total_size);
    }

    return true;

error:
    if (data != NULL) {
        pz_data_free(data);
    }
    return false;
}

static bool
read_data_width(FILE *file, unsigned *mem_width)
{
    uint8_t    raw_width;
    PZ_Width   width;

    if (!read_uint8(file, &raw_width)) return false;
    width = raw_width;
    *mem_width = pz_width_to_bytes(width);

    return true;
}

static bool
read_data_slot(FILE *file, void *dest, PZ_Module *module)
{
    uint8_t               enc_width, raw_enc;
    enum pz_data_enc_type type;

    if (!read_uint8(file, &raw_enc)) return false;
    type = PZ_DATA_ENC_TYPE(raw_enc);

    switch (type) {
        case pz_data_enc_type_normal:
            enc_width = PZ_DATA_ENC_BYTES(raw_enc);
            switch (enc_width) {
                case 1: {
                    uint8_t value;
                    if (!read_uint8(file, &value)) return false;
                    pz_data_write_normal_uint8(dest, value);
                    return true;
                }
                case 2: {
                    uint16_t value;
                    if (!read_uint16(file, &value)) return false;
                    pz_data_write_normal_uint16(dest, value);
                    return true;
                }
                case 4: {
                    uint32_t value;
                    if (!read_uint32(file, &value)) return false;
                    pz_data_write_normal_uint32(dest, value);
                    return true;
                }
                case 8: {
                    uint64_t value;
                    if (!read_uint64(file, &value)) return false;
                    pz_data_write_normal_uint64(dest, value);
                    return true;
                }
                default:
                    fprintf(stderr, "Unexpected data encoding %d.\n",
                            raw_enc);
                    return false;
            }
        case pz_data_enc_type_ptr: {
            uint32_t ref;
            void **  dest_ = (void **)dest;
            void *   data;

            // Data is a reference, link in the correct information.
            // XXX: support non-data references, such as proc
            // references.
            if (!read_uint32(file, &ref)) return false;
            data = pz_module_get_data(module, ref);
            if (data != NULL) {
                *dest_ = data;
            } else {
                fprintf(stderr, "forward references arn't yet supported.\n");
                abort();
            }
            return true;
        }
        case pz_data_enc_type_fast: {
            uint32_t i32;

            /*
             * For these width types the encoded width is 32bit.
             */
            if (!read_uint32(file, &i32)) return false;
            pz_data_write_fast_from_int32(dest, i32);
            return true;
        }
        case pz_data_enc_type_wptr: {
            int32_t i32;

            /*
             * For these width types the encoded width is 32bit.
             */
            if (!read_uint32(file, (uint32_t *)&i32)) return false;
            pz_data_write_wptr(dest, (uintptr_t)i32);
            return true;
        }
        case pz_data_enc_type_global_env:
            pz_data_write_wptr(dest,
                    (uintptr_t)pz_module_get_global_env(module));
            return true;
        default:
            // GCC is having trouble recognising this complete switch.
            fprintf(stderr, "Internal error.\n");
            abort();
    }
}

static bool
read_code(FILE        *file,
          unsigned     num_procs,
          PZ_Module   *module,
          PZ_Imported *imported,
          const char  *filename,
          bool         verbose)
{
    bool       result = false;
    unsigned **block_offsets = malloc(sizeof(unsigned *) * num_procs);
    long       file_pos;

    memset(block_offsets, 0, sizeof(unsigned *) * num_procs);

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

    for (unsigned i = 0; i < num_procs; i++) {
        unsigned proc_size;
        PZ_Proc *proc;

        if (verbose) {
            fprintf(stderr, "Reading proc %d\n", i);
        }

        proc_size =
          read_proc(file, imported, module, NULL, &block_offsets[i]);
        if (proc_size == 0) goto end;
        proc = pz_proc_init(proc_size);
        pz_module_set_proc(module, i, proc);
    }

    /*
     * Now that we've allocated memory for all the procedures, re-read them
     * this time writing them into that memory.  We do this for all the
     * procedures at once otherwise calls in earlier procedures would not
     * know the code addresses of later procedures.
     */
    if (verbose) {
        fprintf(stderr, "Beginning second pass\n");
    }
    if (0 != fseek(file, file_pos, SEEK_SET)) goto end;
    for (unsigned i = 0; i < num_procs; i++) {
        if (verbose) {
            fprintf(stderr, "Reading proc %d\n", i);
        }

        if (0 == read_proc(file, imported, module,
                           pz_module_get_proc_code(module, i),
                           &block_offsets[i]))
        {
            goto end;
        }
    }

    if (verbose) {
        pz_module_print_loaded_stats(module);
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
read_proc(FILE        *file,
          PZ_Imported *imported,
          PZ_Module   *module,
          uint8_t     *proc_code,
          unsigned   **block_offsets)
{
    uint32_t num_blocks;
    bool     first_pass = (proc_code == NULL);
    unsigned proc_offset = 0;

    /*
     * XXX: Signatures currently aren't written into the bytecode, but
     * here's where they might appear.
     */

    if (!read_uint32(file, &num_blocks)) return 0;
    if (first_pass) {
        /*
         * This is the first pass - set up the block offsets array.
         */
        *block_offsets = malloc(sizeof(unsigned) * num_blocks);
    }

    for (unsigned i = 0; i < num_blocks; i++) {
        uint32_t num_instructions;

        if (first_pass) {
            /*
             * Fill in the block_offsets array
             */
            (*block_offsets)[i] = proc_offset;
        }

        if (!read_uint32(file, &num_instructions)) return 0;
        for (uint32_t j = 0; j < num_instructions; j++) {
            uint8_t            byte;
            PZ_Opcode          opcode;
            PZ_Width           width1 = 0, width2 = 0;
            PZ_Immediate_Type  immediate_type;
            PZ_Immediate_Value immediate_value;

            /*
             * Read the opcode and the data width(s)
             */
            if (!read_uint8(file, &byte)) return 0;
            opcode = byte;
            if (pz_instruction_info_data[opcode].ii_num_width_bytes > 0) {
                if (!read_uint8(file, &byte)) return 0;
                width1 = byte;
                if (pz_instruction_info_data[opcode].ii_num_width_bytes
                        > 1)
                {
                    if (!read_uint8(file, &byte)) return 0;
                    width2 = byte;
                }
            }

            /*
             * Read any immediate value
             */
            immediate_type =
                pz_instruction_info_data[opcode].ii_immediate_type;
            switch (immediate_type) {
                case PZ_IMT_NONE:
                    memset(&immediate_value, 0, sizeof(PZ_Immediate_Value));
                    break;
                case PZ_IMT_8:
                    if (!read_uint8(file, &immediate_value.uint8)) return 0;
                    break;
                case PZ_IMT_16:
                    if (!read_uint16(file, &immediate_value.uint16))
                        return 0;
                    break;
                case PZ_IMT_32:
                    if (!read_uint32(file, &immediate_value.uint32))
                        return 0;
                    break;
                case PZ_IMT_64:
                    if (!read_uint64(file, &immediate_value.uint64))
                        return 0;
                    break;
                case PZ_IMT_CODE_REF: {
                    uint32_t imm32;
                    if (!read_uint32(file, &imm32)) return 0;

                    uint32_t tag, real;
                    tag = PZ_ID_GET_TAG(imm32);
                    real = PZ_ID_GET_REAL(imm32);
                    switch (tag) {
                        case PZ_ID_IMPORTED: {
                            assert(real < imported->num_imports);
                            if (opcode == PZI_CALL) {
                                opcode = PZI_CALL_CLOSURE;
                            } else {
                                assert(opcode == PZI_LOAD_IMMEDIATE_CODE);
                            }
                            immediate_value.word =
                              (uintptr_t)imported->imports[real];
                            break;
                        }
                        case PZ_ID_LOCAL:
                            if (!first_pass) {
                                immediate_value.word =
                                  (uintptr_t)pz_module_get_proc_code(module,
                                                                     real);
                            } else {
                                immediate_value.word = 0;
                            }
                            break;
                    }
                    break;
                }
                case PZ_IMT_LABEL_REF: {
                    uint32_t imm32;
                    if (!read_uint32(file, &imm32)) return 0;
                    if (!first_pass) {
                        immediate_value.word =
                          (uintptr_t)&proc_code[(*block_offsets)[imm32]];
                    } else {
                        immediate_value.word = 0;
                    }
                    break;
                }
                case PZ_IMT_DATA_REF: {
                    uint32_t imm32;
                    if (!read_uint32(file, &imm32)) return 0;
                    immediate_value.word =
                      (uintptr_t)pz_module_get_data(module, imm32);
                    break;
                }
                case PZ_IMT_STRUCT_REF: {
                    uint32_t   imm32;
                    PZ_Struct *struct_;
                    if (!read_uint32(file, &imm32)) return 0;
                    struct_ = pz_module_get_struct(module, imm32);
                    immediate_value.word = struct_->total_size;
                    break;
                }
                case PZ_IMT_STRUCT_REF_FIELD: {
                    uint32_t   imm32;
                    uint8_t    imm8;
                    PZ_Struct *struct_;

                    if (!read_uint32(file, &imm32)) return 0;
                    if (!read_uint8(file, &imm8)) return 0;
                    struct_ = pz_module_get_struct(module, imm32);

                    immediate_value.uint16 = struct_->field_offsets[imm8];
                    break;
                }
            }

            proc_offset =
              pz_write_instr(proc_code, proc_offset, opcode, width1, width2,
                             immediate_type, immediate_value);
        }
    }

    return proc_offset;
}

static bool
read_closures(FILE        *file,
              unsigned     num_closures,
              PZ_Imported *imported,
              PZ_Module   *module,
              const char  *filename,
              bool         verbose)
{
    for (unsigned i = 0; i < num_closures; i++) {
        uint32_t    proc_id;
        uint32_t    data_id;
        uint8_t    *proc_code;
        void       *data;
        PZ_Closure *closure;

        if (!read_uint32(file, &proc_id)) return false;
        assert(PZ_ID_GET_TAG(proc_id) == PZ_ID_LOCAL);
        proc_code = pz_module_get_proc_code(module, PZ_ID_GET_REAL(proc_id));

        if (!read_uint32(file, &data_id)) return false;
        data = pz_module_get_data(module, data_id);

        closure = pz_init_closure(proc_code, data);
        pz_module_set_closure(module, i, closure);
    }

    return true;
}

