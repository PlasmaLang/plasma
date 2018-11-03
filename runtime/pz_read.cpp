/*
 * Plasma bytecode reader
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2015-2018 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#include <errno.h>
#include <string.h>

#include "pz_common.h"

#include "pz.h"
#include "pz_closure.h"
#include "pz_code.h"
#include "pz_data.h"
#include "pz_format.h"
#include "pz_interp.h"
#include "pz_io.h"
#include "pz_read.h"

namespace pz {

typedef struct {
    unsigned       num_imports;
    PZ_Closure   **import_closures;
    unsigned      *imports;
} PZ_Imported;

static bool
read_options(BinaryInput &file, int32_t *entry_closure);

static bool
read_imports(BinaryInput &file,
             unsigned     num_imports,
             PZ          &pz,
             PZ_Imported *imported);

static bool
read_structs(BinaryInput &file,
             unsigned     num_structs,
             Module      *module,
             bool         verbose);

static bool
read_data(BinaryInput &file,
          unsigned     num_datas,
          PZ          &pz,
          Module      *module,
          PZ_Imported *imports,
          bool         verbose);

static bool
read_data_width(BinaryInput &file, unsigned *mem_width);

static bool
read_data_slot(BinaryInput &file,
               void        *dest,
               PZ          &pz,
               Module      *module,
               PZ_Imported *imports);

static bool
read_code(BinaryInput &file,
          unsigned     num_procs,
          Module      *module,
          PZ_Imported *imported,
          bool         verbose);

static unsigned
read_proc(BinaryInput  &file,
          PZ_Imported  *imported,
          Module       *module,
          uint8_t      *proc_code,
          unsigned    **block_offsets);

static bool
read_closures(BinaryInput &file,
              unsigned     num_closures,
              PZ_Imported *imported,
              Module      *module,
              bool         verbose);

Module *
read(PZ &pz, const std::string &filename, bool verbose)
{
    BinaryInput  file;
    uint16_t     magic, version;
    int32_t      entry_closure = -1;
    uint32_t     num_imports;
    uint32_t     num_structs;
    uint32_t     num_datas;
    uint32_t     num_procs;
    uint32_t     num_closures;
    uint8_t      extra_byte;
    Module      *module = NULL;
    PZ_Imported  imported;

    imported.imports = NULL;

    if (!file.open(filename)) {
        perror(filename.c_str());
        return NULL;
    }

    if (!file.read_uint16(&magic)) goto error;
    if (magic != PZ_MAGIC_NUMBER) {
        fprintf(stderr, "%s: bad magic value, is this a PZ file?\n",
                filename.c_str());
        goto error;
    }

    {
        Optional<std::string> string = file.read_len_string();
        if (!string.hasValue()) goto error;
        if (!startsWith(string.value(), PZ_MAGIC_STRING_PART)) {
            fprintf(stderr, "%s: bad version string, is this a PZ file?\n",
                    filename.c_str());
            goto error;
        }
    }

    if (!file.read_uint16(&version)) goto error;
    if (version != PZ_FORMAT_VERSION) {
        fprintf(stderr, "Incorrect PZ version, found %d, expecting %d\n",
                version, PZ_FORMAT_VERSION);
        goto error;
    }

    if (!read_options(file, &entry_closure)) goto error;

    if (!file.read_uint32(&num_imports)) goto error;
    if (!file.read_uint32(&num_structs)) goto error;
    if (!file.read_uint32(&num_datas)) goto error;
    if (!file.read_uint32(&num_procs)) goto error;
    if (!file.read_uint32(&num_closures)) goto error;

    module = new Module(num_structs, num_datas, num_procs, num_closures,
            0, entry_closure);

    if (!read_imports(file, num_imports, pz, &imported)) goto error;

    if (!read_structs(file, num_structs, module, verbose)) goto error;

    /*
     * read the file in two passes.  During the first pass we calculate the
     * sizes of datas and procedures and therefore calculating the addresses
     * where each individual entry begins.  Then in the second pass we fill
     * read the bytecode and data, resolving any intra-module references.
     */
    if (!read_data(file, num_datas, pz, module, &imported, verbose)) {
        goto error;
    }
    if (!read_code(file, num_procs, module, &imported, verbose)) {
        goto error;
    }

    if (!read_closures(file, num_closures, &imported, module, verbose)) {
        goto error;
    }

    if (imported.imports) {
        free(imported.imports);
        imported.imports = NULL;
    }

#ifdef PZ_DEV
    /*
     * We should now be at the end of the file, so we should expect to get
     * an error if we read any further.
     */
    if (file.read_uint8(&extra_byte)) {
        fprintf(stderr, "%s: junk at end of file", filename.c_str());
        goto error;
    }
    if (!file.is_at_eof()) {
        fprintf(stderr, "%s: junk at end of file", filename.c_str());
        goto error;
    }
#endif
    file.close();

    return module;

error:
    if (imported.imports) {
        free(imported.imports);
    }
    if (module) {
        delete module;
    }
    return NULL;
}

static bool
read_options(BinaryInput &file, int32_t *entry_closure)
{
    uint16_t num_options;
    uint16_t type, len;
    uint32_t entry_closure_uint;

    if (!file.read_uint16(&num_options)) return false;

    for (unsigned i = 0; i < num_options; i++) {
        if (!file.read_uint16(&type)) return false;
        if (!file.read_uint16(&len)) return false;

        switch (type) {
            case PZ_OPT_ENTRY_CLOSURE:
                if (len != 4) {
                    fprintf(stderr, "%s: Corrupt file while reading options",
                            file.filename_c());
                    return false;
                }
                file.read_uint32(&entry_closure_uint);
                *entry_closure = (int32_t)entry_closure_uint;
                break;
            default:
                if (!file.seek_cur(len)) return false;
                break;
        }
    }

    return true;
}

static bool
read_imports(BinaryInput &file,
             unsigned     num_imports,
             PZ          &pz,
             PZ_Imported *imported)
{
    PZ_Closure **closures = NULL;
    unsigned *imports = NULL;

    closures = malloc(sizeof(PZ_Closure *) * num_imports);
    imports = malloc(sizeof(unsigned) * num_imports);

    for (uint32_t i = 0; i < num_imports; i++) {
        Optional<std::string> module = file.read_len_string();
        if (!module.hasValue()) goto error;
        Optional<std::string> name = file.read_len_string();
        if (!name.hasValue()) goto error;

        /*
         * Currently we don't support linking, only the builtin
         * pseudo-module is recognised.
         */
        if ("builtin" != module.value()) {
            fprintf(stderr, "Linking is not supported.\n");
        }

        Module *builtin_module = pz.lookup_module("builtin");

        Optional<unsigned> id =
            builtin_module->lookup_symbol(name.value().c_str());
        if (id.hasValue()) {
            imports[i] = id.value();
            closures[i] = builtin_module->export_(id.value());
        } else {
            fprintf(stderr, "Procedure not found: %s.%s\n", module, name);
            goto error;
        }
    }

    imported->imports = imports;
    imported->import_closures = closures;
    imported->num_imports = num_imports;
    return true;
error:
    if (closures != NULL) {
        free(closures);
    }
    if (imports != NULL) {
        free(imports);
    }
    return false;
}

static bool
read_structs(BinaryInput &file,
             unsigned     num_structs,
             Module      *module,
             bool         verbose)
{
    for (unsigned i = 0; i < num_structs; i++) {
        uint32_t   num_fields;

        if (!file.read_uint32(&num_fields)) return false;

        Struct& s = module->new_struct(num_fields);

        for (unsigned j = 0; j < num_fields; j++) {
            uint8_t v;
            if (!file.read_uint8(&v)) return false;
            s.set_field_width(j, v);
        }

        s.calculate_layout();
    }

    return true;
}

static bool
read_data(BinaryInput &file,
          unsigned     num_datas,
          PZ          &pz,
          Module      *module,
          PZ_Imported *imports,
          bool         verbose)
{
    unsigned  total_size = 0;
    void     *data = NULL;

    for (uint32_t i = 0; i < num_datas; i++) {
        uint8_t data_type_id;

        if (!file.read_uint8(&data_type_id)) goto error;
        switch (data_type_id) {
            case PZ_DATA_ARRAY: {
                unsigned  mem_width;
                uint16_t  num_elements;
                void     *data_ptr;
                if (!file.read_uint16(&num_elements)) goto error;
                if (!read_data_width(file, &mem_width)) goto error;
                data = data_new_array_data(mem_width, num_elements);
                data_ptr = data;
                for (unsigned i = 0; i < num_elements; i++) {
                    if (!read_data_slot(file, data_ptr, pz, module, imports)) {
                        goto error;
                    }
                    data_ptr += mem_width;
                }
                total_size += mem_width * num_elements;
                break;
            }
            case PZ_DATA_STRUCT: {
                uint32_t struct_id;
                if (!file.read_uint32(&struct_id)) goto error;
                const Struct &struct_ = module->struct_(struct_id);

                data = data_new_struct_data(struct_.total_size());
                for (unsigned f = 0; f < struct_.num_fields(); f++) {
                    void *dest = data + struct_.field_offset(f);
                    if (!read_data_slot(file, dest, pz, module, imports)) {
                        goto error;
                    }
                }
                break;
            }
        }

        module->add_data(data);
        data = NULL;
    }

    if (verbose) {
        printf("Loaded %d data entries with a total of %d bytes\n",
               (unsigned)num_datas, total_size);
    }

    return true;

error:
    if (data != NULL) {
        data_free(data);
    }
    return false;
}

static bool
read_data_width(BinaryInput &file, unsigned *mem_width)
{
    uint8_t    raw_width;
    PZ_Width   width;

    if (!file.read_uint8(&raw_width)) return false;
    width = raw_width;
    *mem_width = width_to_bytes(width);

    return true;
}

static bool
read_data_slot(BinaryInput &file, void *dest, PZ &pz, Module *module,
        PZ_Imported *imports)
{
    uint8_t               enc_width, raw_enc;
    enum pz_data_enc_type type;

    if (!file.read_uint8(&raw_enc)) return false;
    type = PZ_DATA_ENC_TYPE(raw_enc);

    switch (type) {
        case pz_data_enc_type_normal:
            enc_width = PZ_DATA_ENC_BYTES(raw_enc);
            switch (enc_width) {
                case 1: {
                    uint8_t value;
                    if (!file.read_uint8(&value)) return false;
                    data_write_normal_uint8(dest, value);
                    return true;
                }
                case 2: {
                    uint16_t value;
                    if (!file.read_uint16(&value)) return false;
                    data_write_normal_uint16(dest, value);
                    return true;
                }
                case 4: {
                    uint32_t value;
                    if (!file.read_uint32(&value)) return false;
                    data_write_normal_uint32(dest, value);
                    return true;
                }
                case 8: {
                    uint64_t value;
                    if (!file.read_uint64(&value)) return false;
                    data_write_normal_uint64(dest, value);
                    return true;
                }
                default:
                    fprintf(stderr, "Unexpected data encoding %d.\n",
                            raw_enc);
                    return false;
            }
        case pz_data_enc_type_fast: {
            uint32_t i32;

            /*
             * For these width types the encoded width is 32bit.
             */
            if (!file.read_uint32(&i32)) return false;
            data_write_fast_from_int32(dest, i32);
            return true;
        }
        case pz_data_enc_type_wptr: {
            int32_t i32;

            /*
             * For these width types the encoded width is 32bit.
             */
            if (!file.read_uint32((uint32_t *)&i32)) return false;
            data_write_wptr(dest, (uintptr_t)i32);
            return true;
        }
        case pz_data_enc_type_data: {
            uint32_t ref;
            void **  dest_ = (void **)dest;
            void *   data;

            // Data is a reference, link in the correct information.
            // XXX: support non-data references, such as proc
            // references.
            if (!file.read_uint32(&ref)) return false;
            data = module->data(ref);
            if (data != NULL) {
                *dest_ = data;
            } else {
                fprintf(stderr, "forward references arn't yet supported.\n");
                abort();
            }
            return true;
        }
        case pz_data_enc_type_import: {
            uint32_t    ref;
            void      **dest_ = (void **)dest;
            PZ_Closure *import;

            // Data is a reference, link in the correct information.
            // XXX: support non-data references, such as proc
            // references.
            if (!file.read_uint32(&ref)) return false;
            assert(ref < imports->num_imports);
            import = imports->import_closures[ref];
            assert(import);
            *dest_ = import;
            return true;
        }
        default:
            // GCC is having trouble recognising this complete switch.
            fprintf(stderr, "Unrecognised data item encoding.\n");
            abort();
    }
}

static bool
read_code(BinaryInput &file,
          unsigned     num_procs,
          Module      *module,
          PZ_Imported *imported,
          bool         verbose)
{
    bool             result = false;
    unsigned       **block_offsets = malloc(sizeof(unsigned *) * num_procs);

    memset(block_offsets, 0, sizeof(unsigned *) * num_procs);

    /*
     * We read procedures in two phases, once to calculate their sizes, and
     * label offsets, allocating memory for each one.  Then the we read them
     * for real in the second phase when memory locations are known.
     */
    if (verbose) {
        fprintf(stderr, "Reading procs first pass\n");
    }
    auto file_pos = file.tell();
    if (!file_pos.hasValue()) goto end;

    for (unsigned i = 0; i < num_procs; i++) {
        unsigned  proc_size;

        if (verbose) {
            fprintf(stderr, "Reading proc %d\n", i);
        }

        proc_size =
          read_proc(file, imported, module, NULL, &block_offsets[i]);
        if (proc_size == 0) goto end;
        module->new_proc(proc_size);
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
    if (!file.seek_set(file_pos.value())) goto end;
    for (unsigned i = 0; i < num_procs; i++) {
        if (verbose) {
            fprintf(stderr, "Reading proc %d\n", i);
        }

        if (0 == read_proc(file, imported, module,
                           module->proc(i).code(),
                           &block_offsets[i]))
        {
            goto end;
        }
    }

    if (verbose) {
        module->print_loaded_stats();
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
read_proc(BinaryInput &file,
          PZ_Imported *imported,
          Module      *module,
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

    if (!file.read_uint32(&num_blocks)) return 0;
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

        if (!file.read_uint32(&num_instructions)) return 0;
        for (uint32_t j = 0; j < num_instructions; j++) {
            uint8_t            byte;
            PZ_Opcode          opcode;
            PZ_Width           width1 = 0, width2 = 0;
            PZ_Immediate_Type  immediate_type;
            PZ_Immediate_Value immediate_value;

            /*
             * Read the opcode and the data width(s)
             */
            if (!file.read_uint8(&byte)) return 0;
            opcode = byte;
            if (pz_instruction_info_data[opcode].ii_num_width_bytes > 0) {
                if (!file.read_uint8(&byte)) return 0;
                width1 = byte;
                if (pz_instruction_info_data[opcode].ii_num_width_bytes
                        > 1)
                {
                    if (!file.read_uint8(&byte)) return 0;
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
                    if (!file.read_uint8(&immediate_value.uint8)) return 0;
                    break;
                case PZ_IMT_16:
                    if (!file.read_uint16(&immediate_value.uint16))
                        return 0;
                    break;
                case PZ_IMT_32:
                    if (!file.read_uint32(&immediate_value.uint32))
                        return 0;
                    break;
                case PZ_IMT_64:
                    if (!file.read_uint64(&immediate_value.uint64))
                        return 0;
                    break;
                case PZ_IMT_CODE_REF: {
                    uint32_t proc_id;
                    if (!file.read_uint32(&proc_id)) return 0;
                    if (!first_pass) {
                        immediate_value.word =
                          (uintptr_t)module->proc(proc_id).code();
                    } else {
                        immediate_value.word = 0;
                    }
                    break;
                }
                case PZ_IMT_IMPORT_REF:
                case PZ_IMT_IMPORT_CLOSURE_REF: {
                    uint32_t import_id;
                    if (!file.read_uint32(&import_id)) return 0;
                    if (immediate_type == PZ_IMT_IMPORT_REF) {
                        // TODO Should lookup the offset within the struct in
                        // case there's non-pointer sized things in there.
                        immediate_value.uint16 =
                                imported->imports[import_id] * sizeof(void*);
                    } else {
                        immediate_value.word =
                                (uintptr_t)imported->import_closures[import_id];
                    }
                    break;
                }
                case PZ_IMT_LABEL_REF: {
                    uint32_t imm32;
                    if (!file.read_uint32(&imm32)) return 0;
                    if (!first_pass) {
                        immediate_value.word =
                          (uintptr_t)&proc_code[(*block_offsets)[imm32]];
                    } else {
                        immediate_value.word = 0;
                    }
                    break;
                }
                case PZ_IMT_STRUCT_REF: {
                    uint32_t imm32;
                    if (!file.read_uint32(&imm32)) return 0;
                    immediate_value.word = module->struct_(imm32).total_size();
                    break;
                }
                case PZ_IMT_STRUCT_REF_FIELD: {
                    uint32_t   imm32;
                    uint8_t    imm8;

                    if (!file.read_uint32(&imm32)) return 0;
                    if (!file.read_uint8(&imm8)) return 0;
                    immediate_value.uint16 =
                        module->struct_(imm32).field_offset(imm8);
                    break;
                }
            }

            proc_offset =
              write_instr(proc_code, proc_offset, opcode, width1, width2,
                             immediate_type, immediate_value);
        }
    }

    return proc_offset;
}

static bool
read_closures(BinaryInput &file,
              unsigned     num_closures,
              PZ_Imported *imported,
              Module      *module,
              bool         verbose)
{
    for (unsigned i = 0; i < num_closures; i++) {
        uint32_t    proc_id;
        uint32_t    data_id;
        uint8_t    *proc_code;
        void       *data;
        PZ_Closure *closure;

        if (!file.read_uint32(&proc_id)) return false;
        proc_code = module->proc(proc_id).code();

        if (!file.read_uint32(&data_id)) return false;
        data = module->data(data_id);

        closure = pz_init_closure(proc_code, data);
        module->set_closure(closure);
    }

    return true;
}

} // namespace pz

