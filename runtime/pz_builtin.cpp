/*
 * Plasma builtins
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2015-2018 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#include "pz_common.h"

#include "pz_builtin.h"
#include "pz_code.h"
#include "pz_interp.h"
#include "pz_util.h"

static void
builtin_create(PZ_Module *module, const char *name,
        unsigned (*func_make_instrs)(uint8_t *bytecode, void *data),
        void *data);

static void
builtin_create_c_code(PZ_Module *module, const char *name,
        unsigned (*c_func)(void *stack, unsigned sp, PZ_Heap *));

static unsigned
make_ccall_instr(uint8_t *bytecode, void *c_func);

static unsigned
builtin_make_tag_instrs(uint8_t *bytecode, void *data)
{
    unsigned           offset = 0;
    PZ_Immediate_Value imm = {.word = 0 };

    /*
     * Take a word and a primary tag and combine them, this is pretty
     * simple.
     *
     * ptr tag - tagged_ptr
     */
    offset =
      pz_write_instr(bytecode, offset, PZI_OR, PZW_PTR, 0, PZ_IMT_NONE, imm);
    offset = pz_write_instr(bytecode, offset, PZI_RET, 0, 0, PZ_IMT_NONE, imm);

    return offset;
}

static unsigned
builtin_shift_make_tag_instrs(uint8_t *bytecode, void *data)
{
    unsigned           offset = 0;
    PZ_Immediate_Value imm = {.word = 0 };

    /*
     * Take a word shift it left and combine it with a primary tag.
     *
     * word tag - tagged_word
     */
    imm.uint8 = 2;
    offset = pz_write_instr(bytecode, offset, PZI_ROLL,
                PZW_PTR, 0, PZ_IMT_8, imm);
    imm.uint8 = pz_num_tag_bits;
    offset = pz_write_instr(bytecode, offset, PZI_LOAD_IMMEDIATE_NUM,
                PZW_PTR, 0, PZ_IMT_8, imm);
    offset = pz_write_instr(bytecode, offset, PZI_LSHIFT,
                PZW_PTR, 0, PZ_IMT_NONE, imm);
    offset = pz_write_instr(bytecode, offset, PZI_OR,
                PZW_PTR, 0, PZ_IMT_NONE, imm);
    offset = pz_write_instr(bytecode, offset, PZI_RET,
                0, 0, PZ_IMT_NONE, imm);

    return offset;
}

static unsigned
builtin_break_tag_instrs(uint8_t *bytecode, void *data)
{
    unsigned           offset = 0;
    PZ_Immediate_Value imm = {.word = 0 };

    /*
     * Take a tagged pointer and break it into the original pointer and tag.
     *
     * tagged_ptr - ptr tag
     */
    imm.uint8 = 1;
    offset = pz_write_instr(bytecode, offset, PZI_PICK,
            0, 0, PZ_IMT_8, imm);

    // Make pointer
    imm.uint32 = ~0 ^ pz_tag_bits;
    offset = pz_write_instr(bytecode, offset, PZI_LOAD_IMMEDIATE_NUM,
            PZW_32, 0, PZ_IMT_32, imm);
    if (MACHINE_WORD_SIZE == 8) {
        offset = pz_write_instr(bytecode, offset, PZI_SE,
                PZW_32, PZW_64, PZ_IMT_NONE, imm);
    }
    offset = pz_write_instr(bytecode, offset, PZI_AND,
            PZW_PTR, 0, PZ_IMT_NONE, imm);

    imm.uint8 = 2;
    offset = pz_write_instr(bytecode, offset, PZI_ROLL,
            0, 0, PZ_IMT_8, imm);

    // Make tag.
    imm.uint32 = pz_tag_bits;
    offset = pz_write_instr(bytecode, offset, PZI_LOAD_IMMEDIATE_NUM,
            PZW_PTR, 0, PZ_IMT_32, imm);
    offset = pz_write_instr(bytecode, offset, PZI_AND,
            PZW_PTR, 0, PZ_IMT_NONE, imm);

    offset = pz_write_instr(bytecode, offset, PZI_RET,
            0, 0, PZ_IMT_NONE, imm);

    return offset;
}

static unsigned
builtin_break_shift_tag_instrs(uint8_t *bytecode, void *data)
{
    unsigned           offset = 0;
    PZ_Immediate_Value imm = {.word = 0 };

    /*
     * Take a tagged word and break it into the original word which is
     * shifted to the right and tag.
     *
     * tagged_word - word tag
     */
    imm.uint8 = 1;
    offset = pz_write_instr(bytecode, offset, PZI_PICK,
            0, 0, PZ_IMT_8, imm);

    // Make word
    imm.uint32 = ~0 ^ pz_tag_bits;
    offset = pz_write_instr(bytecode, offset, PZI_LOAD_IMMEDIATE_NUM,
            PZW_32, 0, PZ_IMT_32, imm);
    if (MACHINE_WORD_SIZE == 8) {
        offset = pz_write_instr(bytecode, offset, PZI_SE,
                PZW_32, PZW_64, PZ_IMT_NONE, imm);
    }
    offset = pz_write_instr(bytecode, offset, PZI_AND,
            PZW_PTR, 0, PZ_IMT_NONE, imm);
    imm.uint8 = pz_num_tag_bits;
    offset = pz_write_instr(bytecode, offset, PZI_LOAD_IMMEDIATE_NUM,
            PZW_PTR, 0, PZ_IMT_8, imm);
    offset = pz_write_instr(bytecode, offset, PZI_RSHIFT,
            PZW_PTR, 0, PZ_IMT_NONE, imm);

    imm.uint8 = 2;
    offset = pz_write_instr(bytecode, offset, PZI_ROLL,
            0, 0, PZ_IMT_8, imm);

    // Make tag.
    imm.uint32 = pz_tag_bits;
    offset = pz_write_instr(bytecode, offset, PZI_LOAD_IMMEDIATE_NUM,
            PZW_PTR, 0, PZ_IMT_32, imm);
    offset = pz_write_instr(bytecode, offset, PZI_AND,
            PZW_PTR, 0, PZ_IMT_NONE, imm);

    offset = pz_write_instr(bytecode, offset, PZI_RET,
            0, 0, PZ_IMT_NONE, imm);

    return offset;
}

static unsigned
builtin_unshift_value_instrs(uint8_t *bytecode, void *data)
{
    unsigned           offset = 0;
    PZ_Immediate_Value imm = {.word = 0 };

    /*
     * Take a word and shift it to the right to remove the tag.
     *
     * word - word
     */

    imm.uint8 = pz_num_tag_bits;
    offset = pz_write_instr(bytecode, offset, PZI_LOAD_IMMEDIATE_NUM,
            PZW_PTR, 0, PZ_IMT_8, imm);
    offset = pz_write_instr(bytecode, offset, PZI_RSHIFT,
            PZW_PTR, 0, PZ_IMT_NONE, imm);

    offset = pz_write_instr(bytecode, offset, PZI_RET,
            0, 0, PZ_IMT_NONE, imm);

    return offset;
}

PZ_Module *
pz_setup_builtins(void)
{
    PZ_Module *module;

    /*
     * We say that there are zero procs as procs are added differently, they
     * do not use the storage provided by PZ_Module.  TODO: maybe they
     * should?
     */
    module = pz_module_init(0, 0, 0, 0, 12, -1);

    // #1
    builtin_create_c_code(module, "print", builtin_print_func);
    // #2
    builtin_create_c_code(module, "int_to_string", builtin_int_to_string_func);
    // #3
    builtin_create_c_code(module, "setenv", builtin_setenv_func);
    // #4
    builtin_create_c_code(module, "gettimeofday", builtin_gettimeofday_func);
    // #5
    builtin_create_c_code(module, "concat_string", builtin_concat_string_func);
    // #6
    builtin_create_c_code(module, "die", builtin_die_func);
    // #7
    builtin_create_c_code(module, "set_parameter",
            builtin_set_parameter_func);

    // #8
    builtin_create(module, "make_tag", builtin_make_tag_instrs, NULL);
    // #9
    builtin_create(module, "shift_make_tag", builtin_shift_make_tag_instrs,
            NULL);
    // #10
    builtin_create(module, "break_tag", builtin_break_tag_instrs, NULL);
    // #11
    builtin_create(module, "break_shift_tag", builtin_break_shift_tag_instrs,
            NULL);
    // #12
    builtin_create(module, "unshift_value", builtin_unshift_value_instrs,
            NULL);

    return module;
}

static void
builtin_create(PZ_Module *module, const char *name,
        unsigned (*func_make_instrs)(uint8_t *bytecode, void *data), void *data)
{
    PZ_Closure     *closure;
    PZ_Proc        *proc;
    unsigned        size;

    size = func_make_instrs(NULL, NULL);
    proc = pz_proc_init(size);
    func_make_instrs(pz_proc_get_code(proc), data);

    closure = pz_init_closure(pz_proc_get_code(proc), NULL);

    pz_module_add_symbol(module, name, closure);
}

static void
builtin_create_c_code(PZ_Module *module, const char *name,
        unsigned (*c_func)(void *stack, unsigned sp, PZ_Heap *heap))
{
    builtin_create(module, name, make_ccall_instr, c_func);
}

static unsigned
make_ccall_instr(uint8_t *bytecode, void *c_func)
{
    PZ_Immediate_Value immediate_value;
    unsigned offset = 0;

    immediate_value.word = (uintptr_t)c_func;
    offset += pz_write_instr(bytecode, offset, PZI_CCALL, 0, 0, PZ_IMT_CODE_REF,
            immediate_value);
    offset += pz_write_instr(bytecode, offset, PZI_RET, 0, 0, PZ_IMT_NONE,
            immediate_value);

    return offset;
}

