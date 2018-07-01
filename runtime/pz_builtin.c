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
#include "pz_radix_tree.h"
#include "pz_run.h"
#include "pz_util.h"

static void
builtin_create(PZ_Module *module, const char *name,
        unsigned (*func_make_instrs)(uint8_t *bytecode));

static void
builtin_create_c_code(PZ_Module *module, const char *name,
        unsigned (*c_func)(void *stack, unsigned sp));

static unsigned
builtin_make_tag_instrs(uint8_t *bytecode)
{
    unsigned        offset = 0;
    Immediate_Value imm = {.word = 0 };

    /*
     * Take a word and a primary tag and combine them, this is pretty
     * simple.
     *
     * ptr tag - tagged_ptr
     */
    offset =
      pz_write_instr(bytecode, offset, PZI_OR, PZW_PTR, 0, IMT_NONE, imm);
    offset = pz_write_instr(bytecode, offset, PZI_RET, 0, 0, IMT_NONE, imm);

    return offset;
}

static unsigned
builtin_shift_make_tag_instrs(uint8_t *bytecode)
{
    unsigned        offset = 0;
    Immediate_Value imm = {.word = 0 };

    /*
     * Take a word shift it left and combine it with a primary tag.
     *
     * word tag - tagged_word
     */
    imm.uint8 = 2;
    offset = pz_write_instr(bytecode, offset, PZI_ROLL,
                PZW_PTR, 0, IMT_8, imm);
    imm.uint8 = pz_num_tag_bits;
    offset = pz_write_instr(bytecode, offset, PZI_LOAD_IMMEDIATE_NUM,
                PZW_PTR, 0, IMT_8, imm);
    offset = pz_write_instr(bytecode, offset, PZI_LSHIFT,
                PZW_PTR, 0, IMT_NONE, imm);
    offset = pz_write_instr(bytecode, offset, PZI_OR,
                PZW_PTR, 0, IMT_NONE, imm);
    offset = pz_write_instr(bytecode, offset, PZI_RET,
                0, 0, IMT_NONE, imm);

    return offset;
}

static unsigned
builtin_break_tag_instrs(uint8_t *bytecode)
{
    unsigned        offset = 0;
    Immediate_Value imm = {.word = 0 };

    /*
     * Take a tagged pointer and break it into the original pointer and tag.
     *
     * tagged_ptr - ptr tag
     */
    imm.uint8 = 1;
    offset = pz_write_instr(bytecode, offset, PZI_PICK,
            0, 0, IMT_8, imm);

    // Make pointer
    imm.uint32 = ~0 ^ pz_tag_bits;
    offset = pz_write_instr(bytecode, offset, PZI_LOAD_IMMEDIATE_NUM,
            PZW_32, 0, IMT_32, imm);
    if (MACHINE_WORD_SIZE == 8) {
        offset = pz_write_instr(bytecode, offset, PZI_SE,
                PZW_32, PZW_64, IMT_NONE, imm);
    }
    offset = pz_write_instr(bytecode, offset, PZI_AND,
            PZW_PTR, 0, IMT_NONE, imm);

    imm.uint8 = 2;
    offset = pz_write_instr(bytecode, offset, PZI_ROLL,
            0, 0, IMT_8, imm);

    // Make tag.
    imm.uint32 = pz_tag_bits;
    offset = pz_write_instr(bytecode, offset, PZI_LOAD_IMMEDIATE_NUM,
            PZW_PTR, 0, IMT_32, imm);
    offset = pz_write_instr(bytecode, offset, PZI_AND,
            PZW_PTR, 0, IMT_NONE, imm);

    offset = pz_write_instr(bytecode, offset, PZI_RET,
            0, 0, IMT_NONE, imm);

    return offset;
}

static unsigned
builtin_break_shift_tag_instrs(uint8_t *bytecode)
{
    unsigned        offset = 0;
    Immediate_Value imm = {.word = 0 };

    /*
     * Take a tagged word and break it into the original word which is
     * shifted to the right and tag.
     *
     * tagged_word - word tag
     */
    imm.uint8 = 1;
    offset = pz_write_instr(bytecode, offset, PZI_PICK,
            0, 0, IMT_8, imm);

    // Make word
    imm.uint32 = ~0 ^ pz_tag_bits;
    offset = pz_write_instr(bytecode, offset, PZI_LOAD_IMMEDIATE_NUM,
            PZW_32, 0, IMT_32, imm);
    if (MACHINE_WORD_SIZE == 8) {
        offset = pz_write_instr(bytecode, offset, PZI_SE,
                PZW_32, PZW_64, IMT_NONE, imm);
    }
    offset = pz_write_instr(bytecode, offset, PZI_AND,
            PZW_PTR, 0, IMT_NONE, imm);
    imm.uint8 = pz_num_tag_bits;
    offset = pz_write_instr(bytecode, offset, PZI_LOAD_IMMEDIATE_NUM,
            PZW_PTR, 0, IMT_8, imm);
    offset = pz_write_instr(bytecode, offset, PZI_RSHIFT,
            PZW_PTR, 0, IMT_NONE, imm);

    imm.uint8 = 2;
    offset = pz_write_instr(bytecode, offset, PZI_ROLL,
            0, 0, IMT_8, imm);

    // Make tag.
    imm.uint32 = pz_tag_bits;
    offset = pz_write_instr(bytecode, offset, PZI_LOAD_IMMEDIATE_NUM,
            PZW_PTR, 0, IMT_32, imm);
    offset = pz_write_instr(bytecode, offset, PZI_AND,
            PZW_PTR, 0, IMT_NONE, imm);

    offset = pz_write_instr(bytecode, offset, PZI_RET,
            0, 0, IMT_NONE, imm);

    return offset;
}

static unsigned
builtin_unshift_value_instrs(uint8_t *bytecode)
{
    unsigned        offset = 0;
    Immediate_Value imm = {.word = 0 };

    /*
     * Take a word and shift it to the right to remove the tag.
     *
     * word - word
     */

    imm.uint8 = pz_num_tag_bits;
    offset = pz_write_instr(bytecode, offset, PZI_LOAD_IMMEDIATE_NUM,
            PZW_PTR, 0, IMT_8, imm);
    offset = pz_write_instr(bytecode, offset, PZI_RSHIFT,
            PZW_PTR, 0, IMT_NONE, imm);

    offset = pz_write_instr(bytecode, offset, PZI_RET,
            0, 0, IMT_NONE, imm);

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
    module = pz_module_init(0, 0, 0, 0, -1);

    builtin_create_c_code(module, "print", builtin_print_func);
    builtin_create_c_code(module, "int_to_string", builtin_int_to_string_func);
    builtin_create_c_code(module, "free", builtin_free_func);
    builtin_create_c_code(module, "setenv", builtin_setenv_func);
    builtin_create_c_code(module, "gettimeofday", builtin_gettimeofday_func);
    builtin_create_c_code(module, "concat_string", builtin_concat_string_func);
    builtin_create_c_code(module, "die", builtin_die_func);

    builtin_create(module, "make_tag", builtin_make_tag_instrs);
    builtin_create(module, "shift_make_tag", builtin_shift_make_tag_instrs);
    builtin_create(module, "break_tag", builtin_break_tag_instrs);
    builtin_create(module, "break_shift_tag", builtin_break_shift_tag_instrs);
    builtin_create(module, "unshift_value", builtin_unshift_value_instrs);

    /*
     * TODO: Add the new builtins that are built from PZ instructions rather
     * than foreign code.
     *
     * TODO: they need to be loaded in the usual way and converted
     * to the runtime version of the bytecode.
     */

    return module;
}

static void
builtin_create(PZ_Module *module, const char *name,
        unsigned (*func_make_instrs)(uint8_t *bytecode))
{
    PZ_Proc_Symbol *proc;
    unsigned        size;

    size = func_make_instrs(NULL);

    proc = malloc(sizeof(PZ_Proc_Symbol));
    proc->type = PZ_BUILTIN_BYTECODE;
    proc->proc.bytecode = malloc(size);

    func_make_instrs(proc->proc.bytecode);

    pz_module_add_proc_symbol(module, name, proc);
}

static void
builtin_create_c_code(PZ_Module *module, const char *name,
        unsigned (*c_func)(void *stack, unsigned sp))
{
    PZ_Proc_Symbol *symbol = malloc(sizeof(PZ_Proc_Symbol));
    symbol->type = PZ_BUILTIN_C_FUNC;
    symbol->proc.c_func = c_func;

    pz_module_add_proc_symbol(module, name,  symbol);
}

