/*
 * Plasma builtins
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2015-2018 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#include "pz_common.h"

#include "pz_builtin.h"
#include "pz_closure.h"
#include "pz_code.h"
#include "pz_interp.h"
#include "pz_util.h"

namespace pz {

static void
builtin_create(Module *module, const std::string &name,
        unsigned (*func_make_instrs)(uint8_t *bytecode, void *data),
        void *data);

static void
builtin_create_c_code(Module *module, const char *name,
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
      write_instr(bytecode, offset, PZI_OR, PZW_PTR, 0, PZ_IMT_NONE, imm);
    offset = write_instr(bytecode, offset, PZI_RET, 0, 0, PZ_IMT_NONE, imm);

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
    offset = write_instr(bytecode, offset, PZI_ROLL,
                PZW_PTR, 0, PZ_IMT_8, imm);
    imm.uint8 = num_tag_bits;
    offset = write_instr(bytecode, offset, PZI_LOAD_IMMEDIATE_NUM,
                PZW_PTR, 0, PZ_IMT_8, imm);
    offset = write_instr(bytecode, offset, PZI_LSHIFT,
                PZW_PTR, 0, PZ_IMT_NONE, imm);
    offset = write_instr(bytecode, offset, PZI_OR,
                PZW_PTR, 0, PZ_IMT_NONE, imm);
    offset = write_instr(bytecode, offset, PZI_RET,
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
    offset = write_instr(bytecode, offset, PZI_PICK,
            0, 0, PZ_IMT_8, imm);

    // Make pointer
    imm.uint32 = ~0 ^ tag_bits;
    offset = write_instr(bytecode, offset, PZI_LOAD_IMMEDIATE_NUM,
            PZW_32, 0, PZ_IMT_32, imm);
    if (MACHINE_WORD_SIZE == 8) {
        offset = write_instr(bytecode, offset, PZI_SE,
                PZW_32, PZW_64, PZ_IMT_NONE, imm);
    }
    offset = write_instr(bytecode, offset, PZI_AND,
            PZW_PTR, 0, PZ_IMT_NONE, imm);

    imm.uint8 = 2;
    offset = write_instr(bytecode, offset, PZI_ROLL,
            0, 0, PZ_IMT_8, imm);

    // Make tag.
    imm.uint32 = tag_bits;
    offset = write_instr(bytecode, offset, PZI_LOAD_IMMEDIATE_NUM,
            PZW_PTR, 0, PZ_IMT_32, imm);
    offset = write_instr(bytecode, offset, PZI_AND,
            PZW_PTR, 0, PZ_IMT_NONE, imm);

    offset = write_instr(bytecode, offset, PZI_RET,
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
    offset = write_instr(bytecode, offset, PZI_PICK,
            0, 0, PZ_IMT_8, imm);

    // Make word
    imm.uint32 = ~0 ^ tag_bits;
    offset = write_instr(bytecode, offset, PZI_LOAD_IMMEDIATE_NUM,
            PZW_32, 0, PZ_IMT_32, imm);
    if (MACHINE_WORD_SIZE == 8) {
        offset = write_instr(bytecode, offset, PZI_SE,
                PZW_32, PZW_64, PZ_IMT_NONE, imm);
    }
    offset = write_instr(bytecode, offset, PZI_AND,
            PZW_PTR, 0, PZ_IMT_NONE, imm);
    imm.uint8 = num_tag_bits;
    offset = write_instr(bytecode, offset, PZI_LOAD_IMMEDIATE_NUM,
            PZW_PTR, 0, PZ_IMT_8, imm);
    offset = write_instr(bytecode, offset, PZI_RSHIFT,
            PZW_PTR, 0, PZ_IMT_NONE, imm);

    imm.uint8 = 2;
    offset = write_instr(bytecode, offset, PZI_ROLL,
            0, 0, PZ_IMT_8, imm);

    // Make tag.
    imm.uint32 = tag_bits;
    offset = write_instr(bytecode, offset, PZI_LOAD_IMMEDIATE_NUM,
            PZW_PTR, 0, PZ_IMT_32, imm);
    offset = write_instr(bytecode, offset, PZI_AND,
            PZW_PTR, 0, PZ_IMT_NONE, imm);

    offset = write_instr(bytecode, offset, PZI_RET,
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

    imm.uint8 = num_tag_bits;
    offset = write_instr(bytecode, offset, PZI_LOAD_IMMEDIATE_NUM,
            PZW_PTR, 0, PZ_IMT_8, imm);
    offset = write_instr(bytecode, offset, PZI_RSHIFT,
            PZW_PTR, 0, PZ_IMT_NONE, imm);

    offset = write_instr(bytecode, offset, PZI_RET,
            0, 0, PZ_IMT_NONE, imm);

    return offset;
}

Module *
setup_builtins(void)
{
    Module *module;

    module = new Module();

    builtin_create_c_code(module, "print", builtin_print_func);
    builtin_create_c_code(module, "int_to_string", builtin_int_to_string_func);
    builtin_create_c_code(module, "setenv", builtin_setenv_func);
    builtin_create_c_code(module, "gettimeofday", builtin_gettimeofday_func);
    builtin_create_c_code(module, "concat_string", builtin_concat_string_func);
    builtin_create_c_code(module, "die", builtin_die_func);
    builtin_create_c_code(module, "set_parameter",
            builtin_set_parameter_func);

    builtin_create(module, "make_tag", builtin_make_tag_instrs, NULL);
    builtin_create(module, "shift_make_tag", builtin_shift_make_tag_instrs,
            NULL);
    builtin_create(module, "break_tag", builtin_break_tag_instrs, NULL);
    builtin_create(module, "break_shift_tag", builtin_break_shift_tag_instrs,
            NULL);
    builtin_create(module, "unshift_value", builtin_unshift_value_instrs,
            NULL);

    return module;
}

static void
builtin_create(Module *module, const std::string &name,
        unsigned (*func_make_instrs)(uint8_t *bytecode, void *data), void *data)
{
    PZ_Closure     *closure;
    Proc           *proc;
    unsigned        size;

    size = func_make_instrs(NULL, NULL);
    proc = new Proc(size);
    func_make_instrs(proc->code(), data);

    closure = pz_init_closure(proc->code(), NULL);

    module->add_symbol(name, closure);
}

static void
builtin_create_c_code(Module *module, const char *name,
        unsigned (*c_func)(void *stack, unsigned sp, PZ_Heap *heap))
{
    builtin_create(module, name, make_ccall_instr,
            reinterpret_cast<void*>(c_func));
}

static unsigned
make_ccall_instr(uint8_t *bytecode, void *c_func)
{
    PZ_Immediate_Value immediate_value;
    unsigned offset = 0;

    immediate_value.word = (uintptr_t)c_func;
    offset += write_instr(bytecode, offset, PZI_CCALL, 0, 0, PZ_IMT_CODE_REF,
            immediate_value);
    offset += write_instr(bytecode, offset, PZI_RET, 0, 0, PZ_IMT_NONE,
            immediate_value);

    return offset;
}

}

