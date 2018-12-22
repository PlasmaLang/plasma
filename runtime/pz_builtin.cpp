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
#include "pz_gc_rooting.h"
#include "pz_interp.h"
#include "pz_util.h"

namespace pz {

template<typename T>
static void
builtin_create(Module *module, const std::string &name,
        unsigned (*func_make_instrs)(uint8_t *bytecode, T data), T data,
        Heap *heap);

static void
builtin_create_c_code(Module *module, const char *name,
        pz_builtin_c_func c_func, Heap *heap);

static void
builtin_create_c_code_alloc(Module *module, const char *name,
        pz_builtin_c_alloc_func c_func, Heap *heap);

static unsigned
make_ccall_instr(uint8_t *bytecode, pz_builtin_c_func c_func);

static unsigned
make_ccall_alloc_instr(uint8_t *bytecode, pz_builtin_c_alloc_func c_func);

static unsigned
builtin_make_tag_instrs(uint8_t *bytecode, std::nullptr_t data)
{
    unsigned           offset = 0;

    /*
     * Take a word and a primary tag and combine them, this is pretty
     * simple.
     *
     * ptr tag - tagged_ptr
     */
    offset = write_instr(bytecode, offset, PZI_OR, PZW_PTR);
    offset = write_instr(bytecode, offset, PZI_RET);

    return offset;
}

static unsigned
builtin_shift_make_tag_instrs(uint8_t *bytecode, std::nullptr_t data)
{
    unsigned           offset = 0;
    PZ_Immediate_Value imm = {.word = 0 };

    /*
     * Take a word shift it left and combine it with a primary tag.
     *
     * word tag - tagged_word
     */
    imm.uint8 = 2;
    offset = write_instr(bytecode, offset, PZI_ROLL, PZ_IMT_8, imm);
    imm.uint8 = num_tag_bits;
    offset = write_instr(bytecode, offset, PZI_LOAD_IMMEDIATE_NUM,
                PZW_PTR, PZ_IMT_8, imm);
    offset = write_instr(bytecode, offset, PZI_LSHIFT, PZW_PTR);
    offset = write_instr(bytecode, offset, PZI_OR, PZW_PTR);
    offset = write_instr(bytecode, offset, PZI_RET);

    return offset;
}

static unsigned
builtin_break_tag_instrs(uint8_t *bytecode, std::nullptr_t data)
{
    unsigned           offset = 0;
    PZ_Immediate_Value imm = {.word = 0 };

    /*
     * Take a tagged pointer and break it into the original pointer and tag.
     *
     * tagged_ptr - ptr tag
     */
    imm.uint8 = 1;
    offset = write_instr(bytecode, offset, PZI_PICK, PZ_IMT_8, imm);

    // Make pointer
    imm.uint32 = ~0 ^ tag_bits;
    offset = write_instr(bytecode, offset, PZI_LOAD_IMMEDIATE_NUM,
            PZW_32, PZ_IMT_32, imm);
    if (MACHINE_WORD_SIZE == 8) {
        offset = write_instr(bytecode, offset, PZI_SE, PZW_32, PZW_64);
    }
    offset = write_instr(bytecode, offset, PZI_AND, PZW_PTR);

    imm.uint8 = 2;
    offset = write_instr(bytecode, offset, PZI_ROLL, PZ_IMT_8, imm);

    // Make tag.
    imm.uint32 = tag_bits;
    offset = write_instr(bytecode, offset, PZI_LOAD_IMMEDIATE_NUM,
            PZW_PTR, PZ_IMT_32, imm);
    offset = write_instr(bytecode, offset, PZI_AND, PZW_PTR);

    offset = write_instr(bytecode, offset, PZI_RET);

    return offset;
}

static unsigned
builtin_break_shift_tag_instrs(uint8_t *bytecode, std::nullptr_t data)
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
    offset = write_instr(bytecode, offset, PZI_PICK, PZ_IMT_8, imm);

    // Make word
    imm.uint32 = ~0 ^ tag_bits;
    offset = write_instr(bytecode, offset, PZI_LOAD_IMMEDIATE_NUM,
            PZW_32, PZ_IMT_32, imm);
    if (MACHINE_WORD_SIZE == 8) {
        offset = write_instr(bytecode, offset, PZI_SE, PZW_32, PZW_64);
    }
    offset = write_instr(bytecode, offset, PZI_AND, PZW_PTR);
    imm.uint8 = num_tag_bits;
    offset = write_instr(bytecode, offset, PZI_LOAD_IMMEDIATE_NUM,
            PZW_PTR, PZ_IMT_8, imm);
    offset = write_instr(bytecode, offset, PZI_RSHIFT, PZW_PTR);

    imm.uint8 = 2;
    offset = write_instr(bytecode, offset, PZI_ROLL, PZ_IMT_8, imm);

    // Make tag.
    imm.uint32 = tag_bits;
    offset = write_instr(bytecode, offset, PZI_LOAD_IMMEDIATE_NUM,
            PZW_PTR, PZ_IMT_32, imm);
    offset = write_instr(bytecode, offset, PZI_AND, PZW_PTR);

    offset = write_instr(bytecode, offset, PZI_RET);

    return offset;
}

static unsigned
builtin_unshift_value_instrs(uint8_t *bytecode, std::nullptr_t data)
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
            PZW_PTR, PZ_IMT_8, imm);
    offset = write_instr(bytecode, offset, PZI_RSHIFT, PZW_PTR);

    offset = write_instr(bytecode, offset, PZI_RET);

    return offset;
}

Module *
setup_builtins(Heap *heap)
{
    Module *module;

    module = new Module();

    builtin_create_c_code(module,       "print",
            pz_builtin_print_func,         heap);
    builtin_create_c_code_alloc(module, "int_to_string",
            pz_builtin_int_to_string_func, heap);
    builtin_create_c_code(module,       "setenv",
            pz_builtin_setenv_func,        heap);
    builtin_create_c_code(module,       "gettimeofday",
            pz_builtin_gettimeofday_func,  heap);
    builtin_create_c_code_alloc(module, "concat_string",
            pz_builtin_concat_string_func, heap);
    builtin_create_c_code(module,       "die",
            pz_builtin_die_func,           heap);
    builtin_create_c_code_alloc(module, "set_parameter",
            pz_builtin_set_parameter_func, heap);

    builtin_create<std::nullptr_t>(module, "make_tag",
            builtin_make_tag_instrs,        nullptr, heap);
    builtin_create<std::nullptr_t>(module, "shift_make_tag",
            builtin_shift_make_tag_instrs,  nullptr, heap);
    builtin_create<std::nullptr_t>(module, "break_tag",
            builtin_break_tag_instrs,       nullptr, heap);
    builtin_create<std::nullptr_t>(module, "break_shift_tag",
            builtin_break_shift_tag_instrs, nullptr, heap);
    builtin_create<std::nullptr_t>(module, "unshift_value",
            builtin_unshift_value_instrs,   nullptr, heap);

    return module;
}

template<typename T>
static void
builtin_create(Module *module, const std::string &name,
        unsigned (*func_make_instrs)(uint8_t *bytecode, T data), T data,
        Heap *heap)
{
    Tracer           tracer;
    Root<PZ_Closure> closure(tracer);
    Root<Proc>       proc(tracer);
    unsigned         size;

    size = func_make_instrs(nullptr, nullptr);
    proc = new Proc(heap, tracer, size);
    func_make_instrs(proc->code(), data);

    closure = pz_init_closure(heap, proc->code(), nullptr,
        Traceable::trace, (void*)&tracer);

    // After this call the item is rooted through the module and we can exit
    // tracer's scope.
    module->add_symbol(name, closure.get());
}

static void
builtin_create_c_code(Module *module, const char *name,
        pz_builtin_c_func c_func, Heap *heap)
{
    builtin_create<pz_builtin_c_func>(module, name, make_ccall_instr,
            c_func, heap);
}

static void
builtin_create_c_code_alloc(Module *module, const char *name,
        pz_builtin_c_alloc_func c_func, Heap *heap)
{
    builtin_create<pz_builtin_c_alloc_func>(module, name,
            make_ccall_alloc_instr, c_func, heap);
}

static unsigned
make_ccall_instr(uint8_t *bytecode, pz_builtin_c_func c_func)
{
    PZ_Immediate_Value immediate_value;
    unsigned offset = 0;

    immediate_value.word = (uintptr_t)c_func;
    offset += write_instr(bytecode, offset, PZI_CCALL,
            PZ_IMT_CODE_REF, immediate_value);
    offset += write_instr(bytecode, offset, PZI_RET);

    return offset;
}

static unsigned
make_ccall_alloc_instr(uint8_t *bytecode, pz_builtin_c_alloc_func c_func)
{
    PZ_Immediate_Value immediate_value;
    unsigned offset = 0;

    immediate_value.word = (uintptr_t)c_func;
    offset += write_instr(bytecode, offset, PZI_CCALL_ALLOC,
            PZ_IMT_CODE_REF, immediate_value);
    offset += write_instr(bytecode, offset, PZI_RET);

    return offset;
}

}

