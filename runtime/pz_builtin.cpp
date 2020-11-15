/*
 * Plasma builtins
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2015-2020 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#include "pz_common.h"

#include "pz_builtin.h"
#include "pz_closure.h"
#include "pz_code.h"
#include "pz_gc_util.h"
#include "pz_interp.h"
#include "pz_util.h"

namespace pz {

template<typename T>
static void
builtin_create(Library *library, const char *name,
        unsigned (*func_make_instrs)(uint8_t *bytecode, T data), T data,
        GCCapability &gccap);

static void
builtin_create_c_code(Library *library, const char *name,
        pz_builtin_c_func c_func, GCCapability &gccap);

static void
builtin_create_c_code_alloc(Library *library, const char *name,
        pz_builtin_c_alloc_func c_func, GCCapability &gccap);

static void
builtin_create_c_code_special(Library *library, const char *name,
        pz_builtin_c_special_func c_func, GCCapability &gccap);

static unsigned
make_ccall_instr(uint8_t *bytecode, pz_builtin_c_func c_func);

static unsigned
make_ccall_alloc_instr(uint8_t *bytecode, pz_builtin_c_alloc_func c_func);

static unsigned
make_ccall_special_instr(uint8_t *bytecode, pz_builtin_c_special_func c_func);

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
    unsigned       offset = 0;
    ImmediateValue imm = {.word = 0 };

    /*
     * Take a word shift it left and combine it with a primary tag.
     *
     * word tag - tagged_word
     */
    imm.uint8 = 2;
    offset = write_instr(bytecode, offset, PZI_ROLL, IMT_8, imm);
    imm.uint8 = num_tag_bits;
    offset = write_instr(bytecode, offset, PZI_LOAD_IMMEDIATE_NUM,
                PZW_PTR, IMT_8, imm);
    offset = write_instr(bytecode, offset, PZI_LSHIFT, PZW_PTR);
    offset = write_instr(bytecode, offset, PZI_OR, PZW_PTR);
    offset = write_instr(bytecode, offset, PZI_RET);

    return offset;
}

static unsigned
builtin_break_tag_instrs(uint8_t *bytecode, std::nullptr_t data)
{
    unsigned       offset = 0;
    ImmediateValue imm = {.word = 0 };

    /*
     * Take a tagged pointer and break it into the original pointer and tag.
     *
     * tagged_ptr - ptr tag
     */
    imm.uint8 = 1;
    offset = write_instr(bytecode, offset, PZI_PICK, IMT_8, imm);

    // Make pointer
    imm.uint32 = ~0 ^ tag_bits;
    offset = write_instr(bytecode, offset, PZI_LOAD_IMMEDIATE_NUM,
            PZW_32, IMT_32, imm);
    if (WORDSIZE_BYTES == 8) {
        offset = write_instr(bytecode, offset, PZI_SE, PZW_32, PZW_64);
    }
    offset = write_instr(bytecode, offset, PZI_AND, PZW_PTR);

    imm.uint8 = 2;
    offset = write_instr(bytecode, offset, PZI_ROLL, IMT_8, imm);

    // Make tag.
    imm.uint32 = tag_bits;
    offset = write_instr(bytecode, offset, PZI_LOAD_IMMEDIATE_NUM,
            PZW_PTR, IMT_32, imm);
    offset = write_instr(bytecode, offset, PZI_AND, PZW_PTR);

    offset = write_instr(bytecode, offset, PZI_RET);

    return offset;
}

static unsigned
builtin_break_shift_tag_instrs(uint8_t *bytecode, std::nullptr_t data)
{
    unsigned       offset = 0;
    ImmediateValue imm = {.word = 0 };

    /*
     * Take a tagged word and break it into the original word which is
     * shifted to the right and tag.
     *
     * tagged_word - word tag
     */
    imm.uint8 = 1;
    offset = write_instr(bytecode, offset, PZI_PICK, IMT_8, imm);

    // Make word
    imm.uint32 = ~0 ^ tag_bits;
    offset = write_instr(bytecode, offset, PZI_LOAD_IMMEDIATE_NUM,
            PZW_32, IMT_32, imm);
    if (WORDSIZE_BYTES == 8) {
        offset = write_instr(bytecode, offset, PZI_SE, PZW_32, PZW_64);
    }
    offset = write_instr(bytecode, offset, PZI_AND, PZW_PTR);
    imm.uint8 = num_tag_bits;
    offset = write_instr(bytecode, offset, PZI_LOAD_IMMEDIATE_NUM,
            PZW_PTR, IMT_8, imm);
    offset = write_instr(bytecode, offset, PZI_RSHIFT, PZW_PTR);

    imm.uint8 = 2;
    offset = write_instr(bytecode, offset, PZI_ROLL, IMT_8, imm);

    // Make tag.
    imm.uint32 = tag_bits;
    offset = write_instr(bytecode, offset, PZI_LOAD_IMMEDIATE_NUM,
            PZW_PTR, IMT_32, imm);
    offset = write_instr(bytecode, offset, PZI_AND, PZW_PTR);

    offset = write_instr(bytecode, offset, PZI_RET);

    return offset;
}

static unsigned
builtin_unshift_value_instrs(uint8_t *bytecode, std::nullptr_t data)
{
    unsigned       offset = 0;
    ImmediateValue imm = {.word = 0 };

    /*
     * Take a word and shift it to the right to remove the tag.
     *
     * word - word
     */

    imm.uint8 = num_tag_bits;
    offset = write_instr(bytecode, offset, PZI_LOAD_IMMEDIATE_NUM,
            PZW_PTR, IMT_8, imm);
    offset = write_instr(bytecode, offset, PZI_RSHIFT, PZW_PTR);

    offset = write_instr(bytecode, offset, PZI_RET);

    return offset;
}

void
setup_builtins(Library *library, GCCapability &gccap)
{
    // clang-format off
    builtin_create_c_code(library,         "print",
            pz_builtin_print_func,          gccap);
    builtin_create_c_code_alloc(library,   "int_to_string",
            pz_builtin_int_to_string_func,  gccap);
    builtin_create_c_code(library,         "setenv",
            pz_builtin_setenv_func,         gccap);
    builtin_create_c_code(library,         "gettimeofday",
            pz_builtin_gettimeofday_func,   gccap);
    builtin_create_c_code_alloc(library,   "concat_string",
            pz_builtin_concat_string_func,  gccap);
    builtin_create_c_code(library,         "die",
            pz_builtin_die_func,            gccap);
    builtin_create_c_code_special(library, "set_parameter",
            pz_builtin_set_parameter_func,  gccap);
    builtin_create_c_code_special(library, "get_parameter",
            pz_builtin_get_parameter_func,  gccap);

    builtin_create<std::nullptr_t>(library, "make_tag",
            builtin_make_tag_instrs,        nullptr, gccap);
    builtin_create<std::nullptr_t>(library, "shift_make_tag",
            builtin_shift_make_tag_instrs,  nullptr, gccap);
    builtin_create<std::nullptr_t>(library, "break_tag",
            builtin_break_tag_instrs,       nullptr, gccap);
    builtin_create<std::nullptr_t>(library, "break_shift_tag",
            builtin_break_shift_tag_instrs, nullptr, gccap);
    builtin_create<std::nullptr_t>(library, "unshift_value",
            builtin_unshift_value_instrs,   nullptr, gccap);
    // clang-format on
}

template<typename T>
static void
builtin_create(Library *library, const char *name,
        unsigned (*func_make_instrs)(uint8_t *bytecode, T data), T data,
        GCCapability &gccap)
{
    // We forbid GC in this scope until the proc's code and closure are
    // reachable from module.  We will check for OOM before using any
    // allocation results and abort if we're OOM.
    NoGCScope nogc(&gccap);

    // If the proc code area cannot be allocated this is GC safe because it
    // will trace the closure.  It would not work the other way around (we'd
    // have to make it faliable).
    unsigned size = func_make_instrs(nullptr, nullptr);
    Proc *proc = new (nogc) Proc(nogc, name, true, size);

    nogc.abort_if_oom("setting up builtins");
    func_make_instrs(proc->code(), data);

    Closure *closure = new(nogc) Closure(proc->code(), nullptr);

    nogc.abort_if_oom("setting up builtins");
    // XXX: -1 is a temporary hack.
    library->add_symbol(std::string("Builtin.") + name, closure, (unsigned)-1);
}

static void
builtin_create_c_code(Library *library, const char *name,
        pz_builtin_c_func c_func, GCCapability &gccap)
{
    builtin_create<pz_builtin_c_func>(library, name, make_ccall_instr,
            c_func, gccap);
}

static void
builtin_create_c_code_alloc(Library *library, const char *name,
        pz_builtin_c_alloc_func c_func, GCCapability &gccap)
{
    builtin_create<pz_builtin_c_alloc_func>(library, name,
            make_ccall_alloc_instr, c_func, gccap);
}

static void
builtin_create_c_code_special(Library *library, const char *name,
        pz_builtin_c_special_func c_func, GCCapability &gccap)
{
    builtin_create<pz_builtin_c_special_func>(library, name,
            make_ccall_special_instr, c_func, gccap);
}

static unsigned
make_ccall_instr(uint8_t *bytecode, pz_builtin_c_func c_func)
{
    ImmediateValue immediate_value;
    unsigned       offset = 0;

    immediate_value.word = (uintptr_t)c_func;
    offset += write_instr(bytecode, offset, PZI_CCALL,
            IMT_PROC_REF, immediate_value);
    offset += write_instr(bytecode, offset, PZI_RET);

    return offset;
}

static unsigned
make_ccall_alloc_instr(uint8_t *bytecode, pz_builtin_c_alloc_func c_func)
{
    ImmediateValue immediate_value;
    unsigned       offset = 0;

    immediate_value.word = (uintptr_t)c_func;
    offset += write_instr(bytecode, offset, PZI_CCALL_ALLOC,
            IMT_PROC_REF, immediate_value);
    offset += write_instr(bytecode, offset, PZI_RET);

    return offset;
}

static unsigned
make_ccall_special_instr(uint8_t *bytecode,
        pz_builtin_c_special_func c_func)
{
    ImmediateValue immediate_value;
    unsigned       offset = 0;

    immediate_value.word = (uintptr_t)c_func;
    offset += write_instr(bytecode, offset, PZI_CCALL_SPECIAL,
            IMT_PROC_REF, immediate_value);
    offset += write_instr(bytecode, offset, PZI_RET);

    return offset;
}

}

