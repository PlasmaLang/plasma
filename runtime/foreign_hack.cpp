/*
 * Plasma bytecode reader
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2021 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#include <sys/types.h>
#include <sys/unistd.h>

#include "pz_generic_run.h"

namespace pz {

unsigned builtin_getpid_func(void * void_stack, unsigned sp)
{
    StackValue * stack = static_cast<StackValue *>(void_stack);

    stack[++sp].u32 = getpid();

    return sp;
}


static unsigned make_ccall_instr(uint8_t * bytecode, pz_builtin_c_func c_func)
{
    ImmediateValue immediate_value;
    unsigned       offset = 0;

    immediate_value.word = (uintptr_t)c_func;
    offset +=
        write_instr(bytecode, offset, PZI_CCALL, IMT_PROC_REF, immediate_value);
    offset += write_instr(bytecode, offset, PZI_RET);

    return offset;
}

static void make_builtin(String name, pz_builtin_c_func c_func, GCTracer & gc,
        Root<Closure> &closure)
{
    unsigned size = make_ccall_instr(nullptr, nullptr);

    Root<Proc> proc(gc);
    {
        NoGCScope nogc(gc);
        proc = new (nogc) Proc(nogc, name, true, size);
        nogc.abort_if_oom("setting up builtins");
    }
    make_ccall_instr(proc->code(), c_func);

    closure = new (gc) Closure(proc->code(), nullptr);
}

void setup_hack(GCTracer & gc, Root<Closure> &closure)
{
    make_builtin(String("getpid"), builtin_getpid_func, gc, closure);
}

}

