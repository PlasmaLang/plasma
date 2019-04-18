/*
 * Plasma bytecode exection (generic portable version)
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2015-2019 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#include "pz_common.h"

#include <stdio.h>
#include <string.h>

#include "pz_code.h"
#include "pz_cxx_future.h"
#include "pz.h"
#include "pz_interp.h"
#include "pz_trace.h"

#include "pz_generic_closure.h"
#include "pz_generic_run.h"

#define RETURN_STACK_SIZE 2048
#define EXPR_STACK_SIZE 2048

namespace pz {

/* Must match or exceed ptag_bits from src/core.types.m */
const unsigned  num_tag_bits = 2;
const uintptr_t tag_bits = 0x3;

/*
 * Run the program
 *
 ******************/

int
run(PZ &pz, const Options &options)
{
    Context            context;
    uint8_t           *wrapper_proc = nullptr;
    unsigned           wrapper_proc_size;
    int                retcode;
    ImmediateValue     imv_none;
    Module            *entry_module;
    Closure           *entry_closure;

    assert(PZT_LAST_TOKEN < 256);

    context.return_stack = new uint8_t*[RETURN_STACK_SIZE];
    context.expr_stack = new StackValue[EXPR_STACK_SIZE];
#if defined(PZ_DEV) || defined(PZ_DEBUG)
    memset(context.expr_stack, 0, sizeof(StackValue) * EXPR_STACK_SIZE);
#endif

    /*
     * Assemble a special procedure that exits the interpreter and put its
     * address on the call stack.
     */
    memset(&imv_none, 0, sizeof(imv_none));
    wrapper_proc_size = write_instr(nullptr, 0, PZI_END);
    wrapper_proc = static_cast<uint8_t*>(malloc(wrapper_proc_size));
    write_instr(wrapper_proc, 0, PZI_END);
    context.return_stack[0] = nullptr;
    context.return_stack[1] = wrapper_proc;
    context.rsp = 1;

    // Determine the entry procedure.
    entry_module = pz.entry_module();
    entry_closure = entry_module ? entry_module->entry_closure() : nullptr;
    if (!entry_closure) {
        fprintf(stderr, "No entry closure\n");
        abort();
    }

#ifdef PZ_DEV
    trace_enabled = options.interp_trace();
#endif
    retcode = generic_main_loop(context, pz.heap(), entry_closure, pz);

    // TODO: We can skip this if not debugging.
    if (nullptr != wrapper_proc) {
        free(wrapper_proc);
    }
    if (nullptr != context.return_stack) {
        delete[] context.return_stack;
    }
    if (nullptr != context.expr_stack) {
        delete[] context.expr_stack;
    }

    return retcode;
}

}
