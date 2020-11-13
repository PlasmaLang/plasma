/*
 * Plasma bytecode exection (generic portable version)
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2015-2020 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#include "pz_common.h"

#include <stdio.h>
#include <string.h>

#include "pz.h"
#include "pz_code.h"
#include "pz_cxx_future.h"
#include "pz_interp.h"
#include "pz_trace.h"
#include "pz_util.h"

#include "pz_generic_closure.h"
#include "pz_generic_run.h"

namespace pz {

/* Must match or exceed ptag_bits from src/core.types.m */
const unsigned num_tag_bits = 2;
const uintptr_t tag_bits    = 0x3;

/*
 * Run the program
 *
 ******************/

int run(PZ & pz, const Options & options)
{
    uint8_t * wrapper_proc = nullptr;
    unsigned wrapper_proc_size;
    int retcode;
    ImmediateValue imv_none;

    assert(PZT_LAST_TOKEN < 256);

    Context context(pz.heap());

    /*
     * Assemble a special procedure that exits the interpreter and put its
     * address on the call stack.
     */
    memset(&imv_none, 0, sizeof(imv_none));
    wrapper_proc_size = write_instr(nullptr, 0, PZI_END);
    wrapper_proc =
        static_cast<uint8_t *>(context.alloc_bytes(wrapper_proc_size, META));
    heap_set_meta_info(context.heap(), wrapper_proc, nullptr);
    write_instr(wrapper_proc, 0, PZI_END);
    context.return_stack[0] = nullptr;
    // Wrapper proc is tracablo here.
    context.return_stack[1] = wrapper_proc;
    context.rsp             = 1;

    // Determine the entry procedure.
    Library * program       = pz.program_lib();
    Closure * entry_closure = program ? program->entry_closure() : nullptr;
    if (!entry_closure) {
        fprintf(stderr, "No entry closure\n");
        abort();
    }
    PZOptEntrySignature entry_signature = program->entry_signature();
    switch (entry_signature) {
        case PZ_OPT_ENTRY_SIG_PLAIN:
            break;
        case PZ_OPT_ENTRY_SIG_ARGS:
            fprintf(stderr,
                    "Unsupported, cannot execute programs that "
                    "accept command line arguments. (Bug #283)\n");
            abort();
    }

#ifdef PZ_DEV
    trace_enabled = options.interp_trace();
#endif
    retcode = generic_main_loop(context, pz.heap(), entry_closure, pz);

    return retcode;
}

#define RETURN_STACK_SIZE 2048
#define EXPR_STACK_SIZE   4096

Context::Context(Heap * heap)
    : AbstractGCTracer(heap)
    , ip(nullptr)
    , env(nullptr)
    , rsp(0)
    , esp(0)
{
    return_stack = new uint8_t *[RETURN_STACK_SIZE];
    expr_stack   = new StackValue[EXPR_STACK_SIZE];
#if defined(PZ_DEV) || defined(PZ_DEBUG)
    memset(expr_stack, 0, sizeof(StackValue) * EXPR_STACK_SIZE);
#endif
}

Context::~Context()
{
    delete[] return_stack;
    delete[] expr_stack;
}

void Context::do_trace(HeapMarkState * state) const
{
    /*
     * The +1 is required here because the callee will only mark the first N
     * bytes in these memory areas, and esp and rsp are zero-based indexes,
     * So if esp is 2, which means the 3rd (0-based) index is the
     * top-of-stack.  Then we need (2+1)*sizeof(...) to ensure we mark all
     * three items.
     */
    state->mark_root_conservative(expr_stack, (esp + 1) * sizeof(StackValue));
    state->mark_root_conservative_interior(return_stack,
                                           (rsp + 1) * WORDSIZE_BYTES);
    state->mark_root_interior(ip);
    state->mark_root(env);
}

}  // namespace pz
