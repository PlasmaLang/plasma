/*
 * Plasma bytecode exection (generic portable version)
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) Plasma Team
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
const unsigned  num_tag_bits = 2;
const uintptr_t tag_bits     = 0x3;

/*
 * Run the program
 *
 ******************/

int run(PZ & pz, const Options & options, GCCapability &gc)
{
    uint8_t *      wrapper_proc = nullptr;
    unsigned       wrapper_proc_size;
    int            retcode = 0;
    ImmediateValue imv_none;

    assert(PZT_LAST_TOKEN < 256);

    Context context(gc);
    if (!context.allocate()) {
        fprintf(stderr, "Could not allocate context\n");
        return PZ_EXIT_RUNTIME_ERROR; 
    }

    ScopeExit finalise([&context, &retcode, &options]{
        if (!context.release(options.fast_exit())) {
            fprintf(stderr, "Error releasing memory\n");
            if (retcode == 0) {
                retcode = PZ_EXIT_RUNTIME_NONFATAL;
            }
        }
    });

    /*
     * Assemble a special procedure that exits the interpreter and put its
     * address on the call stack.
     */
    memset(&imv_none, 0, sizeof(imv_none));
    wrapper_proc_size = write_instr(nullptr, 0, PZI_END);
    wrapper_proc =
        static_cast<uint8_t *>(context.alloc_bytes(wrapper_proc_size, META));
    heap_set_meta_info(&context.heap(), wrapper_proc, nullptr);
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
        return PZ_EXIT_RUNTIME_ERROR; 
    }
    PZOptEntrySignature entry_signature = program->entry_signature();
    switch (entry_signature) {
        case PZ_OPT_ENTRY_SIG_PLAIN:
            break;
        case PZ_OPT_ENTRY_SIG_ARGS:
            fprintf(stderr,
                    "Unsupported, cannot execute programs that "
                    "accept command line arguments. (Bug #283)\n");
            return PZ_EXIT_RUNTIME_ERROR;
    }

#ifdef PZ_DEV
    trace_enabled = options.interp_trace();
#endif
    int program_retcode =
        generic_main_loop(context, &pz.heap(), entry_closure, pz);
    retcode = program_retcode ? program_retcode : retcode;

    return retcode;
}

Context::Context(GCCapability & gc)
    : AbstractGCTracer(gc)
    , ip(nullptr)
    , env(nullptr)
    , return_stack("return stack")
    , rsp(0)
    , expr_stack("expression stack")
    , esp(0)
{}

Context::~Context()
{
    assert(!return_stack.is_mapped());
    assert(!expr_stack.is_mapped());
}

bool Context::allocate() {
    if (!return_stack.allocate_guarded(RETURN_STACK_SIZE * sizeof(uint8_t*))) {
        return false;
    }

    if (!expr_stack.allocate_guarded(EXPR_STACK_SIZE * sizeof(StackValue))) {
        return false;
    }

    return true;
}

bool Context::release(bool fast) {
    if (fast) {
        return_stack.forget();
        expr_stack.forget();
        return true;
    }

    bool result = true;
    if (!return_stack.release()) {
        result = false;
    }
    if (!expr_stack.release()) {
        result = false;
    }
    return result;
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
    state->mark_root_conservative((void*)expr_stack.ptr(),
                                  (esp + 1) * sizeof(StackValue));
    state->mark_root_conservative_interior((void*)return_stack.ptr(),
                                           (rsp + 1) * WORDSIZE_BYTES);
    state->mark_root_interior(ip);
    state->mark_root(env);
}

}  // namespace pz
