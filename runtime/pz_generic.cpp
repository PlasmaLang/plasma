/*
 * Plasma bytecode exection (generic portable version)
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2015-2018 Plasma Team
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
run(const PZ &pz, const Options &options)
{
    uint8_t          **return_stack;
    unsigned           rsp;
    Stack_Value       *expr_stack;
    uint8_t           *wrapper_proc = NULL;
    unsigned           wrapper_proc_size;
    int                retcode;
    PZ_Immediate_Value imv_none;
    Module            *entry_module;
    Optional<unsigned> entry_closure_id;

    assert(PZT_LAST_TOKEN < 256);

    return_stack = new uint8_t*[RETURN_STACK_SIZE];
    expr_stack = new Stack_Value[EXPR_STACK_SIZE];
#if defined(PZ_DEV) || defined(PZ_DEBUG)
    memset(expr_stack, 0, sizeof(Stack_Value) * EXPR_STACK_SIZE);
#endif

    Heap heap(options, expr_stack);
    if (!heap.init()) {
        fprintf(stderr, "Couldn't initialise heap.");
        retcode = 127;
        goto finish;
    }

    /*
     * Assemble a special procedure that exits the interpreter and put its
     * address on the call stack.
     */
    memset(&imv_none, 0, sizeof(imv_none));
    wrapper_proc_size = write_instr(NULL, 0, PZI_END);
    wrapper_proc = static_cast<uint8_t*>(malloc(wrapper_proc_size));
    write_instr(wrapper_proc, 0, PZI_END);
    return_stack[0] = NULL;
    return_stack[1] = wrapper_proc;
    rsp = 1;

    // Determine the entry procedure.
    entry_module = pz.entry_module();
    if (NULL != entry_module) {
        entry_closure_id = entry_module->entry_closure();
    }
    if (!entry_closure_id.hasValue()) {
        fprintf(stderr, "No entry closure\n");
        abort();
    }

#ifdef PZ_DEV
    pz_trace_enabled = options.interp_trace();
#endif
    retcode = pz_generic_main_loop(return_stack, rsp, expr_stack, &heap,
            entry_module->closure(entry_closure_id.value()));

finish:
    // TODO: We can skip this if not debugging.
    if (NULL != wrapper_proc) {
        free(wrapper_proc);
    }
    if (NULL != return_stack) {
        delete[] return_stack;
    }
    if (NULL != expr_stack) {
        delete[] expr_stack;
    }
    heap.finalise();

    return retcode;
}

}
