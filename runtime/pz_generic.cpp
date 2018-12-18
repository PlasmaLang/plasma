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
run(const PZ &pz)
{
    PZ_Stacks          stacks;
    uint8_t           *wrapper_proc = nullptr;
    unsigned           wrapper_proc_size;
    int                retcode;
    PZ_Immediate_Value imv_none;
    Module            *entry_module;
    Optional<unsigned> entry_closure_id;

    assert(PZT_LAST_TOKEN < 256);

    stacks.return_stack = new uint8_t*[RETURN_STACK_SIZE];
    stacks.expr_stack = new PZ_Stack_Value[EXPR_STACK_SIZE];
#if defined(PZ_DEV) || defined(PZ_DEBUG)
    memset(stacks.expr_stack, 0, sizeof(PZ_Stack_Value) * EXPR_STACK_SIZE);
#endif

    /*
     * Assemble a special procedure that exits the interpreter and put its
     * address on the call stack.
     */
    memset(&imv_none, 0, sizeof(imv_none));
    wrapper_proc_size = write_instr(nullptr, 0, PZI_END);
    wrapper_proc = static_cast<uint8_t*>(malloc(wrapper_proc_size));
    write_instr(wrapper_proc, 0, PZI_END);
    stacks.return_stack[0] = nullptr;
    stacks.return_stack[1] = wrapper_proc;
    stacks.rsp = 1;

    // Determine the entry procedure.
    entry_module = pz.entry_module();
    if (nullptr != entry_module) {
        entry_closure_id = entry_module->entry_closure();
    }
    if (!entry_closure_id.hasValue()) {
        fprintf(stderr, "No entry closure\n");
        abort();
    }

    retcode = pz_generic_main_loop(&stacks, pz.heap(),
            entry_module->closure(entry_closure_id.value()));

    // TODO: We can skip this if not debugging.
    if (nullptr != wrapper_proc) {
        free(wrapper_proc);
    }
    if (nullptr != stacks.return_stack) {
        delete[] stacks.return_stack;
    }
    if (nullptr != stacks.expr_stack) {
        delete[] stacks.expr_stack;
    }

    return retcode;
}

}
