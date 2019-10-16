/*
 * Plasma execution tracing.
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2016, 2018-2019 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#include <stdio.h>

#include "pz_common.h"

#include "pz_code.h"
#include "pz_gc.h"
#include "pz_trace.h"
#include "pz_util.h"

namespace pz {

bool trace_enabled = false;

void trace_instr_(unsigned rsp, const char *instr_name)
{
    fprintf(stderr, "%4u: %s\n", rsp, instr_name);
}

void trace_instr2_(unsigned rsp, const char *instr_name, int num)
{
    fprintf(stderr, "%4u: %s %d\n", rsp, instr_name, num);
}

void trace_state_(const Heap *heap, void *ip, unsigned rsp, unsigned esp,
    uint64_t *stack)
{
    int start;
    
    void *code = heap_interior_ptr_to_ptr(heap, ip);
    assert(ip >= code);
    ptrdiff_t offset = reinterpret_cast<uint8_t*>(ip) -
        reinterpret_cast<uint8_t*>(code);

    Proc *proc = reinterpret_cast<Proc*>(heap_meta_info(heap, code));

    if (proc && proc->filename()) {
        fprintf(stderr, "      IP %p, from %s:%d\n", ip, proc->filename(),
                proc->line(offset));
    } else {
        fprintf(stderr, "      IP %p\n", ip);
    }
    fprintf(stderr, "      RSP %4u ESP %4u\n", rsp, esp);
    fprintf(stderr, "      stack: ");

    start = esp - 4;
    start = start >= 1 ? start : 1;

    for (unsigned i = start; i <= esp; i++) {
        fprintf(stderr, "0x%." WORDSIZE_HEX_CHARS_STR PRIx64 " ", stack[i]);
    }
    fprintf(stderr, "\n\n");
}

} // namespace pz

