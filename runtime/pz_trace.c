/*
 * Plasma execution tracing.
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2016, 2018 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#include <stdio.h>

#include "pz_common.h"
#include "pz_trace.h"
#include "pz_util.h"

void pz_trace_instr(unsigned rsp, const char *instr_name)
{
    fprintf(stderr, "%4u: %s\n", rsp, instr_name);
}

void pz_trace_instr2(unsigned rsp, const char *instr_name, int num)
{
    fprintf(stderr, "%4u: %s %d\n", rsp, instr_name, num);
}

void pz_trace_state(void *ip, unsigned rsp, unsigned esp, uint64_t *stack)
{
    int start;

    fprintf(stderr, "      IP %p RSP %4u ESP %4u\n", ip, rsp, esp);
    fprintf(stderr, "      stack: ");
    
    start = esp - 4;
    start = start >= 1 ? start : 1;

    for (unsigned i = start; i <= esp; i++) {
        fprintf(stderr, "0x%." WORDSIZE_HEX_CHARS_STR PRIx64 " ", stack[i]);
    }
    fprintf(stderr, "\n\n");
}

