/*
 * Plasma execution tracing.
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#include <stdio.h>

#include "pz_common.h"

#include "pz_code.h"
#include "pz_gc.h"
#include "pz_trace.h"
#include "pz_string.h"
#include "pz_util.h"

namespace pz {

bool trace_enabled = false;

/*
 * THese are used to cache some lookup information to find line numbers
 * within procs.
 */
Proc *   last_proc   = nullptr;
unsigned last_lookup = 0;

void trace_instr_(unsigned rsp, const char * instr_name)
{
    fprintf(stderr, "%4u: %s\n", rsp, instr_name);
}

void trace_instr2_(unsigned rsp, const char * instr_name, int num)
{
    fprintf(stderr, "%4u: %s %d\n", rsp, instr_name, num);
}

void trace_state_(const Heap * heap, void * ip, void * env,
                  unsigned rsp, unsigned esp, uint64_t * stack)
{
    void * code = heap_interior_ptr_to_ptr(heap, ip);
    assert(ip >= code);
    std::ptrdiff_t offset =
        reinterpret_cast<uint8_t *>(ip) - reinterpret_cast<uint8_t *>(code);

    // XXX These should be GC roots.
    Proc * proc = reinterpret_cast<Proc *>(heap_meta_info(heap, code));

    if (proc) {
        fprintf(stderr, "      IP  %p: %s+%ld%s", ip,
                proc->name().c_str(),
                (long)offset,
                proc->is_builtin() ? " (builtin)" : "");
    } else {
        fprintf(stderr, "      IP  %p: +%ld (builtin)", ip,
                (long)offset);
    }

    unsigned line = 0;
    if (proc && proc->filename().hasValue()) {
        if (proc != last_proc) {
            last_lookup = 0;
            last_proc   = proc;
        }
        line = proc->line(offset, &last_lookup);
        if (line) {
            fprintf(stderr, " from %s:%d", proc->filename().value().c_str(),
                    line);
        }
    }

    fprintf(stderr, "\n");

    fprintf(stderr, "      ENV %p\n", env);
    fprintf(stderr, "      RSP %4u ESP %4u\n", rsp, esp);
    fprintf(stderr, "      stack: ");

    int start = esp - 4;
    start     = start >= 1 ? start : 1;

    for (unsigned i = start; i <= esp; i++) {
        fprintf(stderr, "0x%." WORDSIZE_HEX_CHARS_STR PRIx64 " ", stack[i]);
    }
    fprintf(stderr, "\n\n");
}

}  // namespace pz
