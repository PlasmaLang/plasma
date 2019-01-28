/*
 * Plasma execution tracing.
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2016 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#ifndef PZ_TRACE_H
#define PZ_TRACE_H

#ifdef PZ_DEV

namespace pz {

extern bool trace_enabled;

void
trace_instr_(unsigned rsp, const char *instr_name);

void
trace_instr2_(unsigned rsp, const char *instr_name, int num);

void
trace_state_(void *ip, unsigned rsp, unsigned esp, uint64_t *stack);

} // namespace pz

#define pz_trace_instr(rip, name) \
    if (pz::trace_enabled) { \
        pz::trace_instr_(rip, name); \
    }
#define pz_trace_instr2(rip, name, num) \
    if (pz::trace_enabled) { \
        pz::trace_instr2_(rip, name, num); \
    }
#define pz_trace_state(rip, rsp, esp, stack) \
    if (pz::trace_enabled) { \
        pz::trace_state_(rip, rsp, esp, stack); \
    }

#else /* ! PZ_DEV */

#define pz_trace_instr(rip, name)
#define pz_trace_instr2(rip, name, num)
#define pz_trace_state(rip, rsp, esp, stack)

#endif /* ! PZ_DEV */

#endif /* ! PZ_TRACE_H */

