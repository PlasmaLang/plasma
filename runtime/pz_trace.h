/*
 * Plasma execution tracing.
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2016 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#ifndef PZ_TRACE_H
#define PZ_TRACE_H

#ifdef __cplusplus
extern "C" {
#endif

#ifdef PZ_DEV

extern bool pz_trace_enabled;

void
pz_trace_instr_(unsigned rsp, const char *instr_name);

void
pz_trace_instr2_(unsigned rsp, const char *instr_name, int num);

void
pz_trace_state_(void *ip, unsigned rsp, unsigned esp, uint64_t *stack);

#define pz_trace_instr(rip, name) \
    if (pz_trace_enabled) { \
        pz_trace_instr_(rip, name); \
    }
#define pz_trace_instr2(rip, name, num) \
    if (pz_trace_enabled) { \
        pz_trace_instr2_(rip, name, num); \
    }
#define pz_trace_state(rip, rsp, esp, stack) \
    if (pz_trace_enabled) { \
        pz_trace_state_(rip, rsp, esp, stack); \
    }

#else /* ! PZ_DEV */

#define pz_trace_instr(rip, name)
#define pz_trace_instr2(rip, name, num)
#define pz_trace_state(rip, rsp, esp, stack)

#endif /* ! PZ_DEV */

#ifdef __cplusplus
} // extern "C"
#endif

#endif /* ! PZ_TRACE_H */

