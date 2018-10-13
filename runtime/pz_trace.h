/*
 * Plasma execution tracing.
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2016 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#ifndef PZ_TRACE_H
#define PZ_TRACE_H

#ifdef PZ_INSTR_TRACE

#ifdef __cplusplus
extern "C" {
#endif

void
pz_trace_instr(unsigned rsp, const char *instr_name);

void
pz_trace_instr2(unsigned rsp, const char *instr_name, int num);

void
pz_trace_state(void *ip, unsigned rsp, unsigned esp, uint64_t *stack);

#else /* ! PZ_INSTR_TRACE */

#define pz_trace_instr(rip, name)
#define pz_trace_instr2(rip, name, num)
#define pz_trace_state(rip, rsp, esp, stack)

#ifdef __cplusplus
} // extern "C"
#endif

#endif /* ! PZ_INSTR_TRACE */

#endif /* ! PZ_TRACE_H */
