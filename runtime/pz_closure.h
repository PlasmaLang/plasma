/*
 * Plasma bytecode code structures and functions
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2018 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#ifndef PZ_CLOSURE_H
#define PZ_CLOSURE_H

#ifdef __cplusplus
extern "C" {
#endif

typedef struct PZ_Closure_S PZ_Closure;

PZ_Closure *
pz_init_closure(uint8_t *code, void *data);

void
pz_closure_free(PZ_Closure *closure);

#ifdef __cplusplus
} // extern "C"
#endif

#endif // ! PZ_CLOSURE_H
