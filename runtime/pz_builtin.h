/*
 * Plasma builtins
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2016 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#ifndef PZ_BUILTIN_H
#define PZ_BUILTIN_H

#include "pz.h"

#ifdef __cplusplus
extern "C" {
#endif

pz::Module *
pz_setup_builtins(void);

#ifdef __cplusplus
}
#endif

#endif /* ! PZ_BUILTIN_H */
