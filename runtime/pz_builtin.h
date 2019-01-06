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
#include "pz_gc.h"

#ifdef __cplusplus
extern "C" {
#endif

namespace pz {
Module *
setup_builtins(Heap *heap);
}

#ifdef __cplusplus
}
#endif

#endif /* ! PZ_BUILTIN_H */
