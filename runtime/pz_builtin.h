/*
 * Plasma builtins
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2016 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#ifndef PZ_BUILTIN_H
#define PZ_BUILTIN_H

#include "pz_radix_tree.h"

PZ_RadixTree *
pz_setup_builtins(void);

void
pz_builtins_free(PZ_RadixTree * builtins);

#endif /* ! PZ_BUILTIN_H */
