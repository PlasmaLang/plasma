/*
 * Plasma bytecode reader
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2015-2916 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#ifndef PZ_READ_H
#define PZ_READ_H

#include "pz_radix_tree.h"

PZ *
pz_read(const char *filename, bool verbose,
    PZ_RadixTree *builtin_symbols);

#endif /* ! PZ_READ_H */
