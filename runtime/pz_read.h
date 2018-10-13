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

#ifdef __cplusplus
extern "C" {
#endif

PZ_Module *
pz_read(PZ *pz, const char *filename, bool verbose);

#ifdef __cplusplus
}
#endif

#endif /* ! PZ_READ_H */
