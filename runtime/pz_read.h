/*
 * Plasma bytecode reader
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2015 Paul Bone
 * Distributed under the terms of the MIT license, see ../LICENSE.runtime
 */

#ifndef PZ_READ_H
#define PZ_READ_H

#include "pz_common.h"

pz* read_pz(const char *filename, bool verbose);

#endif /* ! PZ_READ_H */
