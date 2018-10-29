/*
 * Plasma bytecode reader
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2015-2916 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#ifndef PZ_READ_H
#define PZ_READ_H

pz::Module *
pz_read(pz::PZ &pz, const char *filename, bool verbose);

#endif /* ! PZ_READ_H */
