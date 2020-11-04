/*
 * Plasma builtins
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2016, 2020 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#ifndef PZ_BUILTIN_H
#define PZ_BUILTIN_H

#include "pz.h"
#include "pz_gc.h"

namespace pz {

void
setup_builtins(Module *module, GCCapability &gccap);

}

#endif /* ! PZ_BUILTIN_H */
