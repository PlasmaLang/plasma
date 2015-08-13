/*
 * PZ Utils.
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2015 Paul Bone
 * Distributed under the terms of the MIT license, see ../LICENSE.runtime
 */

#ifndef PZ_UTILS_H
#define PZ_UTILS_H

/*
 * The machine word size.
 */
#define MACHINE_WORD_SIZE   sizeof(uintptr_t)
#define ROUND_UP(X, Y) (((X) + ((Y)-1)) & ~((Y) - 1))

#endif /* ! PZ_UTILS_H */
