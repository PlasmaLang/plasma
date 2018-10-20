/*
 * Radix tree
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2016-2018 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#include <stdio.h>
#include <string.h>

#include "pz_common.h"

#include "pz_radix_tree.h"

/*
 * This is probably not the most efficient radix tree.  There will be a
 * number of easy to improve things, but before we can do that we must
 * benchmark the tree as part the Plasma runtime and also determine whether
 * a radix tree is indeed the best data structure.
 */

namespace pz {

bool
RadixTreeHelpers::streq(const char *s1,
                         const char *s2,
                         unsigned len,
                         unsigned *ret_pos)
{
    unsigned pos;

    for (pos = 0; pos < len; pos++) {
        if (s1[pos] != s2[pos]) {
            *ret_pos = pos;
            return false;
        } else if (s1[pos] == 0) {
            /* Both strings ended together */
            *ret_pos = pos;
            return true;
        }
    }

    /*
     * Stop comparing as this is the end of the prefix in the tree.
     */
    *ret_pos = pos;
    return true;
}

} // namespace pz

