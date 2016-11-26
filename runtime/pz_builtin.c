/*
 * Plasma builtins
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2015 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#include <stdlib.h>

#include "pz_common.h"
#include "pz_builtin.h"
#include "pz_radix_tree.h"
#include "pz_run.h"

PZ_RadixTree *
pz_setup_builtins(void)
{
    PZ_RadixTree *tree;

    tree = pz_radix_init();

    pz_radix_insert(tree, "print",          &builtin_print);
    pz_radix_insert(tree, "int_to_string",  &builtin_int_to_string);
    pz_radix_insert(tree, "free",           &builtin_free);
    pz_radix_insert(tree, "concat_string",  &builtin_concat_string);
    pz_radix_insert(tree, "die",            &builtin_die);

    return tree;
}

void
pz_builtins_free(PZ_RadixTree * builtins)
{
    pz_radix_free(builtins, NULL);
}

