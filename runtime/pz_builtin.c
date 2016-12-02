/*
 * Plasma builtins
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2015 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#include "pz_common.h"
#include "pz_code.h"
#include "pz_builtin.h"
#include "pz_radix_tree.h"
#include "pz_run.h"

Imported_Proc builtin_print = {
    BUILTIN_FOREIGN,
    builtin_print_func
};

Imported_Proc builtin_int_to_string = {
    BUILTIN_FOREIGN,
    builtin_int_to_string_func
};

Imported_Proc builtin_free = {
    BUILTIN_FOREIGN,
    builtin_free_func
};

Imported_Proc builtin_concat_string = {
    BUILTIN_FOREIGN,
    builtin_concat_string_func
};

Imported_Proc builtin_die = {
    BUILTIN_FOREIGN,
    builtin_die_func
};

PZ_Module *
pz_setup_builtins(void)
{
    PZ_Module       *module;
    PZ_RadixTree    *tree;

    module = pz_module_init();
    tree = pz_radix_init();

    pz_radix_insert(tree, "print",          &builtin_print);
    pz_radix_insert(tree, "int_to_string",  &builtin_int_to_string);
    pz_radix_insert(tree, "free",           &builtin_free);
    pz_radix_insert(tree, "concat_string",  &builtin_concat_string);
    pz_radix_insert(tree, "die",            &builtin_die);

    /*
     * TODO: Add the new builtins that are built from PZ instructions rather
     * than foreign code.
     *
     * TODO: they need to be loaded in the usual way and converted
     * to the runtime version of the bytecode.
     */
    module->symbols = tree;

    return module;
}

