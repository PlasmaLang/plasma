/*
 * Plasma builtins
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2015-16 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#include "pz_common.h"
#include "pz_code.h"
#include "pz_builtin.h"
#include "pz_radix_tree.h"
#include "pz_run.h"

static PZ_Proc_Symbol builtin_print = {
    PZ_BUILTIN_C_FUNC,
    { .c_func = builtin_print_func }
};

static PZ_Proc_Symbol builtin_int_to_string = {
    PZ_BUILTIN_C_FUNC,
    { .c_func = builtin_int_to_string_func }
};

static PZ_Proc_Symbol builtin_free = {
    PZ_BUILTIN_C_FUNC,
    { .c_func = builtin_free_func }
};

static PZ_Proc_Symbol builtin_concat_string = {
    PZ_BUILTIN_C_FUNC,
    { .c_func = builtin_concat_string_func }
};

static PZ_Proc_Symbol builtin_die = {
    PZ_BUILTIN_C_FUNC,
    { .c_func = builtin_die_func }
};


PZ_Module *
pz_setup_builtins(void)
{
    PZ_Module       *module;

    module = pz_module_init_empty();

    pz_module_add_proc_symbol(module, "print",
            &builtin_print);
    pz_module_add_proc_symbol(module, "int_to_string",
            &builtin_int_to_string);
    pz_module_add_proc_symbol(module, "free",
            &builtin_free);
    pz_module_add_proc_symbol(module, "concat_string",
            &builtin_concat_string);
    pz_module_add_proc_symbol(module, "die",
            &builtin_die);

    /*
     * TODO: Add the new builtins that are built from PZ instructions rather
     * than foreign code.
     *
     * TODO: they need to be loaded in the usual way and converted
     * to the runtime version of the bytecode.
     */

    return module;
}

