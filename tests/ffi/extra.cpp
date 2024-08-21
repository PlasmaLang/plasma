/*
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

#include <stdio.h>

#include "../../runtime/pz_common.h"
#include "../../runtime/pz_foreign.h"
#include "../../runtime/pz_generic_run.h"

#include "extra.h"

using namespace pz;

unsigned test_extra(void * stack_, unsigned sp)
{
    printf("Test Extra\n");
    return sp;
}

