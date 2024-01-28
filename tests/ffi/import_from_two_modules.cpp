/*
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

#include <stdio.h>
#include <sys/types.h>
#include <unistd.h>

#include "../../runtime/pz_common.h"
#include "../../runtime/pz_foreign.h"
#include "../../runtime/pz_generic_run.h"

#include "import_function.h"

using namespace pz;

unsigned bar(void * stack_, unsigned sp)
{
    printf("Hi mum\n");
    return sp;
}

unsigned my_getpid(void * stack_, unsigned sp)
{
    StackValue * stack = reinterpret_cast<StackValue *>(stack_);
    stack[++sp].u32 = getpid();
    return sp;
}

