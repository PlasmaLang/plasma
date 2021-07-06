/*
 * Plasma bytecode exection (generic portable version)
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2015-2019, 2021 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#include "pz_common.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>

#include "pz_interp.h"

#include "pz_gc.h"
#include "pz_generic_run.h"
#include "pz_string.h"

namespace pz {

/*
 * Imported procedures
 *
 **********************/

unsigned pz_builtin_print_func(void * void_stack, unsigned sp)
{
    StackValue * stack = static_cast<StackValue *>(void_stack);

    String::from_ptr(stack[sp--].ptr).print();

    return sp;
}

/*
 * Long enough for a 32 bit value, plus a sign, plus a null termination
 * byte.
 */
#define INT_TO_STRING_BUFFER_SIZE 11

unsigned pz_builtin_int_to_string_func(void * void_stack, unsigned sp,
                                       AbstractGCTracer & gc_trace)
{
    StackValue * stack = static_cast<StackValue *>(void_stack);
    int32_t num = stack[sp].s32;

    FlatString * string = FlatString::New(gc_trace, INT_TO_STRING_BUFFER_SIZE);
    int result = snprintf(string->buffer(),
            INT_TO_STRING_BUFFER_SIZE, "%d", (int)num);
    if ((result < 0) || (result > (INT_TO_STRING_BUFFER_SIZE - 1))) {
        stack[sp].ptr = NULL;
    } else {
        string->fixSize(result);
        stack[sp].ptr = string;
    }
    return sp;
}

unsigned pz_builtin_setenv_func(void * void_stack, unsigned sp)
{
    StackValue * stack = static_cast<StackValue *>(void_stack);
    const String value = String::from_ptr(stack[sp--].ptr);
    const String name  = String::from_ptr(stack[sp--].ptr);

    int result = setenv(name.c_str(), value.c_str(), 1);

    stack[++sp].u32 = !result;

    return sp;
}

unsigned pz_builtin_gettimeofday_func(void * void_stack, unsigned sp)
{
    StackValue * stack = static_cast<StackValue *>(void_stack);

    struct timeval tv;
    int res = gettimeofday(&tv, NULL);

    stack[++sp].u32 = res == 0 ? 1 : 0;
    // This is aweful, but Plasma itself doesn't handle other inttypes yet.
    stack[++sp].u32 = (uint32_t)tv.tv_sec;
    stack[++sp].u32 = (uint32_t)tv.tv_usec;

    return sp;
}

unsigned pz_builtin_concat_string_func(void * void_stack, unsigned sp,
                                       AbstractGCTracer & gc_trace)
{
    StackValue * stack = static_cast<StackValue *>(void_stack);

    const String s2 = String::from_ptr(stack[sp--].ptr);
    const String s1 = String::from_ptr(stack[sp].ptr);
    String s = String::append(gc_trace, s1, s2);

    stack[sp].ptr = s.ptr();
    return sp;
}

unsigned pz_builtin_die_func(void * void_stack, unsigned sp)
{
    StackValue * stack = static_cast<StackValue *>(void_stack);
    const String s = String::from_ptr(stack[sp].ptr);
    fprintf(stderr, "Die: %s\n", s.c_str());
    exit(1);
}

unsigned pz_builtin_set_parameter_func(void * void_stack, unsigned sp, PZ & pz)
{
    StackValue * stack = static_cast<StackValue *>(void_stack);

    // int32_t value = stack[sp].s32;
    const String name = String::from_ptr(stack[sp - 1].ptr);

    /*
     * There are no parameters defined.
     */
    fprintf(stderr, "No such parameter '%s'\n", name.c_str());
    int32_t result = 0;

    sp--;
    stack[sp].sptr = result;

    return sp;
}

unsigned pz_builtin_get_parameter_func(void * void_stack, unsigned sp, PZ & pz)
{
    StackValue * stack = static_cast<StackValue *>(void_stack);

    const String name = String::from_ptr(stack[sp].ptr);

    int32_t result;
    int32_t value;
    if (name.equals(String("heap_usage"))) {
        value  = heap_get_usage(pz.heap());
        result = 1;
    } else if (name.equals(String("heap_collections"))) {
        value  = heap_get_collections(pz.heap());
        result = 1;
    } else {
        fprintf(stderr, "No such parameter '%s'.\n", name.c_str());
        result = 0;
        value  = 0;
    }

    stack[sp].sptr     = result;
    stack[sp + 1].sptr = value;
    sp++;

    return sp;
}

}  // namespace pz
