/*
 * Plasma bytecode exection (generic portable version)
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2015-2018 Plasma Team
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

/*
 * Imported procedures
 *
 **********************/

unsigned
pz_builtin_print_func(void *void_stack, unsigned sp)
{
    PZ_Stack_Value *stack = void_stack;

    char *string = (char *)(stack[sp--].uptr);
    printf("%s", string);
    return sp;
}

/*
 * Long enough for a 32 bit value, plus a sign, plus a null termination
 * byte.
 */
#define INT_TO_STRING_BUFFER_SIZE 11

unsigned
pz_builtin_int_to_string_func(void *void_stack, unsigned sp, PZ_Heap *heap,
        trace_fn trace, void *stacks)
{
    char           *string;
    int32_t         num;
    int             result;
    PZ_Stack_Value *stack = void_stack;

    num = stack[sp].s32;
    string = pz_gc_alloc_bytes(heap, INT_TO_STRING_BUFFER_SIZE,
                               trace, stacks);
    result = snprintf(string, INT_TO_STRING_BUFFER_SIZE, "%d", (int)num);
    if ((result < 0) || (result > (INT_TO_STRING_BUFFER_SIZE - 1))) {
        stack[sp].ptr = NULL;
    } else {
        stack[sp].ptr = string;
    }
    return sp;
}

unsigned
pz_builtin_setenv_func(void *void_stack, unsigned sp)
{
    PZ_Stack_Value *stack = void_stack;
    int            result;
    const char    *value = stack[sp--].ptr;
    const char    *name = stack[sp--].ptr;

    result = setenv(name, value, 1);

    stack[++sp].u32 = !result;

    return sp;
}

unsigned
pz_builtin_gettimeofday_func(void *void_stack, unsigned sp)
{
    PZ_Stack_Value *stack = void_stack;
    struct timeval  tv;
    int             res;

    res = gettimeofday(&tv, NULL);

    stack[++sp].u32 = res == 0 ? 1 : 0;
    // This is aweful, but Plasma itself doesn't handle other inttypes yet.
    stack[++sp].u32 = (uint32_t)tv.tv_sec;
    stack[++sp].u32 = (uint32_t)tv.tv_usec;

    return sp;
}

unsigned
pz_builtin_concat_string_func(void *void_stack, unsigned sp, PZ_Heap *heap,
        trace_fn trace_thread, void *trace_data)
{
    const char     *s1, *s2;
    char           *s;
    size_t          len;
    PZ_Stack_Value *stack = void_stack;

    s2 = stack[sp--].ptr;
    s1 = stack[sp].ptr;

    len = strlen(s1) + strlen(s2) + 1;
    s = pz_gc_alloc_bytes(heap, sizeof(char) * len, trace_thread, trace_data);
    strcpy(s, s1);
    strcat(s, s2);

    stack[sp].ptr = s;
    return sp;
}

unsigned
pz_builtin_die_func(void *void_stack, unsigned sp)
{
    const char     *s;
    PZ_Stack_Value *stack = void_stack;

    s = stack[sp].ptr;
    fprintf(stderr, "Die: %s\n", s);
    exit(1);
}

unsigned
pz_builtin_set_parameter_func(void *void_stack, unsigned sp, PZ_Heap *heap,
        trace_fn trace_thread, void *trace_data)
{
    PZ_Stack_Value *stack = void_stack;

    int32_t value = stack[sp].s32;
    const char *name = stack[sp-1].ptr;
    int32_t result;

    if (0 == strcmp(name, "heap_size")) {
        result = pz_gc_set_heap_size(heap, value);
    } else {
        fprintf(stderr, "No such parameter '%s'\n", name);
        result = 0;
    }

    sp--;
    stack[sp].sptr = result;

    return sp;
}

