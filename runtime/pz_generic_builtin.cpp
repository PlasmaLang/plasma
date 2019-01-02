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
builtin_print_func(void *void_stack, unsigned sp, PZ_Heap *heap)
{
    Stack_Value *stack = static_cast<Stack_Value*>(void_stack);

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
builtin_int_to_string_func(void *void_stack, unsigned sp, PZ_Heap *heap)
{
    char        *string;
    int32_t      num;
    int          result;
    Stack_Value *stack = static_cast<Stack_Value*>(void_stack);

    num = stack[sp].s32;
    string = static_cast<char*>(
        pz_gc_alloc_bytes(heap, INT_TO_STRING_BUFFER_SIZE, &stack[sp]));
    result = snprintf(string, INT_TO_STRING_BUFFER_SIZE, "%d", (int)num);
    if ((result < 0) || (result > (INT_TO_STRING_BUFFER_SIZE - 1))) {
        stack[sp].ptr = NULL;
    } else {
        stack[sp].ptr = string;
    }
    return sp;
}

unsigned
builtin_setenv_func(void *void_stack, unsigned sp, PZ_Heap *heap)
{
    Stack_Value *stack = static_cast<Stack_Value*>(void_stack);
    int         result;
    const char *value = (const char *)stack[sp--].ptr;
    const char *name = (const char *)stack[sp--].ptr;

    result = setenv(name, value, 1);

    stack[++sp].u32 = !result;

    return sp;
}

unsigned
builtin_gettimeofday_func(void *void_stack, unsigned sp, PZ_Heap *heap)
{
    Stack_Value    *stack = static_cast<Stack_Value*>(void_stack);
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
builtin_concat_string_func(void *void_stack, unsigned sp, PZ_Heap *heap)
{
    const char  *s1, *s2;
    char        *s;
    size_t       len;
    Stack_Value *stack = static_cast<Stack_Value*>(void_stack);
    unsigned     old_sp = sp;

    s2 = (const char *)stack[sp--].ptr;
    s1 = (const char *)stack[sp].ptr;

    len = strlen(s1) + strlen(s2) + 1;
    s = static_cast<char*>(
        pz_gc_alloc_bytes(heap, sizeof(char) * len, &stack[old_sp]));
    strcpy(s, s1);
    strcat(s, s2);

    stack[sp].ptr = s;
    return sp;
}

unsigned
builtin_die_func(void *void_stack, unsigned sp, PZ_Heap *heap)
{
    const char  *s;
    Stack_Value *stack = static_cast<Stack_Value*>(void_stack);

    s = (const char *)stack[sp].ptr;
    fprintf(stderr, "Die: %s\n", s);
    exit(1);
}

unsigned
builtin_set_parameter_func(void *void_stack, unsigned sp, PZ_Heap *heap)
{
    Stack_Value *stack = static_cast<Stack_Value*>(void_stack);

    int32_t value = stack[sp].s32;
    const char *name = (const char *)stack[sp-1].ptr;
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

