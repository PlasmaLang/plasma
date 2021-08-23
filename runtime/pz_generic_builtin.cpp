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

unsigned pz_builtin_readline_func(void * void_stack, unsigned sp,
                                  AbstractGCTracer & gc_trace)
{
    const uint32_t READLINE_BUFFER_SIZE = 128;
    StackValue * stack = static_cast<StackValue *>(void_stack);
    NoGCScope nogc(gc_trace);

    String str("");
    do {
        FlatString *fs = FlatString::New(nogc, READLINE_BUFFER_SIZE);
        char *res = fgets(fs->buffer(), READLINE_BUFFER_SIZE, stdin);
        if (!res) {
            if (ferror(stdin)) {
                perror("stdin");
                exit(PZ_EXIT_RUNTIME_ERROR);
            } else if (feof(stdin)) {
                fs->fixSize(0);
                str = String::append(nogc, str, String(fs));
                break;
            }
        }

        int read_len = strlen(fs->buffer());
        if (read_len == 0) {
            // We don't need to process an empty string.
            break;
        }

        fs->fixSize(strlen(fs->buffer()));
        if (fs->length() > 0 && fs->buffer()[fs->length()-1] == '\n') {
            // Remove the newline character
            // TODO: If string had a way to set chars then we can simplify
            // this by doing the operation on string and having a single
            // call to append.
            fs->buffer()[fs->length()-1] = 0;
            fs->fixSize(fs->length()-1);
            str = String::append(nogc, str, String(fs));
            break;
        }
        str = String::append(nogc, str, String(fs));
        if (fs->length() != (READLINE_BUFFER_SIZE - 1)) {
            break;
        }
    } while(true);

    stack[++sp].ptr = str.ptr();

    nogc.abort_if_oom("reading stdin");

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

unsigned pz_builtin_string_concat_func(void * void_stack, unsigned sp,
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
        value  = heap_get_usage(&pz.heap());
        result = 1;
    } else if (name.equals(String("heap_collections"))) {
        value  = heap_get_collections(&pz.heap());
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

unsigned pz_builtin_char_class(void * void_stack, unsigned sp)
{
    // TODO use a unicode library.  While POSIX is locale-aware it does not
    // handle characters outside the current locale, but applications may
    // use more than a single langauge at a time.
    
    StackValue * stack = static_cast<StackValue *>(void_stack);

    uint32_t c = stack[sp].u32;
    // TODO: Use a proper FFI so we don't need to guess type tags.
    stack[sp].uptr = isspace(c) ? 0 : 1;

    return sp;
}

unsigned pz_builtin_strpos_forward(void * void_stack, unsigned sp,
         AbstractGCTracer &gc)
{
    StackValue * stack = static_cast<StackValue *>(void_stack);

    const StringPos* pos = reinterpret_cast<const StringPos*>(stack[sp].ptr);
    stack[sp].ptr = pos->forward(gc);

    return sp;
}

unsigned pz_builtin_strpos_backward(void * void_stack, unsigned sp,
        AbstractGCTracer &gc)
{
    StackValue * stack = static_cast<StackValue *>(void_stack);

    const StringPos* pos = reinterpret_cast<const StringPos*>(stack[sp].ptr);
    stack[sp].ptr = pos->backward(gc);

    return sp;
}

template<typename T>
static
uintptr_t Box(T v, GCCapability &gc) {
    T *ptr = reinterpret_cast<T*>(gc.alloc_bytes(sizeof(T)));
    *ptr = v;
    return reinterpret_cast<uintptr_t>(ptr);
}

unsigned pz_builtin_strpos_next_char(void * void_stack, unsigned sp,
        AbstractGCTracer & gc)
{
    StackValue * stack = static_cast<StackValue *>(void_stack);

    const StringPos* pos = reinterpret_cast<const StringPos*>(stack[sp].ptr);
    // XXX add pointer tagging macros.
    if (pos->at_end()) {
        stack[sp].uptr = 0;
    } else {
        stack[sp].uptr = Box(pos->next_char(), gc) | 1;
    }

    return sp;
}

unsigned pz_builtin_strpos_prev_char(void * void_stack, unsigned sp,
        AbstractGCTracer & gc)
{
    StackValue * stack = static_cast<StackValue *>(void_stack);

    const StringPos* pos = reinterpret_cast<const StringPos*>(stack[sp].ptr);
    if (pos->at_beginning()) {
        stack[sp].uptr = 0;
    } else {
        stack[sp].uptr = Box(pos->prev_char(), gc) | 1;
    }

    return sp;
}

unsigned pz_builtin_string_begin(void * void_stack, unsigned sp,
        AbstractGCTracer & gc)
{
    StackValue * stack = static_cast<StackValue *>(void_stack);

    const String string = String::from_ptr(stack[sp].ptr);
    stack[sp].ptr = string.begin(gc);

    return sp;
}

unsigned pz_builtin_string_end(void * void_stack, unsigned sp,
        AbstractGCTracer & gc)
{
    StackValue * stack = static_cast<StackValue *>(void_stack);

    const String string = String::from_ptr(stack[sp].ptr);
    stack[sp].ptr = string.end(gc);

    return sp;
}

unsigned pz_builtin_string_substring(void * void_stack, unsigned sp,
        AbstractGCTracer & gc)
{
    StackValue * stack = static_cast<StackValue *>(void_stack);

    const StringPos* pos2 = reinterpret_cast<const StringPos*>(stack[sp--].ptr);
    const StringPos* pos1 = reinterpret_cast<const StringPos*>(stack[sp].ptr);
    const String str = String::substring(gc, pos1, pos2);
    stack[sp].ptr = str.ptr();

    return sp;
}

unsigned pz_builtin_string_equals(void * void_stack, unsigned sp) {
    StackValue * stack = static_cast<StackValue *>(void_stack);

    const String str1 = String::from_ptr(stack[sp--].ptr);
    const String str2 = String::from_ptr(stack[sp].ptr);

    stack[sp].uptr = str1.equals(str2);

    return sp;
}

}  // namespace pz
