/*
 * Plasma bytecode exection (generic portable version)
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2015-2019, 2021, 2023 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#include "pz_common.h"

#include "pz_gc.h"
#include "pz_interp.h"
#include "pz_trace.h"
#include "pz_util.h"

#include <stdio.h>

#include "pz_generic_closure.h"
#include "pz_generic_run.h"

namespace pz {

int generic_main_loop(Context & context, Heap * heap, Closure * closure,
                      PZ & pz)
{
    int retcode;
    context.execute_closure(closure);

    pz_trace_state(heap,
                   context.get_ip(),
                   context.env,
                   context.rsp,
                   context.esp,
                   (uint64_t *)context.expr_stack.ptr());
    while (true) {
        InstructionToken token = context.read_next_token();

        switch (token) {
            case PZT_NOP:
                pz_trace_instr(context.rsp, "nop");
                break;
            case PZT_LOAD_IMMEDIATE_8:
                context.expr_push<uint8_t>(context.read_next_imm<uint8_t>());
                pz_trace_instr(context.rsp, "load imm:8");
                break;
            case PZT_LOAD_IMMEDIATE_16:
                context.expr_push<uint16_t>(context.read_next_imm<uint16_t>());
                pz_trace_instr(context.rsp, "load imm:16");
                break;
            case PZT_LOAD_IMMEDIATE_32:
                context.expr_push<uint32_t>(context.read_next_imm<uint32_t>());
                pz_trace_instr(context.rsp, "load imm:32");
                break;
            case PZT_LOAD_IMMEDIATE_64:
                context.expr_push<uint64_t>(context.read_next_imm<uint64_t>());
                pz_trace_instr(context.rsp, "load imm:64");
                break;
            case PZT_ZE_8_16:
                context.expr_tos<uint16_t>() = context.expr_tos<uint8_t>();
                pz_trace_instr(context.rsp, "ze:8:16");
                break;
            case PZT_ZE_8_32:
                context.expr_tos<uint32_t>() = context.expr_tos<uint8_t>();
                pz_trace_instr(context.rsp, "ze:8:32");
                break;
            case PZT_ZE_8_64:
                context.expr_tos<uint64_t>() = context.expr_tos<uint8_t>();
                pz_trace_instr(context.rsp, "ze:8:64");
                break;
            case PZT_ZE_16_32:
                context.expr_tos<uint32_t>() = context.expr_tos<uint16_t>();
                pz_trace_instr(context.rsp, "ze:16:32");
                break;
            case PZT_ZE_16_64:
                context.expr_tos<uint64_t>() = context.expr_tos<uint16_t>();
                pz_trace_instr(context.rsp, "ze:16:64");
                break;
            case PZT_ZE_32_64:
                context.expr_tos<uint64_t>() = context.expr_tos<uint32_t>();
                pz_trace_instr(context.rsp, "ze:32:64");
                break;
            case PZT_SE_8_16:
                context.expr_tos<int16_t>() = context.expr_tos<int8_t>();
                pz_trace_instr(context.rsp, "se:8:16");
                break;
            case PZT_SE_8_32:
                context.expr_tos<int32_t>() = context.expr_tos<int8_t>();
                pz_trace_instr(context.rsp, "se:8:32");
                break;
            case PZT_SE_8_64:
                context.expr_tos<int64_t>() = context.expr_tos<int8_t>();
                pz_trace_instr(context.rsp, "se:8:64");
                break;
            case PZT_SE_16_32:
                context.expr_tos<int32_t>() = context.expr_tos<int16_t>();
                pz_trace_instr(context.rsp, "se:16:32");
                break;
            case PZT_SE_16_64:
                context.expr_tos<int64_t>() = context.expr_tos<int16_t>();
                pz_trace_instr(context.rsp, "se:16:64");
                break;
            case PZT_SE_32_64:
                context.expr_tos<int64_t>() = context.expr_tos<int32_t>();
                pz_trace_instr(context.rsp, "se:32:64");
                break;
            case PZT_TRUNC_64_32:
                context.expr_tos<uint32_t>() =
                    context.expr_tos<uint64_t>() & 0xFFFFFFFFu;
                pz_trace_instr(context.rsp, "trunc:64:32");
                break;
            case PZT_TRUNC_64_16:
                context.expr_tos<uint16_t>() =
                    context.expr_tos<uint64_t>() & 0xFFFF;
                pz_trace_instr(context.rsp, "trunc:64:16");
                break;
            case PZT_TRUNC_64_8:
                context.expr_tos<uint8_t>() =
                    context.expr_tos<uint64_t>() & 0xFF;
                pz_trace_instr(context.rsp, "trunc:64:8");
                break;
            case PZT_TRUNC_32_16:
                context.expr_tos<uint16_t>() =
                    context.expr_tos<uint32_t>() & 0xFFFF;
                pz_trace_instr(context.rsp, "trunc:32:16");
                break;
            case PZT_TRUNC_32_8:
                context.expr_tos<uint8_t>() =
                    context.expr_tos<uint32_t>() & 0xFF;
                pz_trace_instr(context.rsp, "trunc:32:8");
                break;
            case PZT_TRUNC_16_8:
                context.expr_tos<uint8_t>() =
                    context.expr_tos<uint16_t>() & 0xFF;
                pz_trace_instr(context.rsp, "trunc:16:8");
                break;

// clang-format off
#define PZ_RUN_ARITHMETIC(label, T, operator, op_name)  \
    case label: {                                       \
        T arg2 = context.expr_pop<T>();                 \
        T arg1 = context.expr_tos<T>();                 \
        context.expr_tos<T>() = arg1 operator arg2;     \
        pz_trace_instr(context.rsp, op_name);           \
        break;                                          \
    }
#define PZ_RUN_ARITHMETIC1(label, T,  operator, op_name)    \
    case label:                                             \
        context.expr_tos<T>() =                             \
                operator context.expr_tos<T>();             \
        pz_trace_instr(context.rsp, op_name);               \
        break

                PZ_RUN_ARITHMETIC(PZT_ADD_8,   int8_t,   +,  "add:8");
                PZ_RUN_ARITHMETIC(PZT_ADD_16,  int16_t,  +,  "add:16");
                PZ_RUN_ARITHMETIC(PZT_ADD_32,  int32_t,  +,  "add:32");
                PZ_RUN_ARITHMETIC(PZT_ADD_64,  int64_t,  +,  "add:64");
                PZ_RUN_ARITHMETIC(PZT_SUB_8,   int8_t,   -,  "sub:8");
                PZ_RUN_ARITHMETIC(PZT_SUB_16,  int16_t,  -,  "sub:16");
                PZ_RUN_ARITHMETIC(PZT_SUB_32,  int32_t,  -,  "sub:32");
                PZ_RUN_ARITHMETIC(PZT_SUB_64,  int64_t,  -,  "sub:64");
                PZ_RUN_ARITHMETIC(PZT_MUL_8,   int8_t,   *,  "mul:8");
                PZ_RUN_ARITHMETIC(PZT_MUL_16,  int16_t,  *,  "mul:16");
                PZ_RUN_ARITHMETIC(PZT_MUL_32,  int32_t,  *,  "mul:32");
                PZ_RUN_ARITHMETIC(PZT_MUL_64,  int64_t,  *,  "mul:64");
                PZ_RUN_ARITHMETIC(PZT_DIV_8,   int8_t,   /,  "div:8");
                PZ_RUN_ARITHMETIC(PZT_DIV_16,  int16_t,  /,  "div:16");
                PZ_RUN_ARITHMETIC(PZT_DIV_32,  int32_t,  /,  "div:32");
                PZ_RUN_ARITHMETIC(PZT_DIV_64,  int64_t,  /,  "div:64");
                PZ_RUN_ARITHMETIC(PZT_MOD_8,   int8_t,   %,  "rem:8");
                PZ_RUN_ARITHMETIC(PZT_MOD_16,  int16_t,  %,  "rem:16");
                PZ_RUN_ARITHMETIC(PZT_MOD_32,  int32_t,  %,  "rem:32");
                PZ_RUN_ARITHMETIC(PZT_MOD_64,  int64_t,  %,  "rem:64");
                PZ_RUN_ARITHMETIC(PZT_AND_8,   uint8_t,  &,  "and:8");
                PZ_RUN_ARITHMETIC(PZT_AND_16,  uint16_t, &,  "and:16");
                PZ_RUN_ARITHMETIC(PZT_AND_32,  uint32_t, &,  "and:32");
                PZ_RUN_ARITHMETIC(PZT_AND_64,  uint64_t, &,  "and:64");
                PZ_RUN_ARITHMETIC(PZT_OR_8,    uint8_t,  |,  "or:8");
                PZ_RUN_ARITHMETIC(PZT_OR_16,   uint16_t, |,  "or:16");
                PZ_RUN_ARITHMETIC(PZT_OR_32,   uint32_t, |,  "or:32");
                PZ_RUN_ARITHMETIC(PZT_OR_64,   uint64_t, |,  "or:64");
                PZ_RUN_ARITHMETIC(PZT_XOR_8,   uint8_t,  ^,  "xor:8");
                PZ_RUN_ARITHMETIC(PZT_XOR_16,  uint16_t, ^,  "xor:16");
                PZ_RUN_ARITHMETIC(PZT_XOR_32,  uint32_t, ^,  "xor:32");
                PZ_RUN_ARITHMETIC(PZT_XOR_64,  uint64_t, ^,  "xor:64");
                PZ_RUN_ARITHMETIC(PZT_LT_U_8,  uint8_t,  <,  "ltu:8");
                PZ_RUN_ARITHMETIC(PZT_LT_U_16, uint16_t, <,  "ltu:16");
                PZ_RUN_ARITHMETIC(PZT_LT_U_32, uint32_t, <,  "ltu:32");
                PZ_RUN_ARITHMETIC(PZT_LT_U_64, uint64_t, <,  "ltu:64");
                PZ_RUN_ARITHMETIC(PZT_LT_S_8,  int8_t,   <,  "lts:8");
                PZ_RUN_ARITHMETIC(PZT_LT_S_16, int16_t,  <,  "lts:16");
                PZ_RUN_ARITHMETIC(PZT_LT_S_32, int32_t,  <,  "lts:32");
                PZ_RUN_ARITHMETIC(PZT_LT_S_64, int64_t,  <,  "lts:64");
                PZ_RUN_ARITHMETIC(PZT_GT_U_8,  uint8_t,  >,  "gtu:8");
                PZ_RUN_ARITHMETIC(PZT_GT_U_16, uint16_t, >,  "gtu:16");
                PZ_RUN_ARITHMETIC(PZT_GT_U_32, uint32_t, >,  "gtu:32");
                PZ_RUN_ARITHMETIC(PZT_GT_U_64, uint64_t, >,  "gtu:64");
                PZ_RUN_ARITHMETIC(PZT_GT_S_8,  int8_t,   >,  "gts:8");
                PZ_RUN_ARITHMETIC(PZT_GT_S_16, int16_t,  >,  "gts:16");
                PZ_RUN_ARITHMETIC(PZT_GT_S_32, int32_t,  >,  "gts:32");
                PZ_RUN_ARITHMETIC(PZT_GT_S_64, int64_t,  >,  "gts:64");
                PZ_RUN_ARITHMETIC(PZT_EQ_8,    int8_t,   ==, "eq:8");
                PZ_RUN_ARITHMETIC(PZT_EQ_16,   int16_t,  ==, "eq:16");
                PZ_RUN_ARITHMETIC(PZT_EQ_32,   int32_t,  ==, "eq:32");
                PZ_RUN_ARITHMETIC(PZT_EQ_64,   int64_t,  ==, "eq:64");

                PZ_RUN_ARITHMETIC1(PZT_NOT_8,  uint8_t,  !,  "not:8");
                PZ_RUN_ARITHMETIC1(PZT_NOT_16, uint16_t, !,  "not:16");
                PZ_RUN_ARITHMETIC1(PZT_NOT_32, uint32_t, !,  "not:16");
                PZ_RUN_ARITHMETIC1(PZT_NOT_64, uint64_t, !,  "not:16");
// clang-format on

#undef PZ_RUN_ARITHMETIC
#undef PZ_RUN_ARITHMETIC1

#define PZ_RUN_SHIFT(label, T, operator, op_name)   \
    case label: {                                   \
        uint8_t arg2 = context.expr_pop<uint8_t>(); \
        T arg1 = context.expr_tos<T>();             \
        context.expr_tos<T>() = arg1 operator arg2; \
        pz_trace_instr(context.rsp, op_name);       \
        break;                                      \
    }

                PZ_RUN_SHIFT(PZT_LSHIFT_8,  uint8_t,  <<, "lshift:8");
                PZ_RUN_SHIFT(PZT_LSHIFT_16, uint16_t, <<, "lshift:16");
                PZ_RUN_SHIFT(PZT_LSHIFT_32, uint32_t, <<, "lshift:32");
                PZ_RUN_SHIFT(PZT_LSHIFT_64, uint64_t, <<, "lshift:64");
                PZ_RUN_SHIFT(PZT_RSHIFT_8,  uint8_t,  >>, "rshift:8");
                PZ_RUN_SHIFT(PZT_RSHIFT_16, uint16_t, >>, "rshift:16");
                PZ_RUN_SHIFT(PZT_RSHIFT_32, uint32_t, >>, "rshift:32");
                PZ_RUN_SHIFT(PZT_RSHIFT_64, uint64_t, >>, "rshift:64");

#undef PZ_RUN_SHIFT

            case PZT_DUP:
                context.expr_push<StackValue>(
                    context.expr_tos<StackValue>());
                pz_trace_instr(context.rsp, "dup");
                break;
            case PZT_DROP:
                context.expr_pop<StackValue>();
                pz_trace_instr(context.rsp, "pop");
                break;
            case PZT_SWAP: {
                StackValue temp;
                temp = context.expr_tos<StackValue>();
                context.expr_tos<StackValue>() =
                    context.expr_tos<StackValue>(1);
                context.expr_tos<StackValue>(1) = temp;
                pz_trace_instr(context.rsp, "swap");
                break;
            }
            case PZT_ROLL: {
                uint8_t depth = context.read_next_imm<uint8_t>();
                switch (depth) {
                    case 0:
                        fprintf(stderr, "Illegal rot depth 0");
                        abort();
                    case 1:
                        break;
                    default:
                        /*
                         * subtract 1 as the 1st element on the stack is
                         * context.esp - 0, not context.esp - 1
                         */
                        depth--;
                        StackValue temp = context.expr_tos<StackValue>(depth);
                        for (int i = depth; i > 0; i--) {
                            context.expr_tos<StackValue>(i) =
                                context.expr_tos<StackValue>(i - 1);
                        }
                        context.expr_tos<StackValue>() = temp;
                }
                pz_trace_instr2(context.rsp, "roll", depth + 1);
                break;
            }
            case PZT_PICK: {
                /*
                 * As with PZT_ROLL we would subract 1 here, but we also
                 * have to add 1 because we increment the stack pointer
                 * before accessing the stack.
                 */
                uint8_t depth = context.read_next_imm<uint8_t>();
                context.expr_push<StackValue>(
                    context.expr_tos<StackValue>(depth - 1));
                pz_trace_instr2(context.rsp, "pick", depth);
                break;
            }
            case PZT_CALL: {
                Closure * closure = context.read_next_imm<Closure*>();

                context.return_stack[++context.rsp] =
                    static_cast<uint8_t *>(context.env);
                context.return_stack[++context.rsp] =
                    reinterpret_cast<uint8_t *>(context.get_ip());
                context.execute_closure(closure);

                pz_trace_instr(context.rsp, "call");
                break;
            }
            case PZT_CALL_IND: {
                Closure * closure;

                context.return_stack[++context.rsp] =
                    static_cast<uint8_t *>(context.env);
                context.return_stack[++context.rsp] = context.get_ip();

                closure     = (Closure *)context.expr_stack[context.esp--].ptr;
                context.execute_closure(closure);

                pz_trace_instr(context.rsp, "call_ind");
                break;
            }
            case PZT_CALL_PROC: {
                uint8_t *new_ip = context.read_next_imm<uint8_t*>();

                context.return_stack[++context.rsp] =
                    static_cast<uint8_t *>(context.env);
                context.return_stack[++context.rsp] = context.get_ip();
                context.jump(new_ip);
                pz_trace_instr(context.rsp, "call_proc");
                break;
            }
            case PZT_TCALL:
                context.execute_closure(context.read_next_imm<Closure*>());

                pz_trace_instr(context.rsp, "tcall");
                break;
            case PZT_TCALL_IND: {
                context.execute_closure(context.expr_pop<Closure*>());
                pz_trace_instr(context.rsp, "call_ind");
                break;
            }
            case PZT_TCALL_PROC:
                context.jump(context.read_next_imm<uint8_t*>());
                pz_trace_instr(context.rsp, "tcall_proc");
                break;
            case PZT_CJMP_8: {
                uint8_t *dest = context.read_next_imm<uint8_t*>();
                if (context.expr_pop<uint8_t>()) {
                    context.jump(dest);
                    pz_trace_instr(context.rsp, "cjmp:8 taken");
                } else {
                    pz_trace_instr(context.rsp, "cjmp:8 not taken");
                }
                break;
            }
            case PZT_CJMP_16: {
                uint8_t *dest = context.read_next_imm<uint8_t*>();
                if (context.expr_pop<uint16_t>()) {
                    context.jump(dest);
                    pz_trace_instr(context.rsp, "cjmp:16 taken");
                } else {
                    pz_trace_instr(context.rsp, "cjmp:16 not taken");
                }
                break;
            }
            case PZT_CJMP_32: {
                uint8_t *dest = context.read_next_imm<uint8_t*>();
                if (context.expr_pop<uint32_t>()) {
                    context.jump(dest);
                    pz_trace_instr(context.rsp, "cjmp:32 taken");
                } else {
                    pz_trace_instr(context.rsp, "cjmp:32 not taken");
                }
                break;
            }
            case PZT_CJMP_64: {
                uint8_t *dest = context.read_next_imm<uint8_t*>();
                if (context.expr_pop<uint64_t>()) {
                    context.jump(dest);
                    pz_trace_instr(context.rsp, "cjmp:64 taken");
                } else {
                    pz_trace_instr(context.rsp, "cjmp:64 not taken");
                }
                break;
            }
            case PZT_JMP:
                context.jump(context.read_next_imm<uint8_t*>());
                pz_trace_instr(context.rsp, "jmp");
                break;
            case PZT_RET:
                context.jump(context.return_stack[context.rsp--]);
                context.env = context.return_stack[context.rsp--];
                pz_trace_instr(context.rsp, "ret");
                break;
            case PZT_ALLOC: {
                size_t size = context.read_next_imm<size_t>();

                // pz_gc_alloc uses size in machine words, round the value
                // up and convert it to words rather than bytes.
                size = (size + WORDSIZE_BYTES - 1) / WORDSIZE_BYTES;
                context.expr_push<void*>(context.alloc(size));
                pz_trace_instr(context.rsp, "alloc");
                break;
            }
            case PZT_MAKE_CLOSURE: {
                uint8_t *code = context.read_next_imm<uint8_t*>();
                void *data = context.expr_tos<void*>();
                Closure * closure =
                    new (context) Closure(code, data);
                context.expr_tos<Closure*>() = closure;
                pz_trace_instr(context.rsp, "make_closure");
                break;
            }
            case PZT_LOAD_8: {
                /* (ptr - * ptr) */
                uint16_t offset = context.read_next_imm<uint16_t>();
                uint8_t *base = context.expr_tos<uint8_t*>();
                context.expr_tos<uint8_t>() = *(base + offset);
                context.expr_push<uint8_t*>(base);
                pz_trace_instr(context.rsp, "load_8");
                break;
            }
            case PZT_LOAD_16: {
                /* (ptr - * ptr) */
                uint16_t offset = context.read_next_imm<uint16_t>();
                uint8_t *base = context.expr_tos<uint8_t*>();
                context.expr_tos<uint16_t>() =
                    *reinterpret_cast<uint16_t*>(base + offset);
                context.expr_push<uint8_t*>(base);
                pz_trace_instr(context.rsp, "load_16");
                break;
            }
            case PZT_LOAD_32: {
                /* (ptr - * ptr) */
                uint16_t offset = context.read_next_imm<uint16_t>();
                uint8_t *base = context.expr_tos<uint8_t*>();
                context.expr_tos<uint32_t>() =
                    *reinterpret_cast<uint32_t*>(base + offset);
                context.expr_push<uint8_t*>(base);
                pz_trace_instr(context.rsp, "load_32");
                break;
            }
            case PZT_LOAD_64: {
                /* (ptr - * ptr) */
                uint16_t offset = context.read_next_imm<uint16_t>();
                uint8_t *base = context.expr_tos<uint8_t*>();
                context.expr_tos<uint64_t>() =
                    *reinterpret_cast<uint64_t*>(base + offset);
                context.expr_push<uint8_t*>(base);
                pz_trace_instr(context.rsp, "load_64");
                break;
            }
            case PZT_LOAD_PTR: {
                /* (ptr - ptr ptr) */
                uint16_t offset = context.read_next_imm<uint16_t>();
                uint8_t *base = context.expr_tos<uint8_t*>();
                context.expr_tos<uintptr_t>() =
                    *reinterpret_cast<uintptr_t*>(base + offset);
                context.expr_push<uint8_t*>(base);
                pz_trace_instr(context.rsp, "load_ptr");
                break;
            }
            case PZT_STORE_8: {
                /* (* ptr - ptr) */
                uint16_t offset = context.read_next_imm<uint16_t>();
                uint8_t *base = context.expr_pop<uint8_t*>();
                *(base + offset) = context.expr_tos<uint8_t>();
                context.expr_tos<uint8_t*>() = base;
                pz_trace_instr(context.rsp, "store_8");
                break;
            }
            case PZT_STORE_16: {
                /* (* ptr - ptr) */
                uint16_t offset = context.read_next_imm<uint16_t>();
                uint8_t *base = context.expr_pop<uint8_t*>();
                *reinterpret_cast<uint16_t*>(base + offset) =
                    context.expr_tos<uint16_t>();
                context.expr_tos<uint8_t*>() = base;
                pz_trace_instr(context.rsp, "store_16");
                break;
            }
            case PZT_STORE_32: {
                /* (* ptr - ptr) */
                uint16_t offset = context.read_next_imm<uint16_t>();
                uint8_t *base = context.expr_pop<uint8_t*>();
                *reinterpret_cast<uint32_t*>(base + offset) =
                    context.expr_tos<uint32_t>();
                context.expr_tos<uint8_t*>() = base;
                pz_trace_instr(context.rsp, "store_32");
                break;
            }
            case PZT_STORE_64: {
                /* (* ptr - ptr) */
                uint16_t offset = context.read_next_imm<uint16_t>();
                uint8_t *base = context.expr_pop<uint8_t*>();
                *reinterpret_cast<uint64_t*>(base + offset) =
                    context.expr_tos<uint64_t>();
                context.expr_tos<uint8_t*>() = base;
                pz_trace_instr(context.rsp, "store_64");
                break;
            }
            case PZT_GET_ENV: {
                context.expr_stack[++context.esp].ptr = context.env;
                pz_trace_instr(context.rsp, "get_env");
                break;
            }

            case PZT_END:
                retcode = context.expr_tos<int32_t>();
                if (context.esp != 1) {
                    fprintf(stderr,
                            "Stack misaligned, esp: %d should be 1\n",
                            context.esp);
                    abort();
                }
                pz_trace_instr(context.rsp, "end");
                pz_trace_state(heap,
                               context.get_ip(),
                               context.env,
                               context.rsp,
                               context.esp,
                               (uint64_t *)context.expr_stack.ptr());
                return retcode;
            case PZT_CCALL: {
                pz_foreign_c_func callee =
                    context.read_next_imm<pz_foreign_c_func>();
                context.esp = callee(context.expr_stack.ptr(), context.esp);
                pz_trace_instr(context.rsp, "ccall");
                break;
            }
            case PZT_CCALL_ALLOC: {
                pz_foreign_c_alloc_func callee =
                    context.read_next_imm<pz_foreign_c_alloc_func>();
                context.esp = callee(context.expr_stack.ptr(), context.esp, 
                    context);
                pz_trace_instr(context.rsp, "ccall");
                break;
            }
            case PZT_CCALL_SPECIAL: {
                pz_foreign_c_special_func callee =
                    context.read_next_imm<pz_foreign_c_special_func>();
                context.esp = callee(context.expr_stack.ptr(), context.esp, pz);
                pz_trace_instr(context.rsp, "ccall");
                break;
            }
#ifdef PZ_DEV
            case PZT_INVALID_TOKEN:
                fprintf(stderr, "Attempt to execute poisoned memory\n");
                abort();
#endif
            default:
                fprintf(stderr, "Unknown opcode\n");
                abort();
        }
        pz_trace_state(heap,
                       context.get_ip(),
                       context.env,
                       context.rsp,
                       context.esp,
                       (uint64_t *)context.expr_stack.ptr());
    }
}

}  // namespace pz
