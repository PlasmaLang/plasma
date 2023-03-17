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
                context.expr_stack[++context.esp].u8 = context.read_next_imm<uint8_t>();
                pz_trace_instr(context.rsp, "load imm:8");
                break;
            case PZT_LOAD_IMMEDIATE_16:
                context.expr_stack[++context.esp].u16 = context.read_next_imm<uint16_t>();
                pz_trace_instr(context.rsp, "load imm:16");
                break;
            case PZT_LOAD_IMMEDIATE_32:
                context.expr_stack[++context.esp].u32 = context.read_next_imm<uint32_t>();
                pz_trace_instr(context.rsp, "load imm:32");
                break;
            case PZT_LOAD_IMMEDIATE_64:
                context.expr_stack[++context.esp].u64 = context.read_next_imm<uint64_t>();
                pz_trace_instr(context.rsp, "load imm:64");
                break;
            case PZT_ZE_8_16:
                context.expr_stack[context.esp].u16 =
                    context.expr_stack[context.esp].u8;
                pz_trace_instr(context.rsp, "ze:8:16");
                break;
            case PZT_ZE_8_32:
                context.expr_stack[context.esp].u32 =
                    context.expr_stack[context.esp].u8;
                pz_trace_instr(context.rsp, "ze:8:32");
                break;
            case PZT_ZE_8_64:
                context.expr_stack[context.esp].u64 =
                    context.expr_stack[context.esp].u8;
                pz_trace_instr(context.rsp, "ze:8:64");
                break;
            case PZT_ZE_16_32:
                context.expr_stack[context.esp].u32 =
                    context.expr_stack[context.esp].u16;
                pz_trace_instr(context.rsp, "ze:16:32");
                break;
            case PZT_ZE_16_64:
                context.expr_stack[context.esp].u64 =
                    context.expr_stack[context.esp].u16;
                pz_trace_instr(context.rsp, "ze:16:64");
                break;
            case PZT_ZE_32_64:
                context.expr_stack[context.esp].u64 =
                    context.expr_stack[context.esp].u32;
                pz_trace_instr(context.rsp, "ze:32:64");
                break;
            case PZT_SE_8_16:
                context.expr_stack[context.esp].s16 =
                    context.expr_stack[context.esp].s8;
                pz_trace_instr(context.rsp, "se:8:16");
                break;
            case PZT_SE_8_32:
                context.expr_stack[context.esp].s32 =
                    context.expr_stack[context.esp].s8;
                pz_trace_instr(context.rsp, "se:8:32");
                break;
            case PZT_SE_8_64:
                context.expr_stack[context.esp].s64 =
                    context.expr_stack[context.esp].s8;
                pz_trace_instr(context.rsp, "se:8:64");
                break;
            case PZT_SE_16_32:
                context.expr_stack[context.esp].s32 =
                    context.expr_stack[context.esp].s16;
                pz_trace_instr(context.rsp, "se:16:32");
                break;
            case PZT_SE_16_64:
                context.expr_stack[context.esp].s64 =
                    context.expr_stack[context.esp].s16;
                pz_trace_instr(context.rsp, "se:16:64");
                break;
            case PZT_SE_32_64:
                context.expr_stack[context.esp].s64 =
                    context.expr_stack[context.esp].s32;
                pz_trace_instr(context.rsp, "se:32:64");
                break;
            case PZT_TRUNC_64_32:
                context.expr_stack[context.esp].u32 =
                    context.expr_stack[context.esp].u64 & 0xFFFFFFFFu;
                pz_trace_instr(context.rsp, "trunc:64:32");
                break;
            case PZT_TRUNC_64_16:
                context.expr_stack[context.esp].u16 =
                    context.expr_stack[context.esp].u64 & 0xFFFF;
                pz_trace_instr(context.rsp, "trunc:64:16");
                break;
            case PZT_TRUNC_64_8:
                context.expr_stack[context.esp].u8 =
                    context.expr_stack[context.esp].u64 & 0xFF;
                pz_trace_instr(context.rsp, "trunc:64:8");
                break;
            case PZT_TRUNC_32_16:
                context.expr_stack[context.esp].u16 =
                    context.expr_stack[context.esp].u32 & 0xFFFF;
                pz_trace_instr(context.rsp, "trunc:32:16");
                break;
            case PZT_TRUNC_32_8:
                context.expr_stack[context.esp].u8 =
                    context.expr_stack[context.esp].u32 & 0xFF;
                pz_trace_instr(context.rsp, "trunc:32:8");
                break;
            case PZT_TRUNC_16_8:
                context.expr_stack[context.esp].u8 =
                    context.expr_stack[context.esp].u16 & 0xFF;
                pz_trace_instr(context.rsp, "trunc:16:8");
                break;

#define PZ_RUN_ARITHMETIC(opcode_base, width, signedness, operator, op_name) \
    case opcode_base##_##width:                                              \
        context.expr_stack[context.esp - 1].signedness##width =              \
            (context.expr_stack[context.esp - 1]                             \
                 .signedness##width                                          \
                 operator context.expr_stack[context.esp]                    \
                 .signedness##width);                                        \
        context.esp--;                                                       \
        pz_trace_instr(context.rsp, op_name);                                \
        break
// clang-format off
#define PZ_RUN_ARITHMETIC1(opcode_base, width, signedness, operator, op_name) \
    case opcode_base##_##width:                                               \
        context.expr_stack[context.esp].signedness##width =                   \
                operator context.expr_stack[context.esp].signedness##width;   \
        pz_trace_instr(context.rsp, op_name);                                 \
        break
// clang-format on

                PZ_RUN_ARITHMETIC(PZT_ADD, 8, s, +, "add:8");
                PZ_RUN_ARITHMETIC(PZT_ADD, 16, s, +, "add:16");
                PZ_RUN_ARITHMETIC(PZT_ADD, 32, s, +, "add:32");
                PZ_RUN_ARITHMETIC(PZT_ADD, 64, s, +, "add:64");
                PZ_RUN_ARITHMETIC(PZT_SUB, 8, s, -, "sub:8");
                PZ_RUN_ARITHMETIC(PZT_SUB, 16, s, -, "sub:16");
                PZ_RUN_ARITHMETIC(PZT_SUB, 32, s, -, "sub:32");
                PZ_RUN_ARITHMETIC(PZT_SUB, 64, s, -, "sub:64");
                PZ_RUN_ARITHMETIC(PZT_MUL, 8, s, *, "mul:8");
                PZ_RUN_ARITHMETIC(PZT_MUL, 16, s, *, "mul:16");
                PZ_RUN_ARITHMETIC(PZT_MUL, 32, s, *, "mul:32");
                PZ_RUN_ARITHMETIC(PZT_MUL, 64, s, *, "mul:64");
                PZ_RUN_ARITHMETIC(PZT_DIV, 8, s, /, "div:8");
                PZ_RUN_ARITHMETIC(PZT_DIV, 16, s, /, "div:16");
                PZ_RUN_ARITHMETIC(PZT_DIV, 32, s, /, "div:32");
                PZ_RUN_ARITHMETIC(PZT_DIV, 64, s, /, "div:64");
                PZ_RUN_ARITHMETIC(PZT_MOD, 8, s, %, "rem:8");
                PZ_RUN_ARITHMETIC(PZT_MOD, 16, s, %, "rem:16");
                PZ_RUN_ARITHMETIC(PZT_MOD, 32, s, %, "rem:32");
                PZ_RUN_ARITHMETIC(PZT_MOD, 64, s, %, "rem:64");
                PZ_RUN_ARITHMETIC(PZT_AND, 8, u, &, "and:8");
                PZ_RUN_ARITHMETIC(PZT_AND, 16, u, &, "and:16");
                PZ_RUN_ARITHMETIC(PZT_AND, 32, u, &, "and:32");
                PZ_RUN_ARITHMETIC(PZT_AND, 64, u, &, "and:64");
                PZ_RUN_ARITHMETIC(PZT_OR, 8, u, |, "or:8");
                PZ_RUN_ARITHMETIC(PZT_OR, 16, u, |, "or:16");
                PZ_RUN_ARITHMETIC(PZT_OR, 32, u, |, "or:32");
                PZ_RUN_ARITHMETIC(PZT_OR, 64, u, |, "or:64");
                PZ_RUN_ARITHMETIC(PZT_XOR, 8, u, ^, "xor:8");
                PZ_RUN_ARITHMETIC(PZT_XOR, 16, u, ^, "xor:16");
                PZ_RUN_ARITHMETIC(PZT_XOR, 32, u, ^, "xor:32");
                PZ_RUN_ARITHMETIC(PZT_XOR, 64, u, ^, "xor:64");
                PZ_RUN_ARITHMETIC(PZT_LT_U, 8, u, <, "ltu:8");
                PZ_RUN_ARITHMETIC(PZT_LT_U, 16, u, <, "ltu:16");
                PZ_RUN_ARITHMETIC(PZT_LT_U, 32, u, <, "ltu:32");
                PZ_RUN_ARITHMETIC(PZT_LT_U, 64, u, <, "ltu:64");
                PZ_RUN_ARITHMETIC(PZT_LT_S, 8, s, <, "lts:8");
                PZ_RUN_ARITHMETIC(PZT_LT_S, 16, s, <, "lts:16");
                PZ_RUN_ARITHMETIC(PZT_LT_S, 32, s, <, "lts:32");
                PZ_RUN_ARITHMETIC(PZT_LT_S, 64, s, <, "lts:64");
                PZ_RUN_ARITHMETIC(PZT_GT_U, 8, u, >, "gtu:8");
                PZ_RUN_ARITHMETIC(PZT_GT_U, 16, u, >, "gtu:16");
                PZ_RUN_ARITHMETIC(PZT_GT_U, 32, u, >, "gtu:32");
                PZ_RUN_ARITHMETIC(PZT_GT_U, 64, u, >, "gtu:64");
                PZ_RUN_ARITHMETIC(PZT_GT_S, 8, s, >, "gts:8");
                PZ_RUN_ARITHMETIC(PZT_GT_S, 16, s, >, "gts:16");
                PZ_RUN_ARITHMETIC(PZT_GT_S, 32, s, >, "gts:32");
                PZ_RUN_ARITHMETIC(PZT_GT_S, 64, s, >, "gts:64");
                PZ_RUN_ARITHMETIC(PZT_EQ, 8, s, ==, "eq:8");
                PZ_RUN_ARITHMETIC(PZT_EQ, 16, s, ==, "eq:16");
                PZ_RUN_ARITHMETIC(PZT_EQ, 32, s, ==, "eq:32");
                PZ_RUN_ARITHMETIC(PZT_EQ, 64, s, ==, "eq:64");
                PZ_RUN_ARITHMETIC1(PZT_NOT, 8, u, !, "not:8");
                PZ_RUN_ARITHMETIC1(PZT_NOT, 16, u, !, "not:16");
                PZ_RUN_ARITHMETIC1(PZT_NOT, 32, u, !, "not:16");
                PZ_RUN_ARITHMETIC1(PZT_NOT, 64, u, !, "not:16");

#undef PZ_RUN_ARITHMETIC
#undef PZ_RUN_ARITHMETIC1

#define PZ_RUN_SHIFT(opcode_base, width, operator, op_name) \
    case opcode_base##_##width:                             \
        context.expr_stack[context.esp - 1].u##width =      \
            (context.expr_stack[context.esp - 1]            \
                 .u##width                                  \
                 operator context.expr_stack[context.esp]   \
                 .u8);                                      \
        context.esp--;                                      \
        pz_trace_instr(context.rsp, op_name);               \
        break

                PZ_RUN_SHIFT(PZT_LSHIFT, 8, <<, "lshift:8");
                PZ_RUN_SHIFT(PZT_LSHIFT, 16, <<, "lshift:16");
                PZ_RUN_SHIFT(PZT_LSHIFT, 32, <<, "lshift:32");
                PZ_RUN_SHIFT(PZT_LSHIFT, 64, <<, "lshift:64");
                PZ_RUN_SHIFT(PZT_RSHIFT, 8, >>, "rshift:8");
                PZ_RUN_SHIFT(PZT_RSHIFT, 16, >>, "rshift:16");
                PZ_RUN_SHIFT(PZT_RSHIFT, 32, >>, "rshift:32");
                PZ_RUN_SHIFT(PZT_RSHIFT, 64, >>, "rshift:64");

#undef PZ_RUN_SHIFT

            case PZT_DUP:
                context.esp++;
                context.expr_stack[context.esp] =
                    context.expr_stack[context.esp - 1];
                pz_trace_instr(context.rsp, "dup");
                break;
            case PZT_DROP:
                context.esp--;
                pz_trace_instr(context.rsp, "drop");
                break;
            case PZT_SWAP: {
                StackValue temp;
                temp = context.expr_stack[context.esp];
                context.expr_stack[context.esp] =
                    context.expr_stack[context.esp - 1];
                context.expr_stack[context.esp - 1] = temp;
                pz_trace_instr(context.rsp, "swap");
                break;
            }
            case PZT_ROLL: {
                uint8_t depth = context.read_next_imm<uint8_t>();
                StackValue temp;
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
                        temp = context.expr_stack[context.esp - depth];
                        for (int i = depth; i > 0; i--) {
                            context.expr_stack[context.esp - i] =
                                context.expr_stack[context.esp - (i - 1)];
                        }
                        context.expr_stack[context.esp] = temp;
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
                context.esp++;
                context.expr_stack[context.esp] =
                    context.expr_stack[context.esp - depth];
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
                Closure * closure;

                closure     = (Closure *)context.expr_stack[context.esp--].ptr;
                context.execute_closure(closure);

                pz_trace_instr(context.rsp, "call_ind");
                break;
            }
            case PZT_TCALL_PROC:
                context.jump(context.read_next_imm<uint8_t*>());
                pz_trace_instr(context.rsp, "tcall_proc");
                break;
            case PZT_CJMP_8: {
                uint8_t *dest = context.read_next_imm<uint8_t*>();
                if (context.expr_stack[context.esp--].u8) {
                    context.jump(dest);
                    pz_trace_instr(context.rsp, "cjmp:8 taken");
                } else {
                    pz_trace_instr(context.rsp, "cjmp:8 not taken");
                }
                break;
            }
            case PZT_CJMP_16: {
                uint8_t *dest = context.read_next_imm<uint8_t*>();
                if (context.expr_stack[context.esp--].u16) {
                    context.jump(dest);
                    pz_trace_instr(context.rsp, "cjmp:16 taken");
                } else {
                    pz_trace_instr(context.rsp, "cjmp:16 not taken");
                }
                break;
            }
            case PZT_CJMP_32: {
                uint8_t *dest = context.read_next_imm<uint8_t*>();
                if (context.expr_stack[context.esp--].u32) {
                    context.jump(dest);
                    pz_trace_instr(context.rsp, "cjmp:32 taken");
                } else {
                    pz_trace_instr(context.rsp, "cjmp:32 not taken");
                }
                break;
            }
            case PZT_CJMP_64: {
                uint8_t *dest = context.read_next_imm<uint8_t*>();
                if (context.expr_stack[context.esp--].u64) {
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
                void * addr;

                // pz_gc_alloc uses size in machine words, round the value
                // up and convert it to words rather than bytes.
                size = (size + WORDSIZE_BYTES - 1) / WORDSIZE_BYTES;
                addr = context.alloc(size);
                context.expr_stack[++context.esp].ptr = addr;
                pz_trace_instr(context.rsp, "alloc");
                break;
            }
            case PZT_MAKE_CLOSURE: {
                uint8_t *code = context.read_next_imm<uint8_t*>();
                void *data = context.expr_stack[context.esp].ptr;
                Closure * closure =
                    new (context) Closure(code, data);
                context.expr_stack[context.esp].ptr = closure;
                pz_trace_instr(context.rsp, "make_closure");
                break;
            }
            case PZT_LOAD_8: {
                /* (ptr - * ptr) */
                uint16_t offset = context.read_next_imm<uint16_t>();
                void *addr = 
                    (uint8_t *)context.expr_stack[context.esp].ptr + offset;
                context.expr_stack[context.esp + 1].ptr =
                    context.expr_stack[context.esp].ptr;
                context.expr_stack[context.esp].u8 = *(uint8_t *)addr;
                context.esp++;
                pz_trace_instr(context.rsp, "load_8");
                break;
            }
            case PZT_LOAD_16: {
                /* (ptr - * ptr) */
                uint16_t offset = context.read_next_imm<uint16_t>();
                void *addr =
                    (uint8_t *)context.expr_stack[context.esp].ptr + offset;
                context.expr_stack[context.esp + 1].ptr =
                    context.expr_stack[context.esp].ptr;
                context.expr_stack[context.esp].u16 = *(uint16_t *)addr;
                context.esp++;
                pz_trace_instr(context.rsp, "load_16");
                break;
            }
            case PZT_LOAD_32: {
                /* (ptr - * ptr) */
                uint16_t offset = context.read_next_imm<uint16_t>();
                void *addr =
                    (uint8_t *)context.expr_stack[context.esp].ptr + offset;
                context.expr_stack[context.esp + 1].ptr =
                    context.expr_stack[context.esp].ptr;
                context.expr_stack[context.esp].u32 = *(uint32_t *)addr;
                context.esp++;
                pz_trace_instr(context.rsp, "load_32");
                break;
            }
            case PZT_LOAD_64: {
                /* (ptr - * ptr) */
                uint16_t offset = context.read_next_imm<uint16_t>();
                void *addr = 
                    (uint8_t *)context.expr_stack[context.esp].ptr + offset;
                context.expr_stack[context.esp + 1].ptr =
                    context.expr_stack[context.esp].ptr;
                context.expr_stack[context.esp].u64 = *(uint64_t *)addr;
                context.esp++;
                pz_trace_instr(context.rsp, "load_64");
                break;
            }
            case PZT_LOAD_PTR: {
                /* (ptr - ptr ptr) */
                uint16_t offset = context.read_next_imm<uint16_t>();
                void *addr =
                    (uint8_t *)context.expr_stack[context.esp].ptr + offset;
                context.expr_stack[context.esp + 1].ptr =
                    context.expr_stack[context.esp].ptr;
                context.expr_stack[context.esp].ptr = *(void **)addr;
                context.esp++;
                pz_trace_instr(context.rsp, "load_ptr");
                break;
            }
            case PZT_STORE_8: {
                /* (* ptr - ptr) */
                uint16_t offset = context.read_next_imm<uint16_t>();
                void *addr =
                    (uint8_t *)context.expr_stack[context.esp].ptr + offset;
                *(uint8_t *)addr = context.expr_stack[context.esp - 1].u8;
                context.expr_stack[context.esp - 1].ptr =
                    context.expr_stack[context.esp].ptr;
                context.esp--;
                pz_trace_instr(context.rsp, "store_8");
                break;
            }
            case PZT_STORE_16: {
                /* (* ptr - ptr) */
                uint16_t offset = context.read_next_imm<uint16_t>();
                void *addr =
                    (uint8_t *)context.expr_stack[context.esp].ptr + offset;
                *(uint16_t *)addr = context.expr_stack[context.esp - 1].u16;
                context.expr_stack[context.esp - 1].ptr =
                    context.expr_stack[context.esp].ptr;
                context.esp--;
                pz_trace_instr(context.rsp, "store_16");
                break;
            }
            case PZT_STORE_32: {
                /* (* ptr - ptr) */
                uint16_t offset = context.read_next_imm<uint16_t>();
                void *addr = 
                    (uint8_t *)context.expr_stack[context.esp].ptr + offset;
                *(uint32_t *)addr = context.expr_stack[context.esp - 1].u32;
                context.expr_stack[context.esp - 1].ptr =
                    context.expr_stack[context.esp].ptr;
                context.esp--;
                pz_trace_instr(context.rsp, "store_32");
                break;
            }
            case PZT_STORE_64: {
                /* (* ptr - ptr) */
                uint16_t offset = context.read_next_imm<uint16_t>();
                void *addr =
                    (uint8_t *)context.expr_stack[context.esp].ptr + offset;
                *(uint64_t *)addr = context.expr_stack[context.esp - 1].u64;
                context.expr_stack[context.esp - 1].ptr =
                    context.expr_stack[context.esp].ptr;
                context.esp--;
                pz_trace_instr(context.rsp, "store_64");
                break;
            }
            case PZT_GET_ENV: {
                context.expr_stack[++context.esp].ptr = context.env;
                pz_trace_instr(context.rsp, "get_env");
                break;
            }

            case PZT_END:
                retcode = context.expr_stack[context.esp].s32;
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
