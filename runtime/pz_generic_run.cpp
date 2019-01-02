/*
 * Plasma bytecode exection (generic portable version)
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2015-2018 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#include "pz_common.h"

#include "pz_gc.h"
#include "pz_trace.h"
#include "pz_util.h"

#include <stdio.h>

#include "pz_generic_closure.h"
#include "pz_generic_run.h"

int
pz_generic_main_loop(uint8_t **return_stack,
                     unsigned rsp,
                     Stack_Value *expr_stack,
                     PZ_Heap *heap,
                     PZ_Closure *closure)
{
    int retcode;
    unsigned esp = 0;
    uint8_t *ip = static_cast<uint8_t*>(closure->code);
    void *env = closure->data;

    pz_trace_state(ip, rsp, esp, (uint64_t *)expr_stack);
    while (true) {
        PZ_Instruction_Token token = (PZ_Instruction_Token)(*ip);

        ip++;
        switch (token) {
            case PZT_NOP:
                pz_trace_instr(rsp, "nop");
                break;
            case PZT_LOAD_IMMEDIATE_8:
                expr_stack[++esp].u8 = *ip;
                ip++;
                pz_trace_instr(rsp, "load imm:8");
                break;
            case PZT_LOAD_IMMEDIATE_16:
                ip = (uint8_t *)ALIGN_UP((uintptr_t)ip, 2);
                expr_stack[++esp].u16 = *(uint16_t *)ip;
                ip += 2;
                pz_trace_instr(rsp, "load imm:16");
                break;
            case PZT_LOAD_IMMEDIATE_32:
                ip = (uint8_t *)ALIGN_UP((uintptr_t)ip, 4);
                expr_stack[++esp].u32 = *(uint32_t *)ip;
                ip += 4;
                pz_trace_instr(rsp, "load imm:32");
                break;
            case PZT_LOAD_IMMEDIATE_64:
                ip = (uint8_t *)ALIGN_UP((uintptr_t)ip, 8);
                expr_stack[++esp].u64 = *(uint64_t *)ip;
                ip += 8;
                pz_trace_instr(rsp, "load imm:64");
                break;
            case PZT_ZE_8_16:
                expr_stack[esp].u16 = expr_stack[esp].u8;
                pz_trace_instr(rsp, "ze:8:16");
                break;
            case PZT_ZE_8_32:
                expr_stack[esp].u32 = expr_stack[esp].u8;
                pz_trace_instr(rsp, "ze:8:32");
                break;
            case PZT_ZE_8_64:
                expr_stack[esp].u64 = expr_stack[esp].u8;
                pz_trace_instr(rsp, "ze:8:64");
                break;
            case PZT_ZE_16_32:
                expr_stack[esp].u32 = expr_stack[esp].u16;
                pz_trace_instr(rsp, "ze:16:32");
                break;
            case PZT_ZE_16_64:
                expr_stack[esp].u64 = expr_stack[esp].u16;
                pz_trace_instr(rsp, "ze:16:64");
                break;
            case PZT_ZE_32_64:
                expr_stack[esp].u64 = expr_stack[esp].u32;
                pz_trace_instr(rsp, "ze:32:64");
                break;
            case PZT_SE_8_16:
                expr_stack[esp].s16 = expr_stack[esp].s8;
                pz_trace_instr(rsp, "se:8:16");
                break;
            case PZT_SE_8_32:
                expr_stack[esp].s32 = expr_stack[esp].s8;
                pz_trace_instr(rsp, "se:8:32");
                break;
            case PZT_SE_8_64:
                expr_stack[esp].s64 = expr_stack[esp].s8;
                pz_trace_instr(rsp, "se:8:64");
                break;
            case PZT_SE_16_32:
                expr_stack[esp].s32 = expr_stack[esp].s16;
                pz_trace_instr(rsp, "se:16:32");
                break;
            case PZT_SE_16_64:
                expr_stack[esp].s64 = expr_stack[esp].s16;
                pz_trace_instr(rsp, "se:16:64");
                break;
            case PZT_SE_32_64:
                expr_stack[esp].s64 = expr_stack[esp].s32;
                pz_trace_instr(rsp, "se:32:64");
                break;
            case PZT_TRUNC_64_32:
                expr_stack[esp].u32 = expr_stack[esp].u64 & 0xFFFFFFFFu;
                pz_trace_instr(rsp, "trunc:64:32");
                break;
            case PZT_TRUNC_64_16:
                expr_stack[esp].u16 = expr_stack[esp].u64 & 0xFFFF;
                pz_trace_instr(rsp, "trunc:64:16");
                break;
            case PZT_TRUNC_64_8:
                expr_stack[esp].u8 = expr_stack[esp].u64 & 0xFF;
                pz_trace_instr(rsp, "trunc:64:8");
                break;
            case PZT_TRUNC_32_16:
                expr_stack[esp].u16 = expr_stack[esp].u32 & 0xFFFF;
                pz_trace_instr(rsp, "trunc:32:16");
                break;
            case PZT_TRUNC_32_8:
                expr_stack[esp].u8 = expr_stack[esp].u32 & 0xFF;
                pz_trace_instr(rsp, "trunc:32:8");
                break;
            case PZT_TRUNC_16_8:
                expr_stack[esp].u8 = expr_stack[esp].u16 & 0xFF;
                pz_trace_instr(rsp, "trunc:16:8");
                break;

#define PZ_RUN_ARITHMETIC(opcode_base, width, signedness, operator,         \
                          op_name)                                          \
    case opcode_base##_##width:                                             \
        expr_stack[esp - 1].signedness##width = (expr_stack[esp - 1]        \
                                                   .signedness##width       \
                                                   operator expr_stack[esp] \
                                                   .signedness##width);     \
        esp--;                                                              \
        pz_trace_instr(rsp, op_name);                                       \
        break
#define PZ_RUN_ARITHMETIC1(opcode_base, width, signedness, operator, \
                           op_name)                                  \
    case opcode_base##_##width:                                      \
        expr_stack[esp].signedness##width = operator expr_stack[esp] \
                                              .signedness##width;    \
        pz_trace_instr(rsp, op_name);                                \
        break

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

#define PZ_RUN_SHIFT(opcode_base, width, operator, op_name)           \
    case opcode_base##_##width:                                       \
        expr_stack[esp - 1].u##width =                                \
          (expr_stack[esp - 1].u##width operator expr_stack[esp].u8); \
        esp--;                                                        \
        pz_trace_instr(rsp, op_name);                                 \
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
                esp++;
                expr_stack[esp] = expr_stack[esp - 1];
                pz_trace_instr(rsp, "dup");
                break;
            case PZT_DROP:
                esp--;
                pz_trace_instr(rsp, "drop");
                break;
            case PZT_SWAP: {
                Stack_Value temp;
                temp = expr_stack[esp];
                expr_stack[esp] = expr_stack[esp - 1];
                expr_stack[esp - 1] = temp;
                pz_trace_instr(rsp, "swap");
                break;
            }
            case PZT_ROLL: {
                uint8_t     depth = *ip;
                Stack_Value temp;
                ip++;
                switch (depth) {
                    case 0:
                        fprintf(stderr, "Illegal rot depth 0");
                        abort();
                    case 1:
                        break;
                    default:
                        /*
                         * subtract 1 as the 1st element on the stack is
                         * esp - 0, not esp - 1
                         */
                        depth--;
                        temp = expr_stack[esp - depth];
                        for (int i = depth; i > 0; i--) {
                            expr_stack[esp - i] = expr_stack[esp - (i - 1)];
                        }
                        expr_stack[esp] = temp;
                }
                pz_trace_instr2(rsp, "roll", depth + 1);
                break;
            }
            case PZT_PICK: {
                /*
                 * As with PZT_ROLL we would subract 1 here, but we also
                 * have to add 1 because we increment the stack pointer
                 * before accessing the stack.
                 */
                uint8_t depth = *ip;
                ip++;
                esp++;
                expr_stack[esp] = expr_stack[esp - depth];
                pz_trace_instr2(rsp, "pick", depth);
                break;
            }
            case PZT_CALL:
                ip = (uint8_t *)ALIGN_UP((uintptr_t)ip, MACHINE_WORD_SIZE);
                return_stack[++rsp] = static_cast<uint8_t*>(env);
                return_stack[++rsp] = (ip + MACHINE_WORD_SIZE);
                ip = *(uint8_t **)ip;
                pz_trace_instr(rsp, "call");
                break;
            case PZT_TCALL:
                ip = (uint8_t *)ALIGN_UP((uintptr_t)ip, MACHINE_WORD_SIZE);
                ip = *(uint8_t **)ip;
                pz_trace_instr(rsp, "tcall");
                break;
            case PZT_CALL_CLOSURE: {
                PZ_Closure *closure;

                ip = (uint8_t *)ALIGN_UP((uintptr_t)ip, MACHINE_WORD_SIZE);
                return_stack[++rsp] = static_cast<uint8_t*>(env);
                return_stack[++rsp] = (ip + MACHINE_WORD_SIZE);
                closure = *(PZ_Closure **)ip;
                ip = static_cast<uint8_t*>(closure->code);
                env = closure->data;

                pz_trace_instr(rsp, "call_closure");
                break;
            }
            case PZT_CALL_IND: {
                PZ_Closure *closure;

                return_stack[++rsp] = static_cast<uint8_t*>(env);
                return_stack[++rsp] = ip;

                closure = (PZ_Closure *)expr_stack[esp--].ptr;
                ip = static_cast<uint8_t*>(closure->code);
                env = closure->data;

                pz_trace_instr(rsp, "call_ind");
                break;
            }
            case PZT_CJMP_8:
                ip = (uint8_t *)ALIGN_UP((uintptr_t)ip, MACHINE_WORD_SIZE);
                if (expr_stack[esp--].u8) {
                    ip = *(uint8_t **)ip;
                    pz_trace_instr(rsp, "cjmp:8 taken");
                } else {
                    ip += MACHINE_WORD_SIZE;
                    pz_trace_instr(rsp, "cjmp:8 not taken");
                }
                break;
            case PZT_CJMP_16:
                ip = (uint8_t *)ALIGN_UP((uintptr_t)ip, MACHINE_WORD_SIZE);
                if (expr_stack[esp--].u16) {
                    ip = *(uint8_t **)ip;
                    pz_trace_instr(rsp, "cjmp:16 taken");
                } else {
                    ip += MACHINE_WORD_SIZE;
                    pz_trace_instr(rsp, "cjmp:16 not taken");
                }
                break;
            case PZT_CJMP_32:
                ip = (uint8_t *)ALIGN_UP((uintptr_t)ip, MACHINE_WORD_SIZE);
                if (expr_stack[esp--].u32) {
                    ip = *(uint8_t **)ip;
                    pz_trace_instr(rsp, "cjmp:32 taken");
                } else {
                    ip += MACHINE_WORD_SIZE;
                    pz_trace_instr(rsp, "cjmp:32 not taken");
                }
                break;
            case PZT_CJMP_64:
                ip = (uint8_t *)ALIGN_UP((uintptr_t)ip, MACHINE_WORD_SIZE);
                if (expr_stack[esp--].u64) {
                    ip = *(uint8_t **)ip;
                    pz_trace_instr(rsp, "cjmp:64 taken");
                } else {
                    ip += MACHINE_WORD_SIZE;
                    pz_trace_instr(rsp, "cjmp:64 not taken");
                }
                break;
            case PZT_JMP:
                ip = (uint8_t *)ALIGN_UP((uintptr_t)ip, MACHINE_WORD_SIZE);
                ip = *(uint8_t **)ip;
                pz_trace_instr(rsp, "jmp");
                break;
            case PZT_RET:
                ip = return_stack[rsp--];
                env = return_stack[rsp--];
                pz_trace_instr(rsp, "ret");
                break;
            case PZT_ALLOC: {
                uintptr_t size;
                void     *addr;
                ip = (uint8_t *)ALIGN_UP((uintptr_t)ip, MACHINE_WORD_SIZE);
                size = *(uintptr_t *)ip;
                ip += MACHINE_WORD_SIZE;
                // pz_gc_alloc uses size in machine words, round the value
                // up and convert it to words rather than bytes.
                addr = heap->alloc(
                            (size+MACHINE_WORD_SIZE-1) / MACHINE_WORD_SIZE,
                            &expr_stack[esp+1]);
                expr_stack[++esp].ptr = addr;
                pz_trace_instr(rsp, "alloc");
                break;
            }
            case PZT_MAKE_CLOSURE: {
                void       *code, *data;

                ip = (uint8_t *)ALIGN_UP((uintptr_t)ip, MACHINE_WORD_SIZE);
                code = *(void**)ip;
                ip = (ip + MACHINE_WORD_SIZE);
                data = expr_stack[esp].ptr;
                expr_stack[esp].ptr = pz_init_closure(
                    static_cast<uint8_t*>(code), data);
                pz_trace_instr(rsp, "make_closure");
                break;
            }
            case PZT_LOAD_8: {
                uint16_t offset;
                void *   addr;
                ip = (uint8_t *)ALIGN_UP((uintptr_t)ip, 2);
                offset = *(uint16_t *)ip;
                ip += 2;
                /* (ptr - * ptr) */
                addr = expr_stack[esp].ptr + offset;
                expr_stack[esp + 1].ptr = expr_stack[esp].ptr;
                expr_stack[esp].u8 = *(uint8_t *)addr;
                esp++;
                pz_trace_instr(rsp, "load_8");
                break;
            }
            case PZT_LOAD_16: {
                uint16_t offset;
                void *   addr;
                ip = (uint8_t *)ALIGN_UP((uintptr_t)ip, 2);
                offset = *(uint16_t *)ip;
                ip += 2;
                /* (ptr - * ptr) */
                addr = expr_stack[esp].ptr + offset;
                expr_stack[esp + 1].ptr = expr_stack[esp].ptr;
                expr_stack[esp].u16 = *(uint16_t *)addr;
                esp++;
                pz_trace_instr(rsp, "load_16");
                break;
            }
            case PZT_LOAD_32: {
                uint16_t offset;
                void *   addr;
                ip = (uint8_t *)ALIGN_UP((uintptr_t)ip, 2);
                offset = *(uint16_t *)ip;
                ip += 2;
                /* (ptr - * ptr) */
                addr = expr_stack[esp].ptr + offset;
                expr_stack[esp + 1].ptr = expr_stack[esp].ptr;
                expr_stack[esp].u32 = *(uint32_t *)addr;
                esp++;
                pz_trace_instr(rsp, "load_32");
                break;
            }
            case PZT_LOAD_64: {
                uint16_t offset;
                void *   addr;
                ip = (uint8_t *)ALIGN_UP((uintptr_t)ip, 2);
                offset = *(uint16_t *)ip;
                ip += 2;
                /* (ptr - * ptr) */
                addr = expr_stack[esp].ptr + offset;
                expr_stack[esp + 1].ptr = expr_stack[esp].ptr;
                expr_stack[esp].u64 = *(uint64_t *)addr;
                esp++;
                pz_trace_instr(rsp, "load_64");
                break;
            }
            case PZT_LOAD_PTR: {
                uint16_t offset;
                void *   addr;
                ip = (uint8_t *)ALIGN_UP((uintptr_t)ip, 2);
                offset = *(uint16_t *)ip;
                ip += 2;
                /* (ptr - ptr ptr) */
                addr = expr_stack[esp].ptr + offset;
                expr_stack[esp + 1].ptr = expr_stack[esp].ptr;
                expr_stack[esp].ptr = *(void **)addr;
                esp++;
                pz_trace_instr(rsp, "load_ptr");
                break;
            }
            case PZT_STORE_8: {
                uint16_t offset;
                void *   addr;
                ip = (uint8_t *)ALIGN_UP((uintptr_t)ip, 2);
                offset = *(uint16_t *)ip;
                ip += 2;
                /* (* ptr - ptr) */
                addr = expr_stack[esp].ptr + offset;
                *(uint8_t *)addr = expr_stack[esp - 1].u8;
                expr_stack[esp - 1].ptr = expr_stack[esp].ptr;
                esp--;
                pz_trace_instr(rsp, "store_8");
                break;
            }
            case PZT_STORE_16: {
                uint16_t offset;
                void *   addr;
                ip = (uint8_t *)ALIGN_UP((uintptr_t)ip, 2);
                offset = *(uint16_t *)ip;
                ip += 2;
                /* (* ptr - ptr) */
                addr = expr_stack[esp].ptr + offset;
                *(uint16_t *)addr = expr_stack[esp - 1].u16;
                expr_stack[esp - 1].ptr = expr_stack[esp].ptr;
                esp--;
                pz_trace_instr(rsp, "store_16");
                break;
            }
            case PZT_STORE_32: {
                uint16_t offset;
                void *   addr;
                ip = (uint8_t *)ALIGN_UP((uintptr_t)ip, 2);
                offset = *(uint16_t *)ip;
                ip += 2;
                /* (* ptr - ptr) */
                addr = expr_stack[esp].ptr + offset;
                *(uint32_t *)addr = expr_stack[esp - 1].u32;
                expr_stack[esp - 1].ptr = expr_stack[esp].ptr;
                esp--;
                pz_trace_instr(rsp, "store_32");
                break;
            }
            case PZT_STORE_64: {
                uint16_t offset;
                void *   addr;
                ip = (uint8_t *)ALIGN_UP((uintptr_t)ip, 2);
                offset = *(uint16_t *)ip;
                ip += 2;
                /* (* ptr - ptr) */
                addr = expr_stack[esp].ptr + offset;
                *(uint64_t *)addr = expr_stack[esp - 1].u64;
                expr_stack[esp - 1].ptr = expr_stack[esp].ptr;
                esp--;
                pz_trace_instr(rsp, "store_64");
                break;
            }
            case PZT_GET_ENV: {
                expr_stack[++esp].ptr = env;
                pz_trace_instr(rsp, "get_env");
                break;
            }

            case PZT_END:
                retcode = expr_stack[esp].s32;
                if (esp != 1) {
                    fprintf(stderr, "Stack misaligned, esp: %d should be 1\n",
                            esp);
                    abort();
                }
                pz_trace_instr(rsp, "end");
                pz_trace_state(ip, rsp, esp, (uint64_t *)expr_stack);
                return retcode;
            case PZT_CCALL: {
                ccall_func callee;
                ip = (uint8_t *)ALIGN_UP((uintptr_t)ip, MACHINE_WORD_SIZE);
                callee = *(ccall_func *)ip;
                esp = callee(expr_stack, esp, heap);
                ip += MACHINE_WORD_SIZE;
                pz_trace_instr(rsp, "ccall");
                break;
            }
            default:
                fprintf(stderr, "Unknown opcode\n");
                abort();
        }
        pz_trace_state(ip, rsp, esp, (uint64_t *)expr_stack);
    }
}

