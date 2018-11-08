/*
 * Plasma bytecode memory representation builder
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2015-2018 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#include "pz_common.h"

#include <stdio.h>

#include "pz_data.h"
#include "pz_format.h"
#include "pz_gc.h"
#include "pz_instructions.h"
#include "pz_util.h"

#include "pz_generic_run.h"

namespace pz {

static unsigned
write_opcode(uint8_t              *proc,
             unsigned              offset,
             PZ_Instruction_Token  token,
             PZ_Immediate_Type     imm_type,
             PZ_Immediate_Value    imm_value);

/*
 * Instruction and intermedate data sizes, and procedures to write them.
 *
 *********************/

const unsigned fast_word_size = PZ_FAST_INTEGER_WIDTH / 8;

static unsigned
immediate_size(PZ_Immediate_Type imt)
{
    switch (imt) {
        case PZ_IMT_NONE:
            return 0;
        case PZ_IMT_8:
            return 1;
        case PZ_IMT_16:
        case PZ_IMT_STRUCT_REF_FIELD:
        case PZ_IMT_IMPORT_REF:
            return 2;
        case PZ_IMT_32:
            return 4;
        case PZ_IMT_64:
            return 8;
        case PZ_IMT_CODE_REF:
        case PZ_IMT_IMPORT_CLOSURE_REF:
        case PZ_IMT_STRUCT_REF:
        case PZ_IMT_LABEL_REF:
            return MACHINE_WORD_SIZE;
    }
    abort();
}

#define SELECT_IMMEDIATE(type, value, result)                       \
    switch (type) {                                                 \
        case PZ_IMT_8:                                              \
            (result) = (value).uint8;                            \
            break;                                                  \
        case PZ_IMT_16:                                             \
            (result) = (value).uint16;                           \
            break;                                                  \
        case PZ_IMT_32:                                             \
            (result) = (value).uint32;                           \
            break;                                                  \
        case PZ_IMT_64:                                             \
            (result) = (value).uint64;                           \
            break;                                                  \
        default:                                                    \
            fprintf(                                                \
              stderr,                                               \
              "Invalid immediate value for laod immediate number"); \
            abort();                                                \
    }

unsigned
write_instr(uint8_t *          proc,
            unsigned           offset,
            PZ_Opcode          opcode,
            PZ_Immediate_Type  imm_type,
            PZ_Immediate_Value imm_value)
{
#define PZ_WRITE_INSTR_0(code, tok) \
    if (opcode == (code)) {         \
        return write_opcode(proc, offset, tok, imm_type, imm_value); \
    }

    PZ_WRITE_INSTR_0(PZI_DROP, PZT_DROP);

    if ((opcode == PZI_ROLL) && (imm_type == PZ_IMT_8) &&
            (imm_value.uint8 == 2))
    {
        /* Optimize roll 2 into swap */
        return write_opcode(proc, offset, PZT_SWAP, PZ_IMT_NONE, imm_value);
    }
    PZ_WRITE_INSTR_0(PZI_ROLL, PZT_ROLL);

    if ((opcode == PZI_PICK) && (imm_type == PZ_IMT_8) &&
            (imm_value.uint8 == 1))
    {
        /* Optimize pick 1 into dup */
        return write_opcode(proc, offset, PZT_DUP, PZ_IMT_NONE, imm_value);
    }
    PZ_WRITE_INSTR_0(PZI_PICK, PZT_PICK);

    PZ_WRITE_INSTR_0(PZI_CALL, PZT_CALL);
    PZ_WRITE_INSTR_0(PZI_CALL_IMPORT, PZT_CALL_CLOSURE);
    PZ_WRITE_INSTR_0(PZI_TCALL, PZT_TCALL);
    PZ_WRITE_INSTR_0(PZI_CALL_IND, PZT_CALL_IND);

    if (opcode == PZI_CALL_CLOSURE) {
        unsigned imm_size = immediate_size(imm_type);

        if (proc != NULL) {
            *((uint8_t*)(&proc[offset])) = PZT_CALL_CLOSURE;
        }
        offset += 1;
        assert(imm_type == PZ_IMT_CODE_REF);
        offset = ALIGN_UP(offset, imm_size);
        if (proc != NULL) {
            *((uintptr_t *)(&proc[offset])) = imm_value.word;
        }
        offset += imm_size;
        return offset;
    }

    PZ_WRITE_INSTR_0(PZI_JMP, PZT_JMP);
    PZ_WRITE_INSTR_0(PZI_RET, PZT_RET);

    PZ_WRITE_INSTR_0(PZI_ALLOC,        PZT_ALLOC);
    PZ_WRITE_INSTR_0(PZI_MAKE_CLOSURE, PZT_MAKE_CLOSURE);

    PZ_WRITE_INSTR_0(PZI_LOAD_NAMED,   PZT_LOAD_PTR);

    PZ_WRITE_INSTR_0(PZI_GET_ENV, PZT_GET_ENV);

    PZ_WRITE_INSTR_0(PZI_END, PZT_END);
    PZ_WRITE_INSTR_0(PZI_CCALL, PZT_CCALL);

#undef PZ_WRITE_INSTR_0

    fprintf(stderr, "Bad or unimplemented instruction\n");
    abort();
}

unsigned
write_instr(uint8_t *          proc,
            unsigned           offset,
            PZ_Opcode          opcode,
            CheckedWidth       width1,
            PZ_Immediate_Type  imm_type,
            PZ_Immediate_Value imm_value)
{
    width1 = CheckedWidth(width1).normalize();

#define PZ_WRITE_INSTR_1(code, w1, tok)       \
    if (opcode == (code) && width1.w1()) { \
        return write_opcode(proc, offset, tok, imm_type, imm_value); \
    }

    if (opcode == PZI_LOAD_IMMEDIATE_NUM) {
        switch (width1.raw_width()) {
            case PZW_8:
                SELECT_IMMEDIATE(imm_type, imm_value, imm_value.uint8);
                return write_opcode(proc, offset, PZT_LOAD_IMMEDIATE_8,
                        PZ_IMT_8, imm_value);
            case PZW_16:
                SELECT_IMMEDIATE(imm_type, imm_value, imm_value.uint16);
                return write_opcode(proc, offset, PZT_LOAD_IMMEDIATE_16,
                        PZ_IMT_16, imm_value);
            case PZW_32:
                SELECT_IMMEDIATE(imm_type, imm_value, imm_value.uint32);
                return write_opcode(proc, offset, PZT_LOAD_IMMEDIATE_32,
                        PZ_IMT_32, imm_value);
            case PZW_64:
                SELECT_IMMEDIATE(imm_type, imm_value, imm_value.uint64);
                return write_opcode(proc, offset, PZT_LOAD_IMMEDIATE_64,
                        PZ_IMT_64, imm_value);
            default:
                goto error;
        }
    }

    PZ_WRITE_INSTR_1(PZI_ADD, is_8,  PZT_ADD_8);
    PZ_WRITE_INSTR_1(PZI_ADD, is_16, PZT_ADD_16);
    PZ_WRITE_INSTR_1(PZI_ADD, is_32, PZT_ADD_32);
    PZ_WRITE_INSTR_1(PZI_ADD, is_64, PZT_ADD_64);

    PZ_WRITE_INSTR_1(PZI_SUB, is_8,  PZT_SUB_8);
    PZ_WRITE_INSTR_1(PZI_SUB, is_16, PZT_SUB_16);
    PZ_WRITE_INSTR_1(PZI_SUB, is_32, PZT_SUB_32);
    PZ_WRITE_INSTR_1(PZI_SUB, is_64, PZT_SUB_64);

    PZ_WRITE_INSTR_1(PZI_MUL, is_8,  PZT_MUL_8);
    PZ_WRITE_INSTR_1(PZI_MUL, is_16, PZT_MUL_16);
    PZ_WRITE_INSTR_1(PZI_MUL, is_32, PZT_MUL_32);
    PZ_WRITE_INSTR_1(PZI_MUL, is_64, PZT_MUL_64);

    PZ_WRITE_INSTR_1(PZI_DIV, is_8,  PZT_DIV_8);
    PZ_WRITE_INSTR_1(PZI_DIV, is_16, PZT_DIV_16);
    PZ_WRITE_INSTR_1(PZI_DIV, is_32, PZT_DIV_32);
    PZ_WRITE_INSTR_1(PZI_DIV, is_64, PZT_DIV_64);

    PZ_WRITE_INSTR_1(PZI_MOD, is_8,  PZT_MOD_8);
    PZ_WRITE_INSTR_1(PZI_MOD, is_16, PZT_MOD_16);
    PZ_WRITE_INSTR_1(PZI_MOD, is_32, PZT_MOD_32);
    PZ_WRITE_INSTR_1(PZI_MOD, is_64, PZT_MOD_64);

    PZ_WRITE_INSTR_1(PZI_LSHIFT, is_8,  PZT_LSHIFT_8);
    PZ_WRITE_INSTR_1(PZI_LSHIFT, is_16, PZT_LSHIFT_16);
    PZ_WRITE_INSTR_1(PZI_LSHIFT, is_32, PZT_LSHIFT_32);
    PZ_WRITE_INSTR_1(PZI_LSHIFT, is_64, PZT_LSHIFT_64);

    PZ_WRITE_INSTR_1(PZI_RSHIFT, is_8,  PZT_RSHIFT_8);
    PZ_WRITE_INSTR_1(PZI_RSHIFT, is_16, PZT_RSHIFT_16);
    PZ_WRITE_INSTR_1(PZI_RSHIFT, is_32, PZT_RSHIFT_32);
    PZ_WRITE_INSTR_1(PZI_RSHIFT, is_64, PZT_RSHIFT_64);

    PZ_WRITE_INSTR_1(PZI_AND, is_8,  PZT_AND_8);
    PZ_WRITE_INSTR_1(PZI_AND, is_16, PZT_AND_16);
    PZ_WRITE_INSTR_1(PZI_AND, is_32, PZT_AND_32);
    PZ_WRITE_INSTR_1(PZI_AND, is_64, PZT_AND_64);

    PZ_WRITE_INSTR_1(PZI_OR, is_8,  PZT_OR_8);
    PZ_WRITE_INSTR_1(PZI_OR, is_16, PZT_OR_16);
    PZ_WRITE_INSTR_1(PZI_OR, is_32, PZT_OR_32);
    PZ_WRITE_INSTR_1(PZI_OR, is_64, PZT_OR_64);

    PZ_WRITE_INSTR_1(PZI_XOR, is_8,  PZT_XOR_8);
    PZ_WRITE_INSTR_1(PZI_XOR, is_16, PZT_XOR_16);
    PZ_WRITE_INSTR_1(PZI_XOR, is_32, PZT_XOR_32);
    PZ_WRITE_INSTR_1(PZI_XOR, is_64, PZT_XOR_64);

    PZ_WRITE_INSTR_1(PZI_LT_U, is_8,  PZT_LT_U_8);
    PZ_WRITE_INSTR_1(PZI_LT_U, is_16, PZT_LT_U_16);
    PZ_WRITE_INSTR_1(PZI_LT_U, is_32, PZT_LT_U_32);
    PZ_WRITE_INSTR_1(PZI_LT_U, is_64, PZT_LT_U_64);

    PZ_WRITE_INSTR_1(PZI_LT_S, is_8,  PZT_LT_S_8);
    PZ_WRITE_INSTR_1(PZI_LT_S, is_16, PZT_LT_S_16);
    PZ_WRITE_INSTR_1(PZI_LT_S, is_32, PZT_LT_S_32);
    PZ_WRITE_INSTR_1(PZI_LT_S, is_64, PZT_LT_S_64);

    PZ_WRITE_INSTR_1(PZI_GT_U, is_8,  PZT_GT_U_8);
    PZ_WRITE_INSTR_1(PZI_GT_U, is_16, PZT_GT_U_16);
    PZ_WRITE_INSTR_1(PZI_GT_U, is_32, PZT_GT_U_32);
    PZ_WRITE_INSTR_1(PZI_GT_U, is_64, PZT_GT_U_64);

    PZ_WRITE_INSTR_1(PZI_GT_S, is_8,  PZT_GT_S_8);
    PZ_WRITE_INSTR_1(PZI_GT_S, is_16, PZT_GT_S_16);
    PZ_WRITE_INSTR_1(PZI_GT_S, is_32, PZT_GT_S_32);
    PZ_WRITE_INSTR_1(PZI_GT_S, is_64, PZT_GT_S_64);

    PZ_WRITE_INSTR_1(PZI_EQ, is_8,  PZT_EQ_8);
    PZ_WRITE_INSTR_1(PZI_EQ, is_16, PZT_EQ_16);
    PZ_WRITE_INSTR_1(PZI_EQ, is_32, PZT_EQ_32);
    PZ_WRITE_INSTR_1(PZI_EQ, is_64, PZT_EQ_64);

    PZ_WRITE_INSTR_1(PZI_NOT, is_8,  PZT_NOT_8);
    PZ_WRITE_INSTR_1(PZI_NOT, is_16, PZT_NOT_16);
    PZ_WRITE_INSTR_1(PZI_NOT, is_32, PZT_NOT_32);
    PZ_WRITE_INSTR_1(PZI_NOT, is_64, PZT_NOT_64);

    PZ_WRITE_INSTR_1(PZI_CJMP, is_8,  PZT_CJMP_8);
    PZ_WRITE_INSTR_1(PZI_CJMP, is_16, PZT_CJMP_16);
    PZ_WRITE_INSTR_1(PZI_CJMP, is_32, PZT_CJMP_32);
    PZ_WRITE_INSTR_1(PZI_CJMP, is_64, PZT_CJMP_64);

    PZ_WRITE_INSTR_1(PZI_LOAD,  is_8,  PZT_LOAD_8);
    PZ_WRITE_INSTR_1(PZI_LOAD,  is_16, PZT_LOAD_16);
    PZ_WRITE_INSTR_1(PZI_LOAD,  is_32, PZT_LOAD_32);
    PZ_WRITE_INSTR_1(PZI_LOAD,  is_64, PZT_LOAD_64);
    PZ_WRITE_INSTR_1(PZI_STORE, is_8,  PZT_STORE_8);
    PZ_WRITE_INSTR_1(PZI_STORE, is_16, PZT_STORE_16);
    PZ_WRITE_INSTR_1(PZI_STORE, is_32, PZT_STORE_32);
    PZ_WRITE_INSTR_1(PZI_STORE, is_64, PZT_STORE_64);

#undef PZ_WRITE_INSTR_1

error:
    fprintf(stderr, "Bad or unimplemented instruction\n");
    abort();
}

unsigned
write_instr(uint8_t *          proc,
            unsigned           offset,
            PZ_Opcode          opcode,
            CheckedWidth       width1,
            CheckedWidth       width2,
            PZ_Immediate_Type  imm_type,
            PZ_Immediate_Value imm_value)
{
    PZ_Instruction_Token token;

    width1 = CheckedWidth(width1).normalize();
    width2 = CheckedWidth(width2).normalize();

#define PZ_WRITE_INSTR_2(code, w1, w2, tok)                     \
    if (opcode == (code) && width1.w1() && width2.w2()) { \
        token = (tok);                                          \
        return write_opcode(proc, offset, token, imm_type, imm_value);                                      \
    }

    PZ_WRITE_INSTR_2(PZI_ZE, is_8,  is_8,  PZT_NOP);
    PZ_WRITE_INSTR_2(PZI_ZE, is_8,  is_16, PZT_ZE_8_16);
    PZ_WRITE_INSTR_2(PZI_ZE, is_8,  is_32, PZT_ZE_8_32);
    PZ_WRITE_INSTR_2(PZI_ZE, is_8,  is_64, PZT_ZE_8_64);
    PZ_WRITE_INSTR_2(PZI_ZE, is_16, is_16, PZT_NOP);
    PZ_WRITE_INSTR_2(PZI_ZE, is_16, is_32, PZT_ZE_16_32);
    PZ_WRITE_INSTR_2(PZI_ZE, is_16, is_64, PZT_ZE_16_64);
    PZ_WRITE_INSTR_2(PZI_ZE, is_32, is_32, PZT_NOP);
    PZ_WRITE_INSTR_2(PZI_ZE, is_32, is_64, PZT_ZE_32_64);

    PZ_WRITE_INSTR_2(PZI_SE, is_8,  is_8,  PZT_NOP);
    PZ_WRITE_INSTR_2(PZI_SE, is_8,  is_16, PZT_SE_8_16);
    PZ_WRITE_INSTR_2(PZI_SE, is_8,  is_32, PZT_SE_8_32);
    PZ_WRITE_INSTR_2(PZI_SE, is_8,  is_64, PZT_SE_8_64);
    PZ_WRITE_INSTR_2(PZI_SE, is_16, is_16, PZT_NOP);
    PZ_WRITE_INSTR_2(PZI_SE, is_16, is_32, PZT_SE_16_32);
    PZ_WRITE_INSTR_2(PZI_SE, is_16, is_64, PZT_SE_16_64);
    PZ_WRITE_INSTR_2(PZI_SE, is_32, is_32, PZT_NOP);
    PZ_WRITE_INSTR_2(PZI_SE, is_32, is_64, PZT_SE_32_64);

    PZ_WRITE_INSTR_2(PZI_TRUNC, is_8,  is_8,  PZT_NOP);
    PZ_WRITE_INSTR_2(PZI_TRUNC, is_16, is_16, PZT_NOP);
    PZ_WRITE_INSTR_2(PZI_TRUNC, is_16, is_8,  PZT_TRUNC_16_8);
    PZ_WRITE_INSTR_2(PZI_TRUNC, is_32, is_32, PZT_NOP);
    PZ_WRITE_INSTR_2(PZI_TRUNC, is_32, is_16, PZT_TRUNC_32_16);
    PZ_WRITE_INSTR_2(PZI_TRUNC, is_32, is_8,  PZT_TRUNC_32_8);
    PZ_WRITE_INSTR_2(PZI_TRUNC, is_64, is_64, PZT_NOP);
    PZ_WRITE_INSTR_2(PZI_TRUNC, is_64, is_32, PZT_TRUNC_64_32);
    PZ_WRITE_INSTR_2(PZI_TRUNC, is_64, is_16, PZT_TRUNC_64_16);
    PZ_WRITE_INSTR_2(PZI_TRUNC, is_64, is_8,  PZT_TRUNC_64_8);

#undef PZ_WRITE_INSTR_2

    fprintf(stderr, "Bad or unimplemented instruction\n");
    abort();
}

static unsigned
write_opcode(uint8_t              *proc,
             unsigned              offset,
             PZ_Instruction_Token  token,
             PZ_Immediate_Type     imm_type,
             PZ_Immediate_Value    imm_value)
{
    if (proc != NULL) {
        *((uint8_t *)(&proc[offset])) = token;
    }
    offset += 1;

    if (imm_type != PZ_IMT_NONE) {
        unsigned imm_size = immediate_size(imm_type);
        offset = ALIGN_UP(offset, imm_size);

        if (proc != NULL) {
            switch (imm_type) {
                case PZ_IMT_NONE:
                    break;
                case PZ_IMT_8:
                    *((uint8_t *)(&proc[offset])) = imm_value.uint8;
                    break;
                case PZ_IMT_16:
                case PZ_IMT_STRUCT_REF_FIELD:
                case PZ_IMT_IMPORT_REF:
                    *((uint16_t *)(&proc[offset])) = imm_value.uint16;
                    break;
                case PZ_IMT_32:
                    *((uint32_t *)(&proc[offset])) = imm_value.uint32;
                    break;
                case PZ_IMT_64:
                    *((uint64_t *)(&proc[offset])) = imm_value.uint64;
                    break;
                case PZ_IMT_CODE_REF:
                case PZ_IMT_IMPORT_CLOSURE_REF:
                case PZ_IMT_STRUCT_REF:
                case PZ_IMT_LABEL_REF:
                    *((uintptr_t *)(&proc[offset])) = imm_value.word;
                    break;
            }
        }

        offset += imm_size;
    }

    return offset;
}

} // namespace pz

