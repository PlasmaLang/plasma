/*
 * Plasma bytecode memory representation builder
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2015-2019 Plasma Team
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
write_opcode(uint8_t           *proc,
             unsigned           offset,
             InstructionToken   token);

static unsigned
write_immediate(uint8_t        *proc,
                unsigned        offset,
                ImmediateType   imm_type,
                ImmediateValue  imm_value);

/*
 * Instruction and intermedate data sizes, and procedures to write them.
 *
 *********************/

static unsigned
immediate_size(ImmediateType imt)
{
    switch (imt) {
        case IMT_NONE:
            return 0;
        case IMT_8:
            return 1;
        case IMT_16:
        case IMT_STRUCT_REF_FIELD:
        case IMT_IMPORT_REF:
            return 2;
        case IMT_32:
            return 4;
        case IMT_64:
            return 8;
        case IMT_CLOSURE_REF:
        case IMT_PROC_REF:
        case IMT_IMPORT_CLOSURE_REF:
        case IMT_STRUCT_REF:
        case IMT_LABEL_REF:
            return WORDSIZE_BYTES;
    }
    abort();
}

#define SELECT_IMMEDIATE(type, value, result)                       \
    switch (type) {                                                 \
        case IMT_8:                                              \
            (result) = (value).uint8;                            \
            break;                                                  \
        case IMT_16:                                             \
            (result) = (value).uint16;                           \
            break;                                                  \
        case IMT_32:                                             \
            (result) = (value).uint32;                           \
            break;                                                  \
        case IMT_64:                                             \
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
            PZ_Opcode          opcode)
{
#define PZ_WRITE_INSTR_0(code, tok)                                     \
    if (opcode == (code)) {                                             \
        return write_opcode(proc, offset, tok);                         \
    }

    assert(0 == instruction_info[opcode].ii_num_width_bytes);
    assert(IMT_NONE == instruction_info[opcode].ii_immediate_type);

    PZ_WRITE_INSTR_0(PZI_DROP, PZT_DROP);

    PZ_WRITE_INSTR_0(PZI_CALL_IND, PZT_CALL_IND);
    PZ_WRITE_INSTR_0(PZI_TCALL_IND, PZT_TCALL_IND);
    PZ_WRITE_INSTR_0(PZI_RET, PZT_RET);

    PZ_WRITE_INSTR_0(PZI_GET_ENV, PZT_GET_ENV);

    PZ_WRITE_INSTR_0(PZI_END, PZT_END);

#undef PZ_WRITE_INSTR_0

    fprintf(stderr, "Bad or unimplemented instruction\n");
    abort();
}

unsigned
write_instr(uint8_t       *proc,
            unsigned       offset,
            PZ_Opcode      opcode,
            ImmediateType  imm_type,
            ImmediateValue imm_value)
{
#define PZ_WRITE_INSTR_0(code, tok)                                     \
    if (opcode == (code)) {                                             \
        offset = write_opcode(proc, offset, tok);                       \
        offset = write_immediate(proc, offset, imm_type, imm_value);    \
        return offset;                                                  \
    }

    assert(0 == instruction_info[opcode].ii_num_width_bytes);
    assert(IMT_NONE != instruction_info[opcode].ii_immediate_type);

    if ((opcode == PZI_ROLL) && (imm_type == IMT_8) &&
            (imm_value.uint8 == 2))
    {
        /* Optimize roll 2 into swap */
        return write_opcode(proc, offset, PZT_SWAP);
    }
    PZ_WRITE_INSTR_0(PZI_ROLL, PZT_ROLL);

    if ((opcode == PZI_PICK) && (imm_type == IMT_8) &&
            (imm_value.uint8 == 1))
    {
        /* Optimize pick 1 into dup */
        return write_opcode(proc, offset, PZT_DUP);
    }
    PZ_WRITE_INSTR_0(PZI_PICK, PZT_PICK);

    PZ_WRITE_INSTR_0(PZI_CALL, PZT_CALL);
    PZ_WRITE_INSTR_0(PZI_CALL_IMPORT, PZT_CALL);

    PZ_WRITE_INSTR_0(PZI_CALL_PROC, PZT_CALL_PROC);

    PZ_WRITE_INSTR_0(PZI_TCALL, PZT_TCALL);

    PZ_WRITE_INSTR_0(PZI_TCALL_PROC, PZT_TCALL_PROC);

    PZ_WRITE_INSTR_0(PZI_JMP, PZT_JMP);

    PZ_WRITE_INSTR_0(PZI_ALLOC,        PZT_ALLOC);
    PZ_WRITE_INSTR_0(PZI_MAKE_CLOSURE, PZT_MAKE_CLOSURE);

    PZ_WRITE_INSTR_0(PZI_LOAD_NAMED,   PZT_LOAD_PTR);

    PZ_WRITE_INSTR_0(PZI_CCALL, PZT_CCALL);
    PZ_WRITE_INSTR_0(PZI_CCALL_ALLOC, PZT_CCALL_ALLOC);
    PZ_WRITE_INSTR_0(PZI_CCALL_SPECIAL, PZT_CCALL_SPECIAL);

#undef PZ_WRITE_INSTR_0

    fprintf(stderr, "Bad or unimplemented instruction\n");
    abort();
}

unsigned
write_instr(uint8_t *          proc,
            unsigned           offset,
            PZ_Opcode          opcode,
            PZ_Width           width1)
{
    width1 = width_normalize(width1);

#define PZ_WRITE_INSTR_1(code, w1, tok)                                 \
    if (opcode == (code) && width1 == (w1)) {                           \
        return write_opcode(proc, offset, tok);                         \
    }

    assert(1 == instruction_info[opcode].ii_num_width_bytes);
    assert(IMT_NONE == instruction_info[opcode].ii_immediate_type);

    PZ_WRITE_INSTR_1(PZI_ADD, PZW_8,  PZT_ADD_8);
    PZ_WRITE_INSTR_1(PZI_ADD, PZW_16, PZT_ADD_16);
    PZ_WRITE_INSTR_1(PZI_ADD, PZW_32, PZT_ADD_32);
    PZ_WRITE_INSTR_1(PZI_ADD, PZW_64, PZT_ADD_64);

    PZ_WRITE_INSTR_1(PZI_SUB, PZW_8,  PZT_SUB_8);
    PZ_WRITE_INSTR_1(PZI_SUB, PZW_16, PZT_SUB_16);
    PZ_WRITE_INSTR_1(PZI_SUB, PZW_32, PZT_SUB_32);
    PZ_WRITE_INSTR_1(PZI_SUB, PZW_64, PZT_SUB_64);

    PZ_WRITE_INSTR_1(PZI_MUL, PZW_8,  PZT_MUL_8);
    PZ_WRITE_INSTR_1(PZI_MUL, PZW_16, PZT_MUL_16);
    PZ_WRITE_INSTR_1(PZI_MUL, PZW_32, PZT_MUL_32);
    PZ_WRITE_INSTR_1(PZI_MUL, PZW_64, PZT_MUL_64);

    PZ_WRITE_INSTR_1(PZI_DIV, PZW_8,  PZT_DIV_8);
    PZ_WRITE_INSTR_1(PZI_DIV, PZW_16, PZT_DIV_16);
    PZ_WRITE_INSTR_1(PZI_DIV, PZW_32, PZT_DIV_32);
    PZ_WRITE_INSTR_1(PZI_DIV, PZW_64, PZT_DIV_64);

    PZ_WRITE_INSTR_1(PZI_MOD, PZW_8,  PZT_MOD_8);
    PZ_WRITE_INSTR_1(PZI_MOD, PZW_16, PZT_MOD_16);
    PZ_WRITE_INSTR_1(PZI_MOD, PZW_32, PZT_MOD_32);
    PZ_WRITE_INSTR_1(PZI_MOD, PZW_64, PZT_MOD_64);

    PZ_WRITE_INSTR_1(PZI_LSHIFT, PZW_8,  PZT_LSHIFT_8);
    PZ_WRITE_INSTR_1(PZI_LSHIFT, PZW_16, PZT_LSHIFT_16);
    PZ_WRITE_INSTR_1(PZI_LSHIFT, PZW_32, PZT_LSHIFT_32);
    PZ_WRITE_INSTR_1(PZI_LSHIFT, PZW_64, PZT_LSHIFT_64);

    PZ_WRITE_INSTR_1(PZI_RSHIFT, PZW_8,  PZT_RSHIFT_8);
    PZ_WRITE_INSTR_1(PZI_RSHIFT, PZW_16, PZT_RSHIFT_16);
    PZ_WRITE_INSTR_1(PZI_RSHIFT, PZW_32, PZT_RSHIFT_32);
    PZ_WRITE_INSTR_1(PZI_RSHIFT, PZW_64, PZT_RSHIFT_64);

    PZ_WRITE_INSTR_1(PZI_AND, PZW_8,  PZT_AND_8);
    PZ_WRITE_INSTR_1(PZI_AND, PZW_16, PZT_AND_16);
    PZ_WRITE_INSTR_1(PZI_AND, PZW_32, PZT_AND_32);
    PZ_WRITE_INSTR_1(PZI_AND, PZW_64, PZT_AND_64);

    PZ_WRITE_INSTR_1(PZI_OR, PZW_8,  PZT_OR_8);
    PZ_WRITE_INSTR_1(PZI_OR, PZW_16, PZT_OR_16);
    PZ_WRITE_INSTR_1(PZI_OR, PZW_32, PZT_OR_32);
    PZ_WRITE_INSTR_1(PZI_OR, PZW_64, PZT_OR_64);

    PZ_WRITE_INSTR_1(PZI_XOR, PZW_8,  PZT_XOR_8);
    PZ_WRITE_INSTR_1(PZI_XOR, PZW_16, PZT_XOR_16);
    PZ_WRITE_INSTR_1(PZI_XOR, PZW_32, PZT_XOR_32);
    PZ_WRITE_INSTR_1(PZI_XOR, PZW_64, PZT_XOR_64);

    PZ_WRITE_INSTR_1(PZI_LT_U, PZW_8,  PZT_LT_U_8);
    PZ_WRITE_INSTR_1(PZI_LT_U, PZW_16, PZT_LT_U_16);
    PZ_WRITE_INSTR_1(PZI_LT_U, PZW_32, PZT_LT_U_32);
    PZ_WRITE_INSTR_1(PZI_LT_U, PZW_64, PZT_LT_U_64);

    PZ_WRITE_INSTR_1(PZI_LT_S, PZW_8,  PZT_LT_S_8);
    PZ_WRITE_INSTR_1(PZI_LT_S, PZW_16, PZT_LT_S_16);
    PZ_WRITE_INSTR_1(PZI_LT_S, PZW_32, PZT_LT_S_32);
    PZ_WRITE_INSTR_1(PZI_LT_S, PZW_64, PZT_LT_S_64);

    PZ_WRITE_INSTR_1(PZI_GT_U, PZW_8,  PZT_GT_U_8);
    PZ_WRITE_INSTR_1(PZI_GT_U, PZW_16, PZT_GT_U_16);
    PZ_WRITE_INSTR_1(PZI_GT_U, PZW_32, PZT_GT_U_32);
    PZ_WRITE_INSTR_1(PZI_GT_U, PZW_64, PZT_GT_U_64);

    PZ_WRITE_INSTR_1(PZI_GT_S, PZW_8,  PZT_GT_S_8);
    PZ_WRITE_INSTR_1(PZI_GT_S, PZW_16, PZT_GT_S_16);
    PZ_WRITE_INSTR_1(PZI_GT_S, PZW_32, PZT_GT_S_32);
    PZ_WRITE_INSTR_1(PZI_GT_S, PZW_64, PZT_GT_S_64);

    PZ_WRITE_INSTR_1(PZI_EQ, PZW_8,  PZT_EQ_8);
    PZ_WRITE_INSTR_1(PZI_EQ, PZW_16, PZT_EQ_16);
    PZ_WRITE_INSTR_1(PZI_EQ, PZW_32, PZT_EQ_32);
    PZ_WRITE_INSTR_1(PZI_EQ, PZW_64, PZT_EQ_64);

    PZ_WRITE_INSTR_1(PZI_NOT, PZW_8,  PZT_NOT_8);
    PZ_WRITE_INSTR_1(PZI_NOT, PZW_16, PZT_NOT_16);
    PZ_WRITE_INSTR_1(PZI_NOT, PZW_32, PZT_NOT_32);
    PZ_WRITE_INSTR_1(PZI_NOT, PZW_64, PZT_NOT_64);

#undef PZ_WRITE_INSTR_1

    fprintf(stderr, "Bad or unimplemented instruction\n");
    abort();
}

unsigned
write_instr(uint8_t       *proc,
            unsigned       offset,
            PZ_Opcode      opcode,
            PZ_Width       width1,
            ImmediateType  imm_type,
            ImmediateValue imm_value)
{
    width1 = width_normalize(width1);

#define PZ_WRITE_INSTR_1(code, w1, tok)                                 \
    if (opcode == (code) && width1 == (w1)) {                           \
        offset = write_opcode(proc, offset, tok);                       \
        offset = write_immediate(proc, offset, imm_type, imm_value);    \
        return offset;                                                  \
    }

    assert(1 == instruction_info[opcode].ii_num_width_bytes);
    assert(IMT_NONE != instruction_info[opcode].ii_immediate_type);

    if (opcode == PZI_LOAD_IMMEDIATE_NUM) {
        switch (width1) {
            case PZW_8:
                SELECT_IMMEDIATE(imm_type, imm_value, imm_value.uint8);
                offset = write_opcode(proc, offset, PZT_LOAD_IMMEDIATE_8);
                offset = write_immediate(proc, offset, IMT_8, imm_value);
                return offset;
            case PZW_16:
                SELECT_IMMEDIATE(imm_type, imm_value, imm_value.uint16);
                offset = write_opcode(proc, offset, PZT_LOAD_IMMEDIATE_16);
                offset = write_immediate(proc, offset, IMT_16, imm_value);
                return offset;
            case PZW_32:
                SELECT_IMMEDIATE(imm_type, imm_value, imm_value.uint32);
                offset = write_opcode(proc, offset, PZT_LOAD_IMMEDIATE_32);
                offset = write_immediate(proc, offset, IMT_32, imm_value);
                return offset;
            case PZW_64:
                SELECT_IMMEDIATE(imm_type, imm_value, imm_value.uint64);
                offset = write_opcode(proc, offset, PZT_LOAD_IMMEDIATE_64);
                offset = write_immediate(proc, offset, IMT_64, imm_value);
                return offset;
            default:
                goto error;
        }
    }

    PZ_WRITE_INSTR_1(PZI_CJMP, PZW_8,  PZT_CJMP_8);
    PZ_WRITE_INSTR_1(PZI_CJMP, PZW_16, PZT_CJMP_16);
    PZ_WRITE_INSTR_1(PZI_CJMP, PZW_32, PZT_CJMP_32);
    PZ_WRITE_INSTR_1(PZI_CJMP, PZW_64, PZT_CJMP_64);

    PZ_WRITE_INSTR_1(PZI_LOAD,  PZW_8,  PZT_LOAD_8);
    PZ_WRITE_INSTR_1(PZI_LOAD,  PZW_16, PZT_LOAD_16);
    PZ_WRITE_INSTR_1(PZI_LOAD,  PZW_32, PZT_LOAD_32);
    PZ_WRITE_INSTR_1(PZI_LOAD,  PZW_64, PZT_LOAD_64);
    PZ_WRITE_INSTR_1(PZI_STORE, PZW_8,  PZT_STORE_8);
    PZ_WRITE_INSTR_1(PZI_STORE, PZW_16, PZT_STORE_16);
    PZ_WRITE_INSTR_1(PZI_STORE, PZW_32, PZT_STORE_32);
    PZ_WRITE_INSTR_1(PZI_STORE, PZW_64, PZT_STORE_64);

#undef PZ_WRITE_INSTR_1

error:
    fprintf(stderr, "Bad or unimplemented instruction\n");
    abort();
}

unsigned
write_instr(uint8_t *          proc,
            unsigned           offset,
            PZ_Opcode          opcode,
            PZ_Width           width1,
            PZ_Width           width2)
{
    InstructionToken token;

    width1 = width_normalize(width1);
    width2 = width_normalize(width2);

#define PZ_WRITE_INSTR_2(code, w1, w2, tok)                     \
    if (opcode == (code) && width1 == (w1) && width2 == (w2)) { \
        token = (tok);                                          \
        return write_opcode(proc, offset, token);               \
    }

    assert(2 == instruction_info[opcode].ii_num_width_bytes);
    assert(IMT_NONE == instruction_info[opcode].ii_immediate_type);

    PZ_WRITE_INSTR_2(PZI_ZE, PZW_8,  PZW_8,  PZT_NOP);
    PZ_WRITE_INSTR_2(PZI_ZE, PZW_8,  PZW_16, PZT_ZE_8_16);
    PZ_WRITE_INSTR_2(PZI_ZE, PZW_8,  PZW_32, PZT_ZE_8_32);
    PZ_WRITE_INSTR_2(PZI_ZE, PZW_8,  PZW_64, PZT_ZE_8_64);
    PZ_WRITE_INSTR_2(PZI_ZE, PZW_16, PZW_16, PZT_NOP);
    PZ_WRITE_INSTR_2(PZI_ZE, PZW_16, PZW_32, PZT_ZE_16_32);
    PZ_WRITE_INSTR_2(PZI_ZE, PZW_16, PZW_64, PZT_ZE_16_64);
    PZ_WRITE_INSTR_2(PZI_ZE, PZW_32, PZW_32, PZT_NOP);
    PZ_WRITE_INSTR_2(PZI_ZE, PZW_32, PZW_64, PZT_ZE_32_64);

    PZ_WRITE_INSTR_2(PZI_SE, PZW_8,  PZW_8,  PZT_NOP);
    PZ_WRITE_INSTR_2(PZI_SE, PZW_8,  PZW_16, PZT_SE_8_16);
    PZ_WRITE_INSTR_2(PZI_SE, PZW_8,  PZW_32, PZT_SE_8_32);
    PZ_WRITE_INSTR_2(PZI_SE, PZW_8,  PZW_64, PZT_SE_8_64);
    PZ_WRITE_INSTR_2(PZI_SE, PZW_16, PZW_16, PZT_NOP);
    PZ_WRITE_INSTR_2(PZI_SE, PZW_16, PZW_32, PZT_SE_16_32);
    PZ_WRITE_INSTR_2(PZI_SE, PZW_16, PZW_64, PZT_SE_16_64);
    PZ_WRITE_INSTR_2(PZI_SE, PZW_32, PZW_32, PZT_NOP);
    PZ_WRITE_INSTR_2(PZI_SE, PZW_32, PZW_64, PZT_SE_32_64);

    PZ_WRITE_INSTR_2(PZI_TRUNC, PZW_8,  PZW_8,  PZT_NOP);
    PZ_WRITE_INSTR_2(PZI_TRUNC, PZW_16, PZW_16, PZT_NOP);
    PZ_WRITE_INSTR_2(PZI_TRUNC, PZW_16, PZW_8,  PZT_TRUNC_16_8);
    PZ_WRITE_INSTR_2(PZI_TRUNC, PZW_32, PZW_32, PZT_NOP);
    PZ_WRITE_INSTR_2(PZI_TRUNC, PZW_32, PZW_16, PZT_TRUNC_32_16);
    PZ_WRITE_INSTR_2(PZI_TRUNC, PZW_32, PZW_8,  PZT_TRUNC_32_8);
    PZ_WRITE_INSTR_2(PZI_TRUNC, PZW_64, PZW_64, PZT_NOP);
    PZ_WRITE_INSTR_2(PZI_TRUNC, PZW_64, PZW_32, PZT_TRUNC_64_32);
    PZ_WRITE_INSTR_2(PZI_TRUNC, PZW_64, PZW_16, PZT_TRUNC_64_16);
    PZ_WRITE_INSTR_2(PZI_TRUNC, PZW_64, PZW_8,  PZT_TRUNC_64_8);

#undef PZ_WRITE_INSTR_2

    fprintf(stderr, "Bad or unimplemented instruction\n");
    abort();
}

static unsigned
write_opcode(uint8_t           *proc,
             unsigned           offset,
             InstructionToken   token)
{
    if (proc != nullptr) {
        *((uint8_t *)(&proc[offset])) = token;
    }
    offset += 1;
    return offset;
}

static unsigned
write_immediate(uint8_t        *proc,
                unsigned        offset,
                ImmediateType   imm_type,
                ImmediateValue  imm_value)
{
    assert(imm_type != IMT_NONE);

    unsigned imm_size = immediate_size(imm_type);
    offset = AlignUp(offset, imm_size);

    if (proc != nullptr) {
        switch (imm_type) {
            case IMT_NONE:
                break;
            case IMT_8:
                *((uint8_t *)(&proc[offset])) = imm_value.uint8;
                break;
            case IMT_16:
            case IMT_STRUCT_REF_FIELD:
            case IMT_IMPORT_REF:
                *((uint16_t *)(&proc[offset])) = imm_value.uint16;
                break;
            case IMT_32:
                *((uint32_t *)(&proc[offset])) = imm_value.uint32;
                break;
            case IMT_64:
                *((uint64_t *)(&proc[offset])) = imm_value.uint64;
                break;
            case IMT_CLOSURE_REF:
            case IMT_PROC_REF:
            case IMT_IMPORT_CLOSURE_REF:
            case IMT_STRUCT_REF:
            case IMT_LABEL_REF:
                *((uintptr_t *)(&proc[offset])) = imm_value.word;
                break;
        }
    }

    offset += imm_size;

    return offset;
}

} // namespace pz

