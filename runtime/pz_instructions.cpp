/*
 * Plasma bytecode instructions
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2015-2019 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#include "pz_common.h"

#include "pz_instructions.h"

namespace pz {

/*
 * Instruction encoding
 *
 *************************/

InstructionInfo instruction_info[] = {
    /* PZI_LOAD_IMMEDIATE_NUM
     * XXX: The immediate value is always encoded as a 32 bit number but
     * this restriction should be lifted.
     */
    { 1, PZ_IMT_32 },
    /* PZI_ZE */
    { 2, PZ_IMT_NONE },
    /* PZI_SE */
    { 2, PZ_IMT_NONE },
    /* PZI_TRUNC */
    { 2, PZ_IMT_NONE },
    /* PZI_ADD */
    { 1, PZ_IMT_NONE },
    /* PZI_SUB */
    { 1, PZ_IMT_NONE },
    /* PZI_MUL */
    { 1, PZ_IMT_NONE },
    /* PZI_DIV */
    { 1, PZ_IMT_NONE },
    /* PZI_MOD */
    { 1, PZ_IMT_NONE },
    /* PZI_LSHIFT */
    { 1, PZ_IMT_NONE },
    /* PZI_RSHIFT */
    { 1, PZ_IMT_NONE },
    /* PZI_AND */
    { 1, PZ_IMT_NONE },
    /* PZI_OR */
    { 1, PZ_IMT_NONE },
    /* PZI_XOR */
    { 1, PZ_IMT_NONE },
    /* PZI_LT_U, PZT_LT_S, PZT_GT_U and PZT_GT_S */
    { 1, PZ_IMT_NONE },
    { 1, PZ_IMT_NONE },
    { 1, PZ_IMT_NONE },
    { 1, PZ_IMT_NONE },
    /* PZI_EQ */
    { 1, PZ_IMT_NONE },
    /* PZI_NOT */
    { 1, PZ_IMT_NONE },
    /* PZI_DROP */
    { 0, PZ_IMT_NONE },
    /* PZI_ROLL */
    { 0, PZ_IMT_8 },
    /* PZI_PICK */
    { 0, PZ_IMT_8 },
    /* PZI_CALL */
    { 0, PZ_IMT_CODE_REF },
    /* PZI_CALL_IMPORT */
    { 0, PZ_IMT_IMPORT_CLOSURE_REF },
    /* PZI_TCALL */
    { 0, PZ_IMT_CODE_REF },
    /* PZI_CALL_CLOSURE */
    { 0, PZ_IMT_CODE_REF },
    /* PZI_CALL_IND */
    { 0, PZ_IMT_NONE },
    /* PZI_RET */
    { 0, PZ_IMT_NONE },
    /* PZI_CJMP */
    { 1, PZ_IMT_LABEL_REF },
    /* PZI_JMP */
    { 0, PZ_IMT_LABEL_REF },

    /* PZI_ALLOC */
    { 0, PZ_IMT_STRUCT_REF },
    /* PZI_MAKE_CLOSURE */
    { 0, PZ_IMT_CODE_REF },
    /* PZI_LOAD */
    { 1, PZ_IMT_STRUCT_REF_FIELD },
    /* PZI_LOAD_NAMED */
    { 1, PZ_IMT_IMPORT_REF },
    /* PZI_STORE */
    { 1, PZ_IMT_STRUCT_REF_FIELD },
    /* PZI_GET_ENV */
    { 0, PZ_IMT_NONE },

    /* Non-encoded instructions */
    /* PZI_END */
    { 0, PZ_IMT_NONE },
    /* PZI_CCALL */
    { 0, PZ_IMT_CODE_REF },
    /* PZI_CCALL_ALLOC */
    { 0, PZ_IMT_CODE_REF }
};

} // namespace pz

