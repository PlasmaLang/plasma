/*
 * Plasma bytecode instructions
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2015-2017 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 */

#include "pz_common.h"
#include "pz_instructions.h"

/*
 * Instruction encoding
 *
 *************************/

Instruction_Info instruction_info_data[] = {
    /* PZI_LOAD_IMMEDIATE_NUM
     * XXX: The immediate value is always encoded as a 32 bit number but
     * this restriction should be lifted.
     */
    { 1, IMT_32 },
    /* PZI_LOAD_IMMEDIATE_DATA */
    { 1, IMT_DATA_REF },
    /* PZI_ZE */
    { 2, IMT_NONE },
    /* PZI_SE */
    { 2, IMT_NONE },
    /* PZI_TRUNC */
    { 2, IMT_NONE },
    /* PZI_ADD */
    { 1, IMT_NONE },
    /* PZI_SUB */
    { 1, IMT_NONE },
    /* PZI_MUL */
    { 1, IMT_NONE },
    /* PZI_DIV */
    { 1, IMT_NONE },
    /* PZI_MOD */
    { 1, IMT_NONE },
    /* PZI_LSHIFT */
    { 1, IMT_NONE },
    /* PZI_RSHIFT */
    { 1, IMT_NONE },
    /* PZI_AND */
    { 1, IMT_NONE },
    /* PZI_OR */
    { 1, IMT_NONE },
    /* PZI_XOR */
    { 1, IMT_NONE },
    /* PZI_LT_U, PZT_LT_S, PZT_GT_U and PZT_GT_S */
    { 1, IMT_NONE }, { 1, IMT_NONE }, { 1, IMT_NONE }, { 1, IMT_NONE },
    /* PZI_EQ */
    { 1, IMT_NONE },
    /* PZI_NOT */
    { 1, IMT_NONE },
    /* PZI_DROP */
    { 0, IMT_NONE },
    /* PZI_ROLL */
    { 0, IMT_8 },
    /* PZI_PICK */
    { 0, IMT_8 },
    /* PZI_CALL */
    { 0, IMT_CODE_REF },
    /* PZI_RET */
    { 0, IMT_NONE },
    /* PZI_CJMP */
    { 1, IMT_LABEL_REF },
    /* PZI_JMP */
    { 0, IMT_LABEL_REF },

    /* PZI_ALLOC */
    { 0, IMT_STRUCT_REF },
    /* PZI_LOAD */
    { 1, IMT_STRUCT_REF_FIELD },
    /* PZI_STORE */
    { 1, IMT_STRUCT_REF_FIELD },

    /* Non-encoded instructions */
    /* PZI_END */
    { 0, IMT_NONE },
    /* PZI_CCALL */
    { 0, IMT_CODE_REF }
};

