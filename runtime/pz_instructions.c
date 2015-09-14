/*
 * Plasma bytecode instructions
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2015 Paul Bone
 * Distributed under the terms of the MIT license, see ../LICENSE.runtime
 */

#include <stdlib.h>

#include "pz_common.h"
#include "pz_instructions.h"

/*
 * Instruction encoding
 *
 *************************/

enum immediate_type
pz_immediate(opcode opcode)
{
    switch (opcode) {
        case PZI_LOAD_IMMEDIATE_8:
            return IMT_8;
        case PZI_LOAD_IMMEDIATE_16:
            return IMT_16;
        case PZI_LOAD_IMMEDIATE_32:
            return IMT_32;
        case PZI_LOAD_IMMEDIATE_64:
            return IMT_64;
        case PZI_LOAD_IMMEDIATE_DATA:
            return IMT_DATA_REF;
        case PZI_CALL:
            return IMT_CODE_REF;
        case PZI_CCALL:
            /*
             * Not really a code reference, but this type has the correct
             * size
             */
            return IMT_CODE_REF;
        case PZI_ZE_8_16:
        case PZI_ZE_16_32:
        case PZI_ZE_32_64:
        case PZI_SE_8_16:
        case PZI_SE_16_32:
        case PZI_SE_32_64:
        case PZI_TRUNC_64_32:
        case PZI_TRUNC_32_16:
        case PZI_TRUNC_16_8:
        case PZI_ZE_32_FAST:
        case PZI_SE_32_FAST:
        case PZI_TRUNC_FAST_32:
        case PZI_ADD:
        case PZI_SUB:
        case PZI_MUL:
        case PZI_DIV:
        case PZI_DUP:
        case PZI_SWAP:
        case PZI_RETURN:
        case PZI_END:
            return IMT_NONE;
    }
    abort();
}

