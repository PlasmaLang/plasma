/*
 * Plasma bytecode format constants
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2015 Paul Bone
 * Distributed under the terms of the MIT license, see ../LICENSE.runtime
 *
 * This file is used by both the tools in runtime/ and src/
 */

#ifndef PZ_FORMAT_H
#define PZ_FORMAT_H

#include "pz_common.h"

/*
 * The PZ format is a binary format.  No padding is used and all numbers are
 * unsigned integers in big-endian format unless otherwise specified.
 * Strings are ANSI strings without a null terminated byte.  Their length is
 * usually given by a 16 bit number that precedes them.
 */

/*
 * PZ Syntax description
 *
 * The PZ file begins with a magic number, a description string whose prefix is
 * given below (suffix & length don't matter allowing an ascii version
 * number to be provided), a 16 bit version number, an options entry then
 * the file's entries.
 *
 *   PZ ::= Magic DescString VersionNumber Options DataEntries ProcEntries
 *
 *   Options ::= NumOptions(16bit) OptionEntry*
 *
 * All option entries begin with a 16 bit type and a 16 bit length.  The
 * length gives the length of the value and the type says how to interpret
 * it.
 *
 *   OptionEntry ::= OptionType(16bit) Len(16bit) OptionValue
 *
 *   DataEntries ::= NumDatas(32bit) DataEntry*
 *
 *  A data entry is a data type followed by the data (Numbers) finally
 *  followed by reference information.  The number and widths of each number
 *  are given by the data type.  References to other data items aren't
 *  included in the number (take up no space in the file).  TODO: proc
 *  references.  References are given in the order that the pointer fields
 *  occur, there number and position can be determined using the DataType.
 *
 *   DataEntry ::= DataType Num* DataReference*
 *
 *   DataType ::= DATA_BASIC(8) Width
 *              | DATA_ARRAY(8) NumElements(16) Width
 *              | DATA_STRUCT(8) NumElements(16) Width*
 *
 *   Width ::= 1 | 2 | 4 | 8 | 0 (meaning a pointer)
 *
 *   DataReference ::= DataIndex(32bit)
 *
 *   Num ::= an integer, the width used depends on the corresponding data
 *           type.
 *
 *   ProcEntries ::= NumProcs(32bit) ProcEntry*
 */

#define PZ_MAGIC_NUMBER         0x505A
#define PZ_MAGIC_STRING_PART    "Plasma abstract machine bytecode"
#define PZ_FORMAT_VERSION       0

#define PZ_OPT_ENTRY_PROC       0
    /* Value: 32bit number of the program's entry procedure aka main() */

#define PZ_DATA_BASIC           0
#define PZ_DATA_ARRAY           1
#define PZ_DATA_STRUCT          2

/*
 * Instruction opcodes
 *
 ***********************/

typedef enum {
    PZI_LOAD_IMMEDIATE_8,
    PZI_LOAD_IMMEDIATE_16,
    PZI_LOAD_IMMEDIATE_32,
    PZI_LOAD_IMMEDIATE_64,
    PZI_LOAD_IMMEDIATE_MWORD,
    PZI_LOAD_IMMEDIATE_DATA,
    PZI_CALL
} opcode;

#endif /* ! PZ_FORMAT_H */
