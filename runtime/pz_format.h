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
 *   PZ ::= Magic DescString VersionNumber Options ImportDataRefs
 *          ImportProcRefs DataEntries ProcEntries
 *
 * All option entries begin with a 16 bit type and a 16 bit length.  The
 * length gives the length of the value and the type says how to interpret
 * it.
 *
 *   Options ::= NumOptions(16bit) OptionEntry*
 *
 *   OptionEntry ::= OptionType(16bit) Len(16bit) OptionValue
 *
 *  Procedure and data entries are each given a unique 32bit procedure or
 *  data ID.  To clarify, procedures and data entries exist in seperate ID
 *  spaces.  The IDs start at 0 for the first entry and are given
 *  sequentially in file order.  Therefore the imported procedures have
 *  lower IDs than local ones.  IDs are used for example in the call
 *  instruction which must specify the callee.
 *
 *  Import data refs are currently not implemented.
 *
 *   ImportDataRefs ::= NumRefs(32bit) ImportDataRef*
 *
 *   ImportDataRef ::= ModuleName(String) DataName(String)
 *
 *  Import proc refs map IDs onto procedure names to be provided by other
 *  modules.
 *
 *   ImportProcRefs ::= NumRefs(32bit) ImportProcRef*
 *
 *   ImportProcRef ::= ModuleName(String) ProcName(String)
 *
 *  A data entry is a data type followed by the data (numbers and
 *  references).  The number and widths of each number are given by the data
 *  type.  TODO: proc references.
 *
 *   DataEntries ::= NumDatas(32bit) DataEntry*
 *
 *   DataEntry ::= DataType DataValue*
 *
 *   DataType ::= DATA_BASIC(8) Width
 *              | DATA_ARRAY(8) NumElements(16) Width
 *              | DATA_STRUCT(8) NumElements(16) Width*
 *
 *   Width (see below)
 *
 *  Which data value depends upon context.
 *
 *   DataValue ::= Num
 *               | DataIndex(32bit)
 *
 *   ProcEntries ::= NumProcs(32bit) ProcEntry*
 *
 *   ProcEntry ::= NumBlocks(32bit) Block+
 *   Block ::= NumInstructions(32bit) Instruction+
 *
 *   Instruction ::= Opcode(8bit) WidthByte{0,2} Immediate? InstructionStream?
 *
 *  In many cases the width of an integer is availble by context, in those
 *  cases it is just stored as that many bytes.  The on-disk size of
 *  pointer-width and fast integers is 32 bit.
 *
 *   Num ::= Integer
 *
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
 * The high bits of a data width give the width type.  Width types are:
 *  - Pointers: encoded as 32-bit references to some other value, updated on
 *    load.  TODO: Null pointer, static tagged pointer.
 *  - Words with pointer width: Must be encoded with 32bits.
 *  - Fast words:               Must be encoded with 32bits.
 *  - Normal:                   Encoded and in-memory width are the same.
 *
 * The low bits give the width for normal-width values.  Other values are
 * always encoded as 32bit.  (TODO: maybe this can be changed with a PZ file
 * option.)
 */
#define PZ_DATA_WIDTH_TYPE_BITS     0xF0
#define PZ_DATA_WIDTH_BYTES_BITS    0x0F
#define PZ_DATA_WIDTH_TYPE(byte)    ((byte) & PZ_DATA_WIDTH_TYPE_BITS)
#define PZ_DATA_WIDTH_BYTES(byte)   ((byte) & PZ_DATA_WIDTH_BYTES_BITS)
#define PZ_MAKE_DATA_WIDTH(type, bytes) \
    ((type) | (bytes))

enum pz_data_width_type {
    pz_width_type_normal    = 0x10,
    pz_width_type_fast      = 0x20,
    pz_width_type_wptr      = 0x30,
    pz_width_type_ptr       = 0x40
};

#endif /* ! PZ_FORMAT_H */
