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
 *  A data entry is a data type followed by the data (Numbers) finally
 *  followed by reference information.  The number and widths of each number
 *  are given by the data type.  References to other data items aren't
 *  included in the number (take up no space in the file).  TODO: proc
 *  references.  References are given in the order that the pointer fields
 *  occur, there number and position can be determined using the DataType.
 *
 *   DataEntries ::= NumDatas(32bit) DataEntry*
 *
 *   DataEntry ::= DataType Num* DataReference*
 *
 *   DataType ::= DATA_BASIC(8) Width
 *              | DATA_ARRAY(8) NumElements(16) Width
 *              | DATA_STRUCT(8) NumElements(16) Width*
 *
 *   Width (see below)
 *
 *   DataReference ::= DataIndex(32bit)
 *
 *   Num ::= an integer, the width used depends on the corresponding data
 *           type.
 *
 *   ProcEntries ::= NumProcs(32bit) ProcEntry*
 *
 *   ProcEntry ::= NumBlocks(32bit) Block+
 *   Block ::= NumInstructions(32bit) Instruction+
 *
 *   Instruction ::= Opcode(8bit) WidthByte{0,2} Immediate? InstructionStream?
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
 *    load
 *  - Words with pointer width: Must be encoded with 32bits or fewer.
 *  - Fast words:               Must be encoded with 32bits or fewer.
 *  - Normal:                   Encoded and in-memory width are the same.
 *
 * The low bits give either:
 *  - Normal width: the low bits give the width.
 *  - Poinder-width words or Fast words: The low bits may give the encoded
 *    width, depending on context.
 */
#define PZ_DATA_WIDTH_TYPE_BITS     0xA0
#define PZ_DATA_WIDTH_TYPE_NORMAL   0x00
#define PZ_DATA_WIDTH_TYPE_PTR      0x80
#define PZ_DATA_WIDTH_TYPE_WPTR     0xA0
#define PZ_DATA_WIDTH_TYPE_FAST     0x40

#endif /* ! PZ_FORMAT_H */
