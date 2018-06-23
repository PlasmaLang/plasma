/*
 * Plasma bytecode format constants
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2015-2016 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 *
 * This file is used by both the tools in runtime/ and src/
 */

#ifndef PZ_FORMAT_H
#define PZ_FORMAT_H

/*
 * The PZ format is a binary format.  No padding is used and all numbers are
 * unsigned integers in big-endian format unless otherwise specified.
 * Strings are ANSI strings without a null terminated byte.  Their length is
 * usually given by a 16 bit number that precedes them.
 */

/*
 * PZ Syntax description
 * =====================
 *
 * The PZ file begins with a magic number, a description string whose prefix
 * is given below (suffix & length don't matter allowing an ascii version
 * number to be provided), a 16 bit version number, an options entry then
 * the file's entries.
 *
 *   PZ ::= Magic DescString VersionNumber Options
 *          NumImportProcs(32bit) NumStructs(32bit) NumDatas(32bit)
 *          NumProcs(32bit) NumClosures(32bit)
 *          ImportProcRef* StructEntry* DataEntry* ProcEntry*
 *          ClosureEntry*
 *
 * Options
 * -------
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
 *  sequentially in file order.  IDs are used for example in the call
 *  instruction which must specify the callee.
 *
 * Imports
 * -------
 *
 *  Import proc refs map IDs onto procedure names to be provided by other
 *  modules.  Imported procedures are identified by a high 31st bit.
 *
 *   ImportProcRef ::= ModuleName(String) ProcName(String)
 *
 * Struct information
 * ------------------
 *
 *   StructEntry ::= NumFields(32bit) Width*
 *
 * Constant data
 * -------------
 *
 *  A data entry is a data type followed by the data (numbers and
 *  references).  The number and widths of each number are given by the data
 *  type.  TODO: proc references.
 *
 *   DataEntry ::= DataType DataValue*
 *
 * Note that an array of structs is acheived by an array o pointers to
 * pre-defined structs.  (TODO: it'd be nice to support other data layouts
 * like an array of structs.)
 *
 *   DataType ::= DATA_BASIC(8) Width
 *              | DATA_ARRAY(8) NumElements(16) Width
 *              | DATA_STRUCT(8) StructRef
 *
 *  Which data value depends upon context.
 *
 *   DataValue ::= ENC_NORMAL NumBytes Byte*
 *               | ENC_FAST 4 Byte*
 *               | ENC_WPTR 4 Byte*
 *               | ENC_PTR 4 DataIndex(32bit)
 *
 *  The encoding type and number of bytes are a single byte made up by
 *  PZ_MAKE_ENC below.  Currently fast words and pointer-sized words are
 *  always 32bit.  TODO: Support references to code.
 *
 * Code
 * ----
 *
 *   ProcEntry ::= NumBlocks(32bit) Block+
 *   Block ::= NumInstructions(32bit) Instruction+
 *
 *   Instruction ::= Opcode(8bit) WidthByte{0,2} Immediate?
 *      InstructionStream?
 *
 * Closures
 * --------
 *
 *   ClosureEntry ::= ProcId(32bit) DataId(32bit)
 *
 * Shared items
 * ------------
 *
 *  Widths are a single byte defined by the Width enum.  Note that a data
 *  width (a width for data items) is a seperate thing, and encoded
 *  differently.  They may be:
 *      PZW_8,
 *      PZW_16,
 *      PZW_32,
 *      PZW_64,
 *      PZW_FAST,      efficient integer width
 *      PZW_PTR,       native pointer width
 */

#define PZ_MAGIC_NUMBER         0x505A
#define PZ_MAGIC_STRING_PART    "Plasma abstract machine bytecode"
#define PZ_FORMAT_VERSION       0

#define PZ_OPT_ENTRY_CLOSURE    0
    /* Value: 32bit number of the program's entry closure */

/*
 * The width of data, either as an operand or in memory such as in a struct.
 */
typedef enum {
    PZW_8,
    PZW_16,
    PZW_32,
    PZW_64,
    PZW_FAST, // efficient integer width
    PZW_PTR,  // native pointer width
} Width;

#define PZ_DATA_ARRAY           0
#define PZ_DATA_STRUCT          1

/*
 * The high bits of a data width give the width type.  Width types are:
 *  - Pointers:                 32-bit references to some other
 *                              value, updated on load.
 *  - Words with pointer width: 32-bit values zero-extended to the width of
 *                              a pointer.
 *  - Fast words:               Must be encoded with 32bits.
 *  - Normal:                   Encoded and in-memory width are the same.
 *
 * The low bits give the width for normal-width values.  Other values are
 * always encoded as 32bit.  (TODO: maybe this can be changed with a PZ file
 * option.)
 */
#define PZ_DATA_ENC_TYPE_BITS     0xF0
#define PZ_DATA_ENC_BYTES_BITS    0x0F
#define PZ_DATA_ENC_TYPE(byte)    ((byte) & PZ_DATA_ENC_TYPE_BITS)
#define PZ_DATA_ENC_BYTES(byte)   ((byte) & PZ_DATA_ENC_BYTES_BITS)
#define PZ_MAKE_ENC(type, bytes)  ((type) | (bytes))

enum pz_data_enc_type {
    pz_data_enc_type_normal = 0x10,
    pz_data_enc_type_fast   = 0x20,
    pz_data_enc_type_wptr   = 0x30,
    pz_data_enc_type_ptr    = 0x40
};

#define PZ_ID_IMPORTED  (1<<31)
#define PZ_ID_LOCAL     0
#define PZ_ID_TAG       (1<<31)
#define PZ_ID_REAL      ~PZ_ID_TAG

#define PZ_ID_GET_TAG(x)  ((x) & PZ_ID_TAG)
#define PZ_ID_GET_REAL(x) ((x) & PZ_ID_REAL)

#endif /* ! PZ_FORMAT_H */
