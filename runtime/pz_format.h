/*
 * Plasma bytecode format constants
 * vim: ts=4 sw=4 et
 *
 * Copyright (C) 2015-2016, 2019 Plasma Team
 * Distributed under the terms of the MIT license, see ../LICENSE.code
 *
 * This file is used by both the tools in runtime/ and src/
 */

#ifndef PZ_FORMAT_H
#define PZ_FORMAT_H

#ifdef __cplusplus
extern "C" {
#endif

/*
 * The PZ format is a binary format.  No padding is used and all numbers are
 * unsigned integers in little-endian format unless otherwise specified.
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
 *   PZ ::= Magic(32bit) DescString VersionNumber(16bit) Options
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
 *  modules.  Imported closures are identified by a high 31st bit.
 *
 *   ImportProcRef ::= ModuleName(String) SymbolName(String)
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
 *  references).  The number and in-memory widths of each number are given
 *  by the data type.  The on disk widths/encodings are given in each value.
 *
 *  Data references may not form cycles, and the referred-to data items must
 *  occur before the referred-from items.
 *
 *   DataEntry ::= DataType DataValue*
 *
 *  Note that an array of structs is acheived by an array o pointers to
 *  pre-defined structs.  (TODO: it'd be nice to support other data layouts
 *  like an array of structs.)
 *
 *   DataType ::= DATA_ARRAY(8) NumElements(16) Width
 *              | DATA_STRUCT(8) StructRef
 *
 *  Which data value depends upon context.
 *
 *   DataValue ::= ENC_NORMAL NumBytes Byte*
 *               | ENC_FAST 4 Byte*
 *               | ENC_WPTR 4 Byte*
 *               | ENC_DATA 4 DataIndex(32bit)
 *               | ENC_IMPORT 4 ImportIndex(32bit)
 *               | ENC_CLOSURE 4 ClosureIndex(32bit)
 *
 *  The encoding type and number of bytes are a single byte made up by
 *  PZ_MAKE_ENC below.  Currently fast words and pointer-sized words are
 *  always 32bit.
 *
 * Code
 * ----
 *
 *   ProcEntry ::= Name(String) NumBlocks(32bit) Block+
 *   Block ::= NumInstrObjs(32bit) InstrObj+
 *
 *   InstrObj ::= CODE_INSTR(8) Instruction
 *              | MetaItem
 *   Instruction ::= Opcode(8bit) WidthByte{0,2} Immediate?
 *      InstructionStream?
 *
 *   MetaItem ::= CODE_META_CONTEXT(8) FileName(DataIndex) LineNo(32bit)
 *              | CODE_META_CONTEXT_SHORT(8) LineNo(32bit)
 *              | CODE_META_CONTEXT_NIL(8)
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
 *
 *  Strings are encoded with a number of bytes giving the length followed by
 *  the string's bytes.
 *
 *    String ::= Length(16bit) Bytes*
 *
 */

#define PZ_OBJECT_MAGIC_NUMBER  0x505A4F00
#define PZ_BALL_MAGIC_NUMBER    0x505A4200
#define PZ_OBJECT_MAGIC_STRING  "Plasma object"
#define PZ_BALL_MAGIC_STRING    "Plasma ball"
#define PZ_FORMAT_VERSION       0

#define PZ_OPT_ENTRY_CLOSURE    0
    /* Value: 32bit number of the program's entry closure */

/*
 * The width of data, either as an operand or in memory such as in a struct.
 */
enum PZ_Width {
    PZW_8,
    PZW_16,
    PZW_32,
    PZW_64,
    PZW_FAST, // efficient integer width
    PZW_PTR,  // native pointer width
};
#define PZ_NUM_WIDTHS (PZW_PTR + 1)

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
#define PZ_DATA_ENC_TYPE(byte)    \
    (enum pz_data_enc_type)((byte) & PZ_DATA_ENC_TYPE_BITS)
#define PZ_DATA_ENC_BYTES(byte)   ((byte) & PZ_DATA_ENC_BYTES_BITS)
#define PZ_MAKE_ENC(type, bytes)  ((type) | (bytes))

enum pz_data_enc_type {
    pz_data_enc_type_normal     = 0x00,
    pz_data_enc_type_fast       = 0x10,
    pz_data_enc_type_wptr       = 0x20,
    pz_data_enc_type_data       = 0x30,
    pz_data_enc_type_import     = 0x40,
    pz_data_enc_type_closure    = 0x50,
};
#define PZ_LAST_DATA_ENC_TYPE pz_data_enc_type_closure;

enum PZ_Code_Item {
    PZ_CODE_INSTR,
    PZ_CODE_META_CONTEXT,
    PZ_CODE_META_CONTEXT_SHORT,
    PZ_CODE_META_CONTEXT_NIL,
};
#define PZ_NUM_CODE_ITEMS (PZ_CODE_META_CONTEXT_NIL + 1)

#ifdef __cplusplus
} // extern "C"
#endif

#endif /* ! PZ_FORMAT_H */
