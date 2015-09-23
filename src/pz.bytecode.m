%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module pz.bytecode.
%
% Common code for reading or writing PZ bytecode.
%
% Copyright (C) 2015 Paul Bone
% Distributed under the terms of the GPLv2 see ../LICENSE.tools
%
%-----------------------------------------------------------------------%

:- interface.

:- import_module int.
:- import_module string.

%-----------------------------------------------------------------------%

:- func pzf_magic = int.

:- func pzf_id_string = string.

:- func pzf_version = int.

%-----------------------------------------------------------------------%

% Constants for encoding option types.

:- func pzf_opt_entry_proc = int.

%-----------------------------------------------------------------------%

% Constants for encoding data types.

:- func pzf_data_basic = int.
:- func pzf_data_array = int.
:- func pzf_data_struct = int.

    % Encode the data width.
    %
:- pred pzf_data_width_int(pz_data_width::in, int::out) is det.

%-----------------------------------------------------------------------%

% Instruction encoding

:- type pz_opcode
    --->    pzo_load_immediate_num
    ;       pzo_load_immediate_data
    ;       pzo_ze
    ;       pzo_se
    ;       pzo_trunc
    ;       pzo_add
    ;       pzo_sub
    ;       pzo_mul
    ;       pzo_div
    ;       pzo_dup
    ;       pzo_swap
    ;       pzo_call.

:- pred instr_opcode(pz_instr, pz_opcode).
:- mode instr_opcode(in, out) is det.

:- pred opcode_byte(pz_opcode, int).
:- mode opcode_byte(in, out) is det.

:- pred pzf_operand_width_byte(pzf_operand_width, int).
:- mode pzf_operand_width_byte(in, out) is det.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module require.

:- pragma foreign_decl("C",
"
#include ""pz_format.h""
#include ""pz_instructions.h""
").

%-----------------------------------------------------------------------%

:- pragma foreign_proc("C",
    pzf_magic = (Magic::out),
    [will_not_call_mercury, thread_safe, promise_pure],
    "
        Magic = PZ_MAGIC_NUMBER;
    ").

%-----------------------------------------------------------------------%

pzf_id_string =
    format("%s version %d", [s(id_string_part), i(pzf_version)]).

:- func id_string_part = string.

:- pragma foreign_proc("C",
    id_string_part = (X::out),
    [will_not_call_mercury, thread_safe, promise_pure],
    "
    /*
     * Cast away the const qualifier, Mercury won't modify this string
     * because it does not have a unique mode.
     */
    X = (char*)PZ_MAGIC_STRING_PART;
    ").

%-----------------------------------------------------------------------%

:- pragma foreign_proc("C",
    pzf_version = (X::out),
    [will_not_call_mercury, thread_safe, promise_pure],
    "X = PZ_FORMAT_VERSION;").

%-----------------------------------------------------------------------%

:- pragma foreign_proc("C",
    pzf_opt_entry_proc = (X::out),
    [will_not_call_mercury, thread_safe, promise_pure],
    "X = PZ_OPT_ENTRY_PROC;").

%-----------------------------------------------------------------------%

:- pragma foreign_proc("C",
    pzf_data_basic = (X::out),
    [will_not_call_mercury, thread_safe, promise_pure],
    "X = PZ_DATA_BASIC;").
:- pragma foreign_proc("C",
    pzf_data_array = (X::out),
    [will_not_call_mercury, thread_safe, promise_pure],
    "X = PZ_DATA_ARRAY;").
:- pragma foreign_proc("C",
    pzf_data_struct = (X::out),
    [will_not_call_mercury, thread_safe, promise_pure],
    "X = PZ_DATA_STRUCT;").

pzf_data_width_int(Width, BasicWidth \/ WidthTypeInt) :-
    basic_width(Width, BasicWidth),
    width_type(Width, WidthType),
    width_type_int(WidthType, WidthTypeInt).

:- pred basic_width(pz_data_width::in, int::out) is det.

basic_width(w8,       0x01).
basic_width(w16,      0x02).
basic_width(w32,      0x04).
basic_width(w64,      0x08).
basic_width(ptr,      0x00).
% for now, these values are always 32 bits wide.
basic_width(w_ptr,    0x04).
basic_width(w_fast,   0x04).

:- type width_type
    --->    word
    ;       pointer
    ;       word_pointer
    ;       word_fast.

:- pred width_type(pz_data_width::in, width_type::out) is det.

width_type(w8,      word).
width_type(w16,     word).
width_type(w32,     word).
width_type(w64,     word).
width_type(w_fast,  word_fast).
width_type(w_ptr,   word_pointer).
width_type(ptr,     pointer).

:- pred width_type_int(width_type::in, int::out) is det.

width_type_int(word,            width_type_normal).
width_type_int(pointer,         width_type_ptr).
width_type_int(word_fast,       width_type_fast).
width_type_int(word_pointer,    width_type_wptr).

:- func width_type_normal = int.
:- pragma foreign_proc("C",
    width_type_normal = (Num::out),
    [will_not_call_mercury, promise_pure, thread_safe],
    "Num = PZ_DATA_WIDTH_TYPE_NORMAL;").

:- func width_type_ptr = int.
:- pragma foreign_proc("C",
    width_type_ptr = (Num::out),
    [will_not_call_mercury, promise_pure, thread_safe],
    "Num = PZ_DATA_WIDTH_TYPE_PTR;").

:- func width_type_wptr = int.
:- pragma foreign_proc("C",
    width_type_wptr = (Num::out),
    [will_not_call_mercury, promise_pure, thread_safe],
    "Num = PZ_DATA_WIDTH_TYPE_WPTR;").

:- func width_type_fast = int.
:- pragma foreign_proc("C",
    width_type_fast = (Num::out),
    [will_not_call_mercury, promise_pure, thread_safe],
    "Num = PZ_DATA_WIDTH_TYPE_FAST;").

%-----------------------------------------------------------------------%

% Instruction encoding

:- pragma foreign_enum("C", pz_opcode/0, [
    pzo_load_immediate_num  - "PZI_LOAD_IMMEDIATE_NUM",
    pzo_load_immediate_data - "PZI_LOAD_IMMEDIATE_DATA",
    pzo_ze                  - "PZI_ZE",
    pzo_se                  - "PZI_SE",
    pzo_trunc               - "PZI_TRUNC",
    pzo_add                 - "PZI_ADD",
    pzo_sub                 - "PZI_SUB",
    pzo_mul                 - "PZI_MUL",
    pzo_div                 - "PZI_DIV",
    pzo_dup                 - "PZI_DUP",
    pzo_swap                - "PZI_SWAP",
    pzo_call                - "PZI_CALL"
]).

:- pragma foreign_proc("C",
    opcode_byte(OpcodeValue::in, Byte::out),
    [will_not_call_mercury, promise_pure, thread_safe],
    "Byte = OpcodeValue").

instr_opcode(pzi_load_immediate(_, Imm), Opcode) :-
    (
        ( Imm = immediate8(_)
        ; Imm = immediate16(_)
        ; Imm = immediate32(_)
        ; Imm = immediate64(_)
        ),
        Opcode = pzo_load_immediate_num
    ;
        Imm = immediate_data(_),
        Opcode = pzo_load_immediate_data
    ;
        Imm = immediate_code(_),
        sorry($file, $pred, "Load immediate code reference")
    ).
instr_opcode(pzi_add(_),    pzo_add).
instr_opcode(pzi_sub(_),    pzo_sub).
instr_opcode(pzi_mul(_),    pzo_mul).
instr_opcode(pzi_div(_),    pzo_div).
instr_opcode(pzi_dup(_),    pzo_dup).
instr_opcode(pzi_swap(_),   pzo_swap).
instr_opcode(pzi_call(_),   pzo_call).

%-----------------------------------------------------------------------%

:- pragma foreign_proc("C",
    pzf_operand_width_byte(Width::in, Byte::out),
    [will_not_call_mercury, promise_pure, thread_safe],
    "Byte = Width;").

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
