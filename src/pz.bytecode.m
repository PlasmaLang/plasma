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

%-----------------------------------------------------------------------%

% Instruction encoding

:- pred instr_opcode(pz_instr, int).
:- mode instr_opcode(in, out) is det.

:- type immediate_value
    --->    no_immediate
    ;       immediate8(int)
    ;       immediate16(int)
    ;       immediate32(int)
    ;       immediate64(int).

:- pred instr_immediate(pz::in, pz_instr::in, immediate_value::out) is det.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

:- import_module list.

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

%-----------------------------------------------------------------------%

instr_opcode(pzi_load_immediate_8(_),  op_load_immediate_8).
instr_opcode(pzi_load_immediate_16(_), op_load_immediate_16).
instr_opcode(pzi_load_immediate_32(_), op_load_immediate_32).
instr_opcode(pzi_load_immediate_64(_), op_load_immediate_64).
instr_opcode(pzi_load_data_ref(_),     op_load_data_ref).
instr_opcode(pzi_call(_),              op_call).

:- func op_load_immediate_8 = int.
:- pragma foreign_proc("C",
    op_load_immediate_8 = (Int::out),
    [will_not_call_mercury, promise_pure, thread_safe],
    "Int = PZI_LOAD_IMMEDIATE_8;").

:- func op_load_immediate_16 = int.
:- pragma foreign_proc("C",
    op_load_immediate_16 = (Int::out),
    [will_not_call_mercury, promise_pure, thread_safe],
    "Int = PZI_LOAD_IMMEDIATE_16;").

:- func op_load_immediate_32 = int.
:- pragma foreign_proc("C",
    op_load_immediate_32 = (Int::out),
    [will_not_call_mercury, promise_pure, thread_safe],
    "Int = PZI_LOAD_IMMEDIATE_32;").

:- func op_load_immediate_64 = int.
:- pragma foreign_proc("C",
    op_load_immediate_64 = (Int::out),
    [will_not_call_mercury, promise_pure, thread_safe],
    "Int = PZI_LOAD_IMMEDIATE_64;").

:- func op_load_data_ref = int.
:- pragma foreign_proc("C",
    op_load_data_ref = (Int::out),
    [will_not_call_mercury, promise_pure, thread_safe],
    "Int = PZI_LOAD_IMMEDIATE_DATA;").

:- func op_call = int.
:- pragma foreign_proc("C",
    op_call = (Int::out),
    [will_not_call_mercury, promise_pure, thread_safe],
    "Int = PZI_CALL;").

instr_immediate(PZ, Instr, Imm) :-
    ( Instr = pzi_load_immediate_8(Int),
        Imm = immediate8(Int)
    ; Instr = pzi_load_immediate_16(Int),
        Imm = immediate16(Int)
    ; Instr = pzi_load_immediate_32(Int),
        Imm = immediate32(Int)
    ; Instr = pzi_load_immediate_64(Int),
        Imm = immediate64(Int)
    ; Instr = pzi_load_data_ref(DID),
        Imm = immediate32(DID ^ pzd_id_num)
    ; Instr = pzi_call(PID),
        Imm = immediate32(pzp_id_get_num(PZ, PID))
    ).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
