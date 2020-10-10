%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module pz.format.
%
% Common code for the PZ file format.
%
% Copyright (C) 2015-2020 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
%-----------------------------------------------------------------------%

:- interface.

:- import_module pz.bytecode.
:- import_module pz.code.

%-----------------------------------------------------------------------%

:- func pz_object_magic = uint32.

:- func pz_ball_magic = uint32.

:- func pz_object_id_string = string.
:- func pz_object_id_string_part = string.

:- func pz_ball_id_string = string.
:- func pz_ball_id_string_part = string.

:- func pz_version = uint16.

%-----------------------------------------------------------------------%

% Constants for encoding option types.

:- func pzf_opt_entry_closure = uint16.
:- func pzf_opt_entry_candidate = uint16.

:- pred pz_signature_byte(pz_entry_signature, uint8).
:- mode pz_signature_byte(in, out) is det.
:- mode pz_signature_byte(out, in) is semidet.

%-----------------------------------------------------------------------%

% Constants for encoding data types.

:- func pzf_data_array = uint8.
:- func pzf_data_struct = uint8.

    % Encoding type is used for data items, it is used by the code that
    % reads/writes this static data so that it knows how to interpret each
    % value.
    %
:- type enc_type
    --->    t_normal
    ;       t_wfast
    ;       t_wptr
    ;       t_data
    ;       t_import
    ;       t_closure.

:- pred pz_enc_byte(enc_type, int, uint8).
:- mode pz_enc_byte(in, in, out) is det.
:- mode pz_enc_byte(out, out, in) is semidet.

%-----------------------------------------------------------------------%

:- pred code_entry_byte(code_entry_type, uint8).
:- mode code_entry_byte(in, out) is det.
:- mode code_entry_byte(out, in) is semidet.

%-----------------------------------------------------------------------%

:- pred opcode_byte(pz_opcode, uint8).
:- mode opcode_byte(in, out) is det.
:- mode opcode_byte(out, in) is semidet.

:- pred pz_width_byte(pz_width, uint8).
:- mode pz_width_byte(in, out) is det.
:- mode pz_width_byte(out, in) is semidet.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

:- import_module uint16.

:- pragma foreign_decl("C",
"
#include ""pz_common.h""
#include ""pz_format.h""
").

%-----------------------------------------------------------------------%

:- pragma foreign_proc("C",
    pz_object_magic = (Magic::out),
    [will_not_call_mercury, thread_safe, promise_pure],
    "
        Magic = PZ_OBJECT_MAGIC_NUMBER;
    ").

:- pragma foreign_proc("C",
    pz_ball_magic = (Magic::out),
    [will_not_call_mercury, thread_safe, promise_pure],
    "
        Magic = PZ_BALL_MAGIC_NUMBER;
    ").

%-----------------------------------------------------------------------%

pz_object_id_string =
    format("%s version %d",
        [s(pz_object_id_string_part), i(to_int(pz_version))]).

:- pragma foreign_proc("C",
    pz_object_id_string_part = (X::out),
    [will_not_call_mercury, thread_safe, promise_pure],
    "
    /*
     * Cast away the const qualifier, Mercury won't modify this string
     * because it does not have a unique mode.
     */
    X = (char*)PZ_OBJECT_MAGIC_STRING;
    ").

pz_ball_id_string =
    format("%s version %d",
        [s(pz_ball_id_string_part), i(to_int(pz_version))]).

:- pragma foreign_proc("C",
    pz_ball_id_string_part = (X::out),
    [will_not_call_mercury, thread_safe, promise_pure],
    "
    /*
     * Cast away the const qualifier, Mercury won't modify this string
     * because it does not have a unique mode.
     */
    X = (char*)PZ_BALL_MAGIC_STRING;
    ").

%-----------------------------------------------------------------------%

:- pragma foreign_proc("C",
    pz_version = (X::out),
    [will_not_call_mercury, thread_safe, promise_pure],
    "X = PZ_FORMAT_VERSION;").

%-----------------------------------------------------------------------%

:- pragma foreign_proc("C",
    pz_signature_byte(Val::in, Byte::out),
    [promise_pure, thread_safe, will_not_call_mercury,
        will_not_throw_exception],
    "
        Byte = Val;
    ").

:- pragma foreign_proc("C",
    pz_signature_byte(Val::out, Byte::in),
    [promise_pure, thread_safe, will_not_call_mercury,
        will_not_throw_exception],
    "
        SUCCESS_INDICATOR = Byte <= PZ_OPT_ENTRY_SIG_LAST;
        Val = Byte;
    ").

%-----------------------------------------------------------------------%

:- pragma foreign_proc("C",
    pzf_opt_entry_closure = (X::out),
    [will_not_call_mercury, thread_safe, promise_pure],
    "X = PZ_OPT_ENTRY_CLOSURE;").

:- pragma foreign_proc("C",
    pzf_opt_entry_candidate = (X::out),
    [will_not_call_mercury, thread_safe, promise_pure],
    "X = PZ_OPT_ENTRY_CANDIDATE;").

%-----------------------------------------------------------------------%

% These are used directly as integers when writing out PZ files,
% otherwise this would be a good candidate for a foreign_enum.

:- pragma foreign_proc("C",
    pzf_data_array = (X::out),
    [will_not_call_mercury, thread_safe, promise_pure],
    "X = PZ_DATA_ARRAY;").
:- pragma foreign_proc("C",
    pzf_data_struct = (X::out),
    [will_not_call_mercury, thread_safe, promise_pure],
    "X = PZ_DATA_STRUCT;").

%-----------------------------------------------------------------------%

:- pragma foreign_enum("C", enc_type/0,
    [   t_normal        - "pz_data_enc_type_normal",
        t_wfast         - "pz_data_enc_type_fast",
        t_wptr          - "pz_data_enc_type_wptr",
        t_data          - "pz_data_enc_type_data",
        t_import        - "pz_data_enc_type_import",
        t_closure       - "pz_data_enc_type_closure"
    ]).

:- pragma foreign_proc("C",
    pz_enc_byte(EncType::in, NumBytes::in, EncInt::out),
    [will_not_call_mercury, promise_pure, thread_safe],
    "
        EncInt = PZ_MAKE_ENC(EncType, NumBytes);
    ").

:- pragma foreign_proc("C",
    pz_enc_byte(EncType::out, NumBytes::out, EncInt::in),
    [will_not_call_mercury, promise_pure, thread_safe],
    "
        EncType = PZ_DATA_ENC_TYPE(EncInt);
        NumBytes = PZ_DATA_ENC_BYTES(EncInt);
        SUCCESS_INDICATOR = EncType <= PZ_LAST_DATA_ENC_TYPE;
    ").

%-----------------------------------------------------------------------%

:- pragma foreign_proc("C",
    code_entry_byte(CodeEntry::in, Byte::out),
    [will_not_call_mercury, promise_pure, thread_safe],
    "Byte = CodeEntry").

:- pragma foreign_proc("C",
    code_entry_byte(CodeEntry::out, Byte::in),
    [will_not_call_mercury, promise_pure, thread_safe],
    "
        CodeEntry = Byte;
        SUCCESS_INDICATOR = CodeEntry < PZ_NUM_CODE_ITEMS;
    ").

%-----------------------------------------------------------------------%

:- pragma foreign_proc("C",
    opcode_byte(OpcodeValue::in, Byte::out),
    [will_not_call_mercury, promise_pure, thread_safe],
    "Byte = OpcodeValue").

:- pragma foreign_proc("C",
    opcode_byte(OpcodeValue::out, Byte::in),
    [will_not_call_mercury, promise_pure, thread_safe],
    "
        OpcodeValue = Byte;
        SUCCESS_INDICATOR = Byte < PZ_NUM_OPCODES;
    ").

%-----------------------------------------------------------------------%

:- pragma foreign_proc("C",
    pz_width_byte(WidthValue::in, Byte::out),
    [will_not_call_mercury, promise_pure, thread_safe],
    "Byte = WidthValue;").

:- pragma foreign_proc("C",
    pz_width_byte(WidthValue::out, Byte::in),
    [will_not_call_mercury, promise_pure, thread_safe],
    "
        WidthValue = Byte;
        SUCCESS_INDICATOR = Byte < PZ_NUM_WIDTHS;
    ").

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
