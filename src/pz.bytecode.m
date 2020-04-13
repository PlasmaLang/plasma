%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module pz.bytecode.
%
% Common code for reading or writing PZ bytecode.
%
% Copyright (C) 2015-2020 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
%-----------------------------------------------------------------------%

:- interface.

:- import_module int.
:- import_module string.

:- import_module common_types.

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

:- type code_entry_type
    --->    code_instr
    ;       code_meta_context
    ;       code_meta_context_short
    ;       code_meta_context_nil.

:- inst code_entry_type_context
    --->    code_meta_context
    ;       code_meta_context_short
    ;       code_meta_context_nil.

:- pred code_entry_byte(code_entry_type, uint8).
:- mode code_entry_byte(in, out) is det.
:- mode code_entry_byte(out, in) is semidet.

%-----------------------------------------------------------------------%
% Instruction encoding
%-----------------------------------------------------------------------%

:- type pz_opcode
    --->    pzo_load_immediate_num
    ;       pzo_ze
    ;       pzo_se
    ;       pzo_trunc
    ;       pzo_add
    ;       pzo_sub
    ;       pzo_mul
    ;       pzo_div
    ;       pzo_mod
    ;       pzo_lshift
    ;       pzo_rshift
    ;       pzo_and
    ;       pzo_or
    ;       pzo_xor
    ;       pzo_lt_u
    ;       pzo_lt_s
    ;       pzo_gt_u
    ;       pzo_gt_s
    ;       pzo_eq
    ;       pzo_not
    ;       pzo_drop
    ;       pzo_roll
    ;       pzo_pick
    ;       pzo_call
    ;       pzo_call_import
    ;       pzo_call_ind
    ;       pzo_call_proc
    ;       pzo_tcall
    ;       pzo_tcall_import
    ;       pzo_tcall_ind
    ;       pzo_tcall_proc
    ;       pzo_cjmp
    ;       pzo_jmp
    ;       pzo_ret
    ;       pzo_alloc
    ;       pzo_make_closure
    ;       pzo_load
    ;       pzo_load_named
    ;       pzo_store
    ;       pzo_get_env.

:- type maybe_operand_width
    --->    one_width(pz_width)
    ;       two_widths(pz_width, pz_width)
    ;       no_width.

:- pred instruction(pz_instr, pz_opcode, maybe_operand_width,
    maybe(pz_immediate_value)).
:- mode instruction(in, out, out, out) is det.
:- mode instruction(out, in, in, in) is semidet.

:- type num_needed_widths
    --->    one_width
    ;       two_widths
    ;       no_width.

    % This type represents intermediate values within the instruction
    % stream, such as labels and stack depths.  The related immediate_value
    % type, represents only the types of immediate values that can be loaded
    % with the pzi_load_immediate instruction.
    %
:- type immediate_needed
    --->    im_none
    ;       im_num
    ;       im_closure
    ;       im_proc
    ;       im_import
    ;       im_struct
    ;       im_struct_field
    ;       im_label
    ;       im_depth. % A stack depth

% Instruction encoding information.

:- pred instruction_encoding(pz_opcode, num_needed_widths, immediate_needed).
:- mode instruction_encoding(in, out, out) is det.

%-----------------------------------------------------------------------%

:- pred opcode_byte(pz_opcode, uint8).
:- mode opcode_byte(in, out) is det.
:- mode opcode_byte(out, in) is semidet.

:- pred pz_width_byte(pz_width, uint8).
:- mode pz_width_byte(in, out) is det.
:- mode pz_width_byte(out, in) is semidet.

    % This type represents intermediate values within the instruction
    % stream, such as labels and stack depths.  The related immediate_value
    % type, represents only the types of immediate values that can be loaded
    % with the pzi_load_immediate instruction.
    %
:- type pz_immediate_value
    --->    pz_im_i8(int8)
    ;       pz_im_u8(uint8)
    ;       pz_im_i16(int16)
    ;       pz_im_u16(uint16)
    ;       pz_im_i32(int32)
    ;       pz_im_u32(uint32)
    ;       pz_im_i64(int64)
    ;       pz_im_u64(uint64)
    ;       pz_im_closure(pzc_id)
    ;       pz_im_proc(pzp_id)
    ;       pz_im_import(pzi_id)
    ;       pz_im_struct(pzs_id)
    ;       pz_im_struct_field(pzs_id, field_num)
    ;       pz_im_label(pzb_id)
    ;       pz_im_depth(int). % A stack depth

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module uint8.
:- import_module uint16.

:- import_module util.

:- pragma foreign_decl("C",
"
#include ""pz_common.h""
#include ""pz_format.h""
#include ""pz_instructions.h""
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

pz_ball_id_string =
    format("%s version %d",
        [s(pz_ball_id_string_part), i(to_int(pz_version))]).

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
    pzf_opt_entry_closure = (X::out),
    [will_not_call_mercury, thread_safe, promise_pure],
    "X = PZ_OPT_ENTRY_CLOSURE;").

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
        SUCCESS_INDICATOR = EncType <= pz_data_enc_type_last;
    ").

%-----------------------------------------------------------------------%

:- pragma foreign_enum("C", code_entry_type/0, [
    code_instr              - "PZ_CODE_INSTR",
    code_meta_context       - "PZ_CODE_META_CONTEXT",
    code_meta_context_short - "PZ_CODE_META_CONTEXT_SHORT",
    code_meta_context_nil   - "PZ_CODE_META_CONTEXT_NIL"
]).

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
% Instruction encoding
%-----------------------------------------------------------------------%

:- pragma foreign_enum("C", pz_opcode/0, [
    pzo_load_immediate_num  - "PZI_LOAD_IMMEDIATE_NUM",
    pzo_ze                  - "PZI_ZE",
    pzo_se                  - "PZI_SE",
    pzo_trunc               - "PZI_TRUNC",
    pzo_add                 - "PZI_ADD",
    pzo_sub                 - "PZI_SUB",
    pzo_mul                 - "PZI_MUL",
    pzo_div                 - "PZI_DIV",
    pzo_mod                 - "PZI_MOD",
    pzo_lshift              - "PZI_LSHIFT",
    pzo_rshift              - "PZI_RSHIFT",
    pzo_and                 - "PZI_AND",
    pzo_or                  - "PZI_OR",
    pzo_xor                 - "PZI_XOR",
    pzo_lt_u                - "PZI_LT_U",
    pzo_lt_s                - "PZI_LT_S",
    pzo_gt_u                - "PZI_GT_U",
    pzo_gt_s                - "PZI_GT_S",
    pzo_eq                  - "PZI_EQ",
    pzo_not                 - "PZI_NOT",
    pzo_drop                - "PZI_DROP",
    pzo_roll                - "PZI_ROLL",
    pzo_pick                - "PZI_PICK",
    pzo_call                - "PZI_CALL",
    pzo_call_import         - "PZI_CALL_IMPORT",
    pzo_call_ind            - "PZI_CALL_IND",
    pzo_call_proc           - "PZI_CALL_PROC",
    pzo_tcall               - "PZI_TCALL",
    pzo_tcall_import        - "PZI_TCALL_IMPORT",
    pzo_tcall_ind           - "PZI_TCALL_IND",
    pzo_tcall_proc          - "PZI_TCALL_PROC",
    pzo_cjmp                - "PZI_CJMP",
    pzo_jmp                 - "PZI_JMP",
    pzo_ret                 - "PZI_RET",
    pzo_alloc               - "PZI_ALLOC",
    pzo_make_closure        - "PZI_MAKE_CLOSURE",
    pzo_load                - "PZI_LOAD",
    pzo_load_named          - "PZI_LOAD_NAMED",
    pzo_store               - "PZI_STORE",
    pzo_get_env             - "PZI_GET_ENV"
]).

instruction(pzi_load_immediate(W, NI), pzo_load_immediate_num, one_width(W),
        yes(I)) :-
    immediate_num(NI, I).
instruction(pzi_ze(W1, W2),             pzo_ze,             two_widths(W1, W2),
    no).
instruction(pzi_se(W1, W2),             pzo_se,             two_widths(W1, W2),
    no).
instruction(pzi_trunc(W1, W2),          pzo_trunc,          two_widths(W1, W2),
    no).
instruction(pzi_add(W),                 pzo_add,            one_width(W),
    no).
instruction(pzi_sub(W),                 pzo_sub,            one_width(W),
    no).
instruction(pzi_mul(W),                 pzo_mul,            one_width(W),
    no).
instruction(pzi_div(W),                 pzo_div,            one_width(W),
    no).
instruction(pzi_mod(W),                 pzo_mod,            one_width(W),
    no).
instruction(pzi_lshift(W),              pzo_lshift,         one_width(W),
    no).
instruction(pzi_rshift(W),              pzo_rshift,         one_width(W),
    no).
instruction(pzi_and(W),                 pzo_and,            one_width(W),
    no).
instruction(pzi_or(W),                  pzo_or,             one_width(W),
    no).
instruction(pzi_xor(W),                 pzo_xor,            one_width(W),
    no).
instruction(pzi_lt_u(W),                pzo_lt_u,           one_width(W),
    no).
instruction(pzi_lt_s(W),                pzo_lt_s,           one_width(W),
    no).
instruction(pzi_gt_u(W),                pzo_gt_u,           one_width(W),
    no).
instruction(pzi_gt_s(W),                pzo_gt_s,           one_width(W),
    no).
instruction(pzi_eq(W),                  pzo_eq,             one_width(W),
    no).
instruction(pzi_not(W),                 pzo_not,            one_width(W),
    no).
instruction(pzi_drop,                   pzo_drop,           no_width,
    no).
instruction(pzi_roll(D),                pzo_roll,           no_width,
    yes(pz_im_depth(D))).
instruction(pzi_pick(D),                pzo_pick,           no_width,
    yes(pz_im_depth(D))).
instruction(pzi_call(pzc_closure(C)),   pzo_call,           no_width,
    yes(pz_im_closure(C))).
instruction(pzi_call(pzc_import(I)),    pzo_call_import,    no_width,
    yes(pz_im_import(I))).
instruction(pzi_call(pzc_proc_opt(P)),  pzo_call_proc,      no_width,
    yes(pz_im_proc(P))).
instruction(pzi_call_ind,               pzo_call_ind,       no_width,
    no).
instruction(pzi_tcall(pzc_closure(C)),  pzo_tcall,          no_width,
    yes(pz_im_closure(C))).
instruction(pzi_tcall(pzc_import(I)),   pzo_tcall_import,   no_width,
    yes(pz_im_import(I))).
instruction(pzi_tcall(pzc_proc_opt(P)), pzo_tcall_proc,     no_width,
    yes(pz_im_proc(P))).
instruction(pzi_tcall_ind,              pzo_tcall_ind,      no_width,
    no).
instruction(pzi_cjmp(L, W),             pzo_cjmp,           one_width(W),
    yes(pz_im_label(L))).
instruction(pzi_jmp(L),                 pzo_jmp,            no_width,
    yes(pz_im_label(L))).
instruction(pzi_ret,                    pzo_ret,            no_width,
    no).
instruction(pzi_alloc(S),               pzo_alloc,          no_width,
    yes(pz_im_struct(S))).
instruction(pzi_make_closure(P),        pzo_make_closure,   no_width,
    yes(pz_im_proc(P))).
instruction(pzi_load(S, F, W),          pzo_load,           one_width(W),
    yes(pz_im_struct_field(S, F))).
instruction(pzi_load_named(I, W),       pzo_load_named,     one_width(W),
    yes(pz_im_import(I))).
instruction(pzi_store(S, F, W),         pzo_store,          one_width(W),
    yes(pz_im_struct_field(S, F))).
instruction(pzi_get_env,                pzo_get_env,        no_width,
    no).

:- pred immediate_num(immediate_value, pz_immediate_value).
:- mode immediate_num(in, out) is det.
:- mode immediate_num(out, in) is semidet.

immediate_num(im_i8(N),  pz_im_i8(N)).
immediate_num(im_u8(N),  pz_im_u8(N)).
immediate_num(im_i16(N), pz_im_i16(N)).
immediate_num(im_u16(N), pz_im_u16(N)).
immediate_num(im_i32(N), pz_im_i32(N)).
immediate_num(im_u32(N), pz_im_u32(N)).
immediate_num(im_i64(N), pz_im_i64(N)).
immediate_num(im_u64(N), pz_im_u64(N)).

instruction_encoding(pzo_load_immediate_num,    one_width,  im_num).
instruction_encoding(pzo_ze,                    two_widths, im_none).
instruction_encoding(pzo_se,                    two_widths, im_none).
instruction_encoding(pzo_trunc,                 two_widths, im_none).
instruction_encoding(pzo_add,                   one_width,  im_none).
instruction_encoding(pzo_sub,                   one_width,  im_none).
instruction_encoding(pzo_mul,                   one_width,  im_none).
instruction_encoding(pzo_div,                   one_width,  im_none).
instruction_encoding(pzo_mod,                   one_width,  im_none).
instruction_encoding(pzo_lshift,                one_width,  im_none).
instruction_encoding(pzo_rshift,                one_width,  im_none).
instruction_encoding(pzo_and,                   one_width,  im_none).
instruction_encoding(pzo_or,                    one_width,  im_none).
instruction_encoding(pzo_xor,                   one_width,  im_none).
instruction_encoding(pzo_lt_u,                  one_width,  im_none).
instruction_encoding(pzo_lt_s,                  one_width,  im_none).
instruction_encoding(pzo_gt_u,                  one_width,  im_none).
instruction_encoding(pzo_gt_s,                  one_width,  im_none).
instruction_encoding(pzo_eq,                    one_width,  im_none).
instruction_encoding(pzo_not,                   one_width,  im_none).
instruction_encoding(pzo_drop,                  no_width,   im_none).
instruction_encoding(pzo_roll,                  no_width,   im_depth).
instruction_encoding(pzo_pick,                  no_width,   im_depth).
instruction_encoding(pzo_call,                  no_width,   im_closure).
instruction_encoding(pzo_call_import,           no_width,   im_import).
instruction_encoding(pzo_call_proc,             no_width,   im_proc).
instruction_encoding(pzo_call_ind,              no_width,   im_none).
instruction_encoding(pzo_tcall,                 no_width,   im_closure).
instruction_encoding(pzo_tcall_import,          no_width,   im_import).
instruction_encoding(pzo_tcall_proc,            no_width,   im_proc).
instruction_encoding(pzo_tcall_ind,             no_width,   im_none).
instruction_encoding(pzo_cjmp,                  one_width,  im_label).
instruction_encoding(pzo_jmp,                   no_width,   im_label).
instruction_encoding(pzo_ret,                   no_width,   im_none).
instruction_encoding(pzo_alloc,                 no_width,   im_struct).
instruction_encoding(pzo_make_closure,          no_width,   im_proc).
instruction_encoding(pzo_load,                  one_width,  im_struct_field).
instruction_encoding(pzo_load_named,            one_width,  im_import).
instruction_encoding(pzo_store,                 one_width,  im_struct_field).
instruction_encoding(pzo_get_env,               no_width,   im_none).

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
        SUCCESS_INDICATOR = Byte < PZI_NUM_OPCODES;
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
        SUCCESS_INDICATOR = Byte < PZW_NUM_WIDTHS;
    ").

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
