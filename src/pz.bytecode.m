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

:- func pz_ball_id_string = string.

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

:- pred pz_enc_byte(enc_type::in, int::in, uint8::out) is det.

%-----------------------------------------------------------------------%

:- type code_entry_type
    --->    code_instr
    ;       code_meta_context
    ;       code_meta_context_short
    ;       code_meta_context_nil.

:- pred code_entry_byte(code_entry_type::in, uint8::out) is det.

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

:- pred instruction(pz_instr, pz_opcode, maybe_operand_width).
:- mode instruction(in, out, out) is det.

%-----------------------------------------------------------------------%

:- pred opcode_byte(pz_opcode, uint8).
:- mode opcode_byte(in, out) is det.

:- pred pz_width_byte(pz_width, uint8).
:- mode pz_width_byte(in, out) is det.

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
    ;       pz_im_label(pzb_id).

    % Get the first immedate value if any.
    %
:- pred pz_instr_immediate(pz_instr::in, pz_immediate_value::out) is semidet.

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
    format("%s version %d", [s(object_id_string_part), i(to_int(pz_version))]).

pz_ball_id_string =
    format("%s version %d", [s(ball_id_string_part), i(to_int(pz_version))]).

:- func object_id_string_part = string.

:- pragma foreign_proc("C",
    object_id_string_part = (X::out),
    [will_not_call_mercury, thread_safe, promise_pure],
    "
    /*
     * Cast away the const qualifier, Mercury won't modify this string
     * because it does not have a unique mode.
     */
    X = (char*)PZ_OBJECT_MAGIC_STRING;
    ").

:- func ball_id_string_part = string.

:- pragma foreign_proc("C",
    ball_id_string_part = (X::out),
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

instruction(pzi_load_immediate(W, _),   pzo_load_immediate_num, one_width(W)).
instruction(pzi_ze(W1, W2),             pzo_ze,             two_widths(W1, W2)).
instruction(pzi_se(W1, W2),             pzo_se,             two_widths(W1, W2)).
instruction(pzi_trunc(W1, W2),          pzo_trunc,          two_widths(W1, W2)).
instruction(pzi_add(W),                 pzo_add,            one_width(W)).
instruction(pzi_sub(W),                 pzo_sub,            one_width(W)).
instruction(pzi_mul(W),                 pzo_mul,            one_width(W)).
instruction(pzi_div(W),                 pzo_div,            one_width(W)).
instruction(pzi_mod(W),                 pzo_mod,            one_width(W)).
instruction(pzi_lshift(W),              pzo_lshift,         one_width(W)).
instruction(pzi_rshift(W),              pzo_rshift,         one_width(W)).
instruction(pzi_and(W),                 pzo_and,            one_width(W)).
instruction(pzi_or(W),                  pzo_or,             one_width(W)).
instruction(pzi_xor(W),                 pzo_xor,            one_width(W)).
instruction(pzi_lt_u(W),                pzo_lt_u,           one_width(W)).
instruction(pzi_lt_s(W),                pzo_lt_s,           one_width(W)).
instruction(pzi_gt_u(W),                pzo_gt_u,           one_width(W)).
instruction(pzi_gt_s(W),                pzo_gt_s,           one_width(W)).
instruction(pzi_eq(W),                  pzo_eq,             one_width(W)).
instruction(pzi_not(W),                 pzo_not,            one_width(W)).
instruction(pzi_drop,                   pzo_drop,           no_width).
instruction(pzi_roll(_),                pzo_roll,           no_width).
instruction(pzi_pick(_),                pzo_pick,           no_width).
instruction(pzi_call(pzc_closure(_)),   pzo_call,           no_width).
instruction(pzi_call(pzc_import(_)),    pzo_call_import,    no_width).
instruction(pzi_call(pzc_proc_opt(_)),  pzo_call_proc,      no_width).
instruction(pzi_call_ind,               pzo_call_ind,       no_width).
instruction(pzi_tcall(pzc_closure(_)),  pzo_tcall,          no_width).
instruction(pzi_tcall(pzc_import(_)),   pzo_tcall_import,   no_width).
instruction(pzi_tcall(pzc_proc_opt(_)), pzo_tcall_proc,     no_width).
instruction(pzi_tcall_ind,              pzo_tcall_ind,      no_width).
instruction(pzi_cjmp(_, W),             pzo_cjmp,           one_width(W)).
instruction(pzi_jmp(_),                 pzo_jmp,            no_width).
instruction(pzi_ret,                    pzo_ret,            no_width).
instruction(pzi_alloc(_),               pzo_alloc,          no_width).
instruction(pzi_make_closure(_),        pzo_make_closure,   no_width).
instruction(pzi_load(_, _, W),          pzo_load,           one_width(W)).
instruction(pzi_load_named(_, W),       pzo_load_named,     one_width(W)).
instruction(pzi_store(_, _, W),         pzo_store,          one_width(W)).
instruction(pzi_get_env,                pzo_get_env,        no_width).

%-----------------------------------------------------------------------%

:- pragma foreign_proc("C",
    opcode_byte(OpcodeValue::in, Byte::out),
    [will_not_call_mercury, promise_pure, thread_safe],
    "Byte = OpcodeValue").

%-----------------------------------------------------------------------%

:- pragma foreign_proc("C",
    pz_width_byte(WidthValue::in, Byte::out),
    [will_not_call_mercury, promise_pure, thread_safe],
    "Byte = WidthValue;").

%-----------------------------------------------------------------------%

pz_instr_immediate(Instr, Imm) :-
    require_complete_switch [Instr]
    ( Instr = pzi_load_immediate(_, Imm0),
        immediate_to_pz_immediate(Imm0, Imm)
    ;
        ( Instr = pzi_call(Callee)
        ; Instr = pzi_tcall(Callee)
        ),
        require_complete_switch [Callee]
        ( Callee = pzc_closure(ClosureId),
            Imm = pz_im_closure(ClosureId)
        ; Callee = pzc_import(ImportId),
            Imm = pz_im_import(ImportId)
        ; Callee = pzc_proc_opt(ProcId),
            Imm = pz_im_proc(ProcId)
        )
    ;
        Instr = pzi_make_closure(ProcId),
        Imm = pz_im_proc(ProcId)
    ;
        Instr = pzi_load_named(ImportId, _),
        Imm = pz_im_import(ImportId)
    ;
        ( Instr = pzi_cjmp(Target, _)
        ; Instr = pzi_jmp(Target)
        ),
        Imm = pz_im_label(Target)
    ;
        ( Instr = pzi_roll(NumSlots)
        ; Instr = pzi_pick(NumSlots)
        ),
        Imm = pz_im_u8(det_from_int(NumSlots))
    ;
        ( Instr = pzi_ze(_, _)
        ; Instr = pzi_se(_, _)
        ; Instr = pzi_trunc(_, _)
        ; Instr = pzi_add(_)
        ; Instr = pzi_sub(_)
        ; Instr = pzi_mul(_)
        ; Instr = pzi_div(_)
        ; Instr = pzi_mod(_)
        ; Instr = pzi_lshift(_)
        ; Instr = pzi_rshift(_)
        ; Instr = pzi_and(_)
        ; Instr = pzi_or(_)
        ; Instr = pzi_xor(_)
        ; Instr = pzi_lt_u(_)
        ; Instr = pzi_lt_s(_)
        ; Instr = pzi_gt_u(_)
        ; Instr = pzi_gt_s(_)
        ; Instr = pzi_eq(_)
        ; Instr = pzi_not(_)
        ; Instr = pzi_drop
        ; Instr = pzi_call_ind
        ; Instr = pzi_tcall_ind
        ; Instr = pzi_ret
        ; Instr = pzi_get_env
        ),
        false
    ; Instr = pzi_alloc(Struct),
        Imm = pz_im_struct(Struct)
    ;
        ( Instr = pzi_load(Struct, Field, _)
        ; Instr = pzi_store(Struct, Field, _)
        ),
        Imm = pz_im_struct_field(Struct, Field)
    ).

:- pred immediate_to_pz_immediate(immediate_value, pz_immediate_value).
:- mode immediate_to_pz_immediate(in, out) is det.

immediate_to_pz_immediate(im_i8(Int), pz_im_i8(Int)).
immediate_to_pz_immediate(im_u8(Int), pz_im_u8(Int)).
immediate_to_pz_immediate(im_i16(Int), pz_im_i16(Int)).
immediate_to_pz_immediate(im_u16(Int), pz_im_u16(Int)).
immediate_to_pz_immediate(im_i32(Int), pz_im_i32(Int)).
immediate_to_pz_immediate(im_u32(Int), pz_im_u32(Int)).
immediate_to_pz_immediate(im_i64(Int), pz_im_i64(Int)).
immediate_to_pz_immediate(im_u64(Int), pz_im_u64(Int)).


%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
