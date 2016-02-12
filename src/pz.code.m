%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module pz.code.
%
% PZ representation of code.
%
% Copyright (C) 2015 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
%-----------------------------------------------------------------------%
:- interface.

:- import_module symtab.

:- import_module list.

:- type pz_proc
    --->    pz_proc(
                pzp_name            :: symbol,
                pzp_signature       :: pz_signature,

                    % Procedures imported from other modules will not have a
                    % body.
                pzp_blocks          :: maybe(list(pz_block))
            ).

    % A procedure's signature describes how it behaves with respect to the
    % parameter stack.
    %
    %   ( before - after )
    %
    % before is the list of items (left = lower) on the stack before the
    % call, after is the list of items (left = lower) on the stack after the
    % call.  Of course other things may be on the stack, but this call
    % promises no to affect them.
    %
    % The bytecode interpreter/code generator isn't required to check this,
    % but it may use this information to generate code - so it must be
    % correct.
    %
    % XXX: varargs
    %
:- type pz_signature
    --->    pz_signature(
                pzs_before      :: list(pz_data_width),
                pzs_after       :: list(pz_data_width)
            ).

:- type pz_block
    --->    pz_block(
                pzb_instrs          :: list(pz_instr)
            ).

:- type pz_instr
    --->    pzi_load_immediate(pzf_operand_width, immediate_value)
    ;       pzi_ze(pzf_operand_width, pzf_operand_width)
    ;       pzi_se(pzf_operand_width, pzf_operand_width)
    ;       pzi_trunc(pzf_operand_width, pzf_operand_width)
    ;       pzi_add(pzf_operand_width)
    ;       pzi_sub(pzf_operand_width)
    ;       pzi_mul(pzf_operand_width)
    ;       pzi_div(pzf_operand_width)
    ;       pzi_lt_u(pzf_operand_width)
    ;       pzi_lt_s(pzf_operand_width)
    ;       pzi_gt_u(pzf_operand_width)
    ;       pzi_gt_s(pzf_operand_width)
    ;       pzi_dup
    ;       pzi_drop
    ;       pzi_swap
    ;       pzi_call(pzp_id)
    ;       pzi_cjmp(int, pzf_operand_width)
    ;       pzi_ret.

:- type immediate_value
    --->    immediate8(int)
    ;       immediate16(int)
    ;       immediate32(int)
    ;       immediate64(
                i64_high    :: int,
                i64_low     :: int
            )
    ;       immediate_data(pzd_id)
    ;       immediate_code(pzp_id)
    ;       immediate_label(int).

:- pred instr_immediate(pz_instr::in, immediate_value::out) is semidet.

    % The data widths encoded in the instruction stream are seperate from
    % those in pz.m, these do not need to understand which is a pointer and
    % which isn't.
    %
:- type pzf_operand_width
    --->    pzow_8
    ;       pzow_16
    ;       pzow_32
    ;       pzow_64
    ;       pzow_fast
    ;       pzow_ptr.

:- type maybe_operand_width
    --->    one_width(pzf_operand_width)
    ;       two_widths(pzf_operand_width, pzf_operand_width)
    ;       no_width.

:- pred instr_operand_width(pz_instr, maybe_operand_width).
:- mode instr_operand_width(in, out) is det.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

:- pragma foreign_decl("C",
"
#include ""pz_instructions.h""
").

instr_immediate(Instr, Imm) :-
    require_complete_switch [Instr]
    ( Instr = pzi_load_immediate(_, Imm)
    ; Instr = pzi_call(Callee),
        Imm = immediate_code(Callee)
    ; Instr = pzi_cjmp(Target, _),
        Imm = immediate_label(Target)
    ;
        ( Instr = pzi_ze(_, _)
        ; Instr = pzi_se(_, _)
        ; Instr = pzi_trunc(_, _)
        ; Instr = pzi_add(_)
        ; Instr = pzi_sub(_)
        ; Instr = pzi_mul(_)
        ; Instr = pzi_div(_)
        ; Instr = pzi_lt_u(_)
        ; Instr = pzi_lt_s(_)
        ; Instr = pzi_gt_u(_)
        ; Instr = pzi_gt_s(_)
        ; Instr = pzi_dup
        ; Instr = pzi_drop
        ; Instr = pzi_swap
        ; Instr = pzi_ret
        ),
        false
    ).

%-----------------------------------------------------------------------%

:- pragma foreign_enum("C",
    pzf_operand_width/0, [
    pzow_8      - "PZOW_8",
    pzow_16     - "PZOW_16",
    pzow_32     - "PZOW_32",
    pzow_64     - "PZOW_64",
    pzow_fast   - "PZOW_FAST",
    pzow_ptr    - "PZOW_PTR"
]).

instr_operand_width(pzi_load_immediate(W, _),   one_width(W)).
instr_operand_width(pzi_ze(W1, W2),             two_widths(W1, W2)).
instr_operand_width(pzi_se(W1, W2),             two_widths(W1, W2)).
instr_operand_width(pzi_trunc(W1, W2),          two_widths(W1, W2)).
instr_operand_width(pzi_add(W),                 one_width(W)).
instr_operand_width(pzi_sub(W),                 one_width(W)).
instr_operand_width(pzi_mul(W),                 one_width(W)).
instr_operand_width(pzi_div(W),                 one_width(W)).
instr_operand_width(pzi_lt_u(W),                one_width(W)).
instr_operand_width(pzi_lt_s(W),                one_width(W)).
instr_operand_width(pzi_gt_u(W),                one_width(W)).
instr_operand_width(pzi_gt_s(W),                one_width(W)).
instr_operand_width(pzi_dup,                    no_width).
instr_operand_width(pzi_drop,                   no_width).
instr_operand_width(pzi_swap,                   no_width).
instr_operand_width(pzi_call(_),                no_width).
instr_operand_width(pzi_cjmp(_, W),             one_width(W)).
instr_operand_width(pzi_ret,                    no_width).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
