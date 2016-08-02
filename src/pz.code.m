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

:- import_module q_name.

:- import_module list.

:- type pz_proc
    --->    pz_proc(
                pzp_name            :: q_name,
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
    % When functions are translated to procedures, parameters are pushed
    % onto the stack in the order they appear - leftmost parameters are
    % deeper on the stack, this is the same for return parameters.
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
                pzb_instrs          :: list(pz_instr_obj)
            ).

    % An instruction object.  Is anything that can appear within an
    % instruction stream including a comment.
    %
:- type pz_instr_obj
    --->    pzio_instr(
                pzio_instr          :: pz_instr
            )
    ;       pzio_comment(
                pzio_comment        :: string
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
    ;       pzi_mod(pzf_operand_width)
    ;       pzi_lshift(pzf_operand_width)
    ;       pzi_rshift(pzf_operand_width)
    ;       pzi_and(pzf_operand_width)
    ;       pzi_or(pzf_operand_width)
    ;       pzi_xor(pzf_operand_width)
    ;       pzi_lt_u(pzf_operand_width)
    ;       pzi_lt_s(pzf_operand_width)
    ;       pzi_gt_u(pzf_operand_width)
    ;       pzi_gt_s(pzf_operand_width)
    ;       pzi_eq(pzf_operand_width)
    ;       pzi_not(pzf_operand_width)
    ;       pzi_drop

            % Roll to the left, the deepest item becomes the TOS and all
            % other items shift along one space deeper (to the left).
    ;       pzi_roll(int)
    ;       pzi_pick(int)
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

%
% Some aliases for commonly used instructions.
%

:- func pzi_dup = pz_instr.

:- func pzi_swap = pz_instr.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

:- import_module require.

:- pragma foreign_decl("C",
"
#include ""pz_common.h""
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
        ( Instr = pzi_roll(NumSlots)
        ; Instr = pzi_pick(NumSlots)
        ),
        ( if NumSlots > 255 then
            sorry($file, $pred, "roll depth greater than 255")
        else
            Imm = immediate8(NumSlots)
        )
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
instr_operand_width(pzi_mod(W),                 one_width(W)).
instr_operand_width(pzi_lshift(W),              one_width(W)).
instr_operand_width(pzi_rshift(W),              one_width(W)).
instr_operand_width(pzi_and(W),                 one_width(W)).
instr_operand_width(pzi_or(W),                  one_width(W)).
instr_operand_width(pzi_xor(W),                 one_width(W)).
instr_operand_width(pzi_lt_u(W),                one_width(W)).
instr_operand_width(pzi_lt_s(W),                one_width(W)).
instr_operand_width(pzi_gt_u(W),                one_width(W)).
instr_operand_width(pzi_gt_s(W),                one_width(W)).
instr_operand_width(pzi_eq(W),                  one_width(W)).
instr_operand_width(pzi_not(W),                 one_width(W)).
instr_operand_width(pzi_drop,                   no_width).
instr_operand_width(pzi_roll(_),                no_width).
instr_operand_width(pzi_pick(_),                no_width).
instr_operand_width(pzi_call(_),                no_width).
instr_operand_width(pzi_cjmp(_, W),             one_width(W)).
instr_operand_width(pzi_ret,                    no_width).

%-----------------------------------------------------------------------%

pzi_dup = pzi_pick(1).

pzi_swap = pzi_roll(2).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
