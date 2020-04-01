%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module pz.code.
%
% PZ representation of code.
%
% Copyright (C) 2015-2020 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
%-----------------------------------------------------------------------%
:- interface.

:- import_module list.

:- import_module q_name.
:- import_module context.
:- import_module common_types.

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
                pzs_before      :: list(pz_width),
                pzs_after       :: list(pz_width)
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
    ;       pzio_context(
                pzio_context        :: pz_context
            )
    ;       pzio_comment(
                pzio_comment        :: string
            ).

:- type pz_instr
    --->    pzi_load_immediate(pz_width, immediate_value)
    ;       pzi_ze(pz_width, pz_width)
    ;       pzi_se(pz_width, pz_width)
    ;       pzi_trunc(pz_width, pz_width)
    ;       pzi_add(pz_width)
    ;       pzi_sub(pz_width)
    ;       pzi_mul(pz_width)
    ;       pzi_div(pz_width)
    ;       pzi_mod(pz_width)
    ;       pzi_lshift(pz_width)
    ;       pzi_rshift(pz_width)
    ;       pzi_and(pz_width)
    ;       pzi_or(pz_width)
    ;       pzi_xor(pz_width)
    ;       pzi_lt_u(pz_width)
    ;       pzi_lt_s(pz_width)
    ;       pzi_gt_u(pz_width)
    ;       pzi_gt_s(pz_width)
    ;       pzi_eq(pz_width)
    ;       pzi_not(pz_width)
    ;       pzi_drop

            % Roll the top N items on the stack shifting them toward the
            % left, the deepest item becomes the TOS and all
            % other items shift along one space deeper (to the left).
            % roll 1 is a no-op, roll 2 is "swap".
    ;       pzi_roll(int)
    ;       pzi_pick(int)
    ;       pzi_call(pz_callee)
    ;       pzi_tcall(pz_callee)
    ;       pzi_call_ind
    ;       pzi_tcall_ind
    ;       pzi_cjmp(pzb_id, pz_width)
    ;       pzi_jmp(pzb_id)
    ;       pzi_ret

    ;       pzi_alloc(pzs_id)
    ;       pzi_make_closure(pzp_id)
    ;       pzi_load(pzs_id, field_num, pz_width)
    ;       pzi_load_named(pzi_id, pz_width)
    ;       pzi_store(pzs_id, field_num, pz_width)
    ;       pzi_get_env.

:- type pz_callee
    --->    pzc_closure(pzc_id)
    ;       pzc_import(pzi_id)
            % Being able to refer to a proc directly is an optimisation.
    ;       pzc_proc_opt(pzp_id).

    % This type represents the kinds of immediate value that can be loaded
    % onto the stack via the pzi_load_immediate instruction.  The related
    % type pz_immediate_value is more comprehensive and covers intermediate
    % values within the instruction stream, such as labels and stack depths.
    %
:- type immediate_value
    --->    im_i8(int8)
    ;       im_u8(uint8)
    ;       im_i16(int16)
    ;       im_u16(uint16)
    ;       im_i32(int32)
    ;       im_u32(uint32)
    ;       im_i64(int64)
    ;       im_u64(uint64).

:- type pz_context
    --->    pz_context(
                pzic_context        :: context,
                pzic_file_data      :: pzd_id
            )
    ;       pz_context_short(
                pzics_line          :: int
            )
    ;       pz_nil_context.

:- type maybe_operand_width
    --->    one_width(pz_width)
    ;       two_widths(pz_width, pz_width)
    ;       no_width.

    % Block ID
    %
:- type pzb_id == uint32.

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

%-----------------------------------------------------------------------%

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
instr_operand_width(pzi_tcall(_),               no_width).
instr_operand_width(pzi_call_ind,               no_width).
instr_operand_width(pzi_tcall_ind,              no_width).
instr_operand_width(pzi_cjmp(_, W),             one_width(W)).
instr_operand_width(pzi_jmp(_),                 no_width).
instr_operand_width(pzi_ret,                    no_width).
instr_operand_width(pzi_alloc(_),               no_width).
instr_operand_width(pzi_make_closure(_),        no_width).
instr_operand_width(pzi_load(_, _, W),          one_width(W)).
instr_operand_width(pzi_load_named(_, W),       one_width(W)).
instr_operand_width(pzi_store(_, _, W),         one_width(W)).
instr_operand_width(pzi_get_env,                no_width).

%-----------------------------------------------------------------------%

pzi_dup = pzi_pick(1).

pzi_swap = pzi_roll(2).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
