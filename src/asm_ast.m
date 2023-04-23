%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module asm_ast.
%
% AST for PZ Textual representation.
%
% Copyright (C) Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
%-----------------------------------------------------------------------%

:- interface.

:- import_module list.

:- import_module common_types.
:- import_module context.
:- import_module pz.
:- import_module pz.code.
:- import_module q_name.

%-----------------------------------------------------------------------%

:- type asm
    --->    asm(
                asm_module      :: q_name,
                asm_filename    :: string,
                asm_items       :: asm_items
            ).

:- type asm_items
    == list(asm_item).

    % Everything is defined at the same "global entry" level in the same
    % namespace: a procedure and some static data cannot have the same name.
    % When that name is used we can decide what to do depending on the entry
    % type.
    %
    % Visibility rules will be added later.
    %
:- type asm_item
    --->    asm_item(
                asmi_name       :: q_name,
                asmi_context    :: context,
                asmi_type       :: entry_type
            )
    ;       asm_entrypoint(
                asme_context   :: context,
                asme_name      :: q_name
            ).

    % There are currently two entry types.
    %
:- type entry_type
            % A procedure
    --->    asm_proc(
                asmp_sig        :: pz_signature,
                asmp_blocks     :: list(pzt_block)
            )
            % A procedure import
    ;       asm_import(
                asmpd_sig       :: pz_signature
            )
            % A structure
    ;       asm_struct(
                asms_fields     :: list(pz_width)
            )
            % Global data
    ;       asm_data(
                asmd_type       :: asm_data_type,
                asmd_value      :: list(asm_data_value)
            )
    ;       asm_closure(
                asmc_proc       :: string,
                asmc_data       :: string,
                asmc_sharing    :: sharing
            ).

:- type asm_data_type
    --->    asm_dtype_array(pz_width)
            % Note that this is a string and it is not possible to refer to
            % structs in other modules.
    ;       asm_dtype_struct(string)
    ;       asm_dtype_string.

:- type asm_data_value
    --->    asm_dvalue_num(int)
    ;       asm_dvalue_name(q_name).

%-----------------------------------------------------------------------%
%
% Procedures
%

:- type pzt_block
    --->    pzt_block(
                pztb_name       :: string,
                pztb_instrs     :: list(pzt_instruction),
                pztb_context    :: context
            ).

:- type pzt_instruction
    --->    pzt_instruction(
                pzti_instr          :: pzt_instruction_code,
                pzti_widths         :: pzt_instruction_widths,
                pzti_context        :: context
            ).

    % Instructions such as "add" although not really implemented as calls in
    % the runtime, look like calls in this structure.  They use pzti_word.
    % Instructions that require special handling by the parser are handled
    % specifically.
    %
:- type pzt_instruction_code
    --->    pzti_word(string)

            % Call instructions are handled specifically because it'll be
            % easier when we introduce tail calls.
    ;       pzti_call(q_name)
    ;       pzti_tcall(q_name)

            % These instructions are handled specifically because the have
            % immediate values.
    ;       pzti_load_immediate(int)
    ;       pzti_jmp(string)
    ;       pzti_cjmp(string)
    ;       pzti_roll(int)
    ;       pzti_pick(int)
    ;       pzti_alloc(string)
    ;       pzti_make_closure(q_name)
    ;       pzti_load(string, field_num)
    ;       pzti_store(string, field_num).

:- type pzt_instruction_widths
    --->    no
    ;       one_width(pz_width)
    ;       two_widths(pz_width, pz_width).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
