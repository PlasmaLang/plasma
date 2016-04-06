%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module asm_ast.
%
% AST for PZ Textual representation.
%
% Copyright (C) 2015 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
%-----------------------------------------------------------------------%

:- interface.

:- import_module list.

:- import_module context.
:- import_module pz.
:- import_module pz.code.
:- import_module symtab.

%-----------------------------------------------------------------------%

:- type asm
    --->    asm(
                asm_entries     :: asm_entries
            ).

:- type asm_entries
    == list(asm_entry).

    % Everything is defined at the same "global entry" level in the same
    % namespace: a procedure and some static data cannot have the same name.
    % When that name is used we can decide what to do depending on the entry
    % type.
    %
    % Visibility rules will be added later.
    %
:- type asm_entry
    --->    asm_entry(
                asme_name       :: symbol,
                asme_context    :: context,
                asme_type       :: entry_type
            ).

    % There are currently two entry types.
    %
:- type entry_type
            % A procedure
    --->    asm_proc(
                asmp_sig        :: pz_signature,
                asmp_blocks     :: list(pzt_block)
            )
            % A procedure declaration.
    ;       asm_proc_decl(
                asmpd_sig       :: pz_signature
            )
            % Global data
    ;       asm_data(
                asmd_type       :: pz_data_type,
                asmd_value      :: pz_data_value
            ).

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
    --->    pzti_word(symbol)

            % These instructions are handled specifically because the have
            % immediate values.
    ;       pzti_load_immediate(int)
    ;       pzti_cjmp(string)
    ;       pzti_roll(int)
    ;       pzti_pick(int).

:- type pzt_instruction_widths
    --->    no
    ;       one_width(pz_data_width)
    ;       two_widths(pz_data_width, pz_data_width).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
