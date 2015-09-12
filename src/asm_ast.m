%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module asm_ast.
%
% AST for PZ Textual representation.
%
% Copyright (C) 2015 Paul Bone
% Distributed under the terms of the GPLv2 see ../LICENSE.tools
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
                asmp_insts      :: list(pzt_instruction)
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

:- type pzt_instruction
    --->    pzt_instruction(
                pzti_instr      :: pzt_instruction_code,
                pzti_context    :: context
            ).

:- type pzt_instruction_code
    --->    pzti_load_immediate(int)
    ;       pzti_word(symbol).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

:- import_module result.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
