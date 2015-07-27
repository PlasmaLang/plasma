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
:- import_module string.

:- import_module context.
:- import_module pz.

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
                asme_name       :: string,
                asme_context    :: context,
                asme_type       :: entry_type
            ).

    % There are currently two entry types.
    %
:- type entry_type
    --->    asm_proc(
                asmp_sig        :: pzt_signature,
                asmp_insts      :: list(pzt_instruction)
            )
    ;       asm_data(
                asmd_type       :: pz_data_type,
                asmd_value      :: pz_value
            ).

%-----------------------------------------------------------------------%
%
% Procedures
%

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
:- type pzt_signature
    --->    pzt_signature(
                pzts_before     :: list(pz_data_width),
                pzts_after      :: list(pz_data_width)
            ).

:- type pzt_instruction
    --->    pzti_load_immediate(int)
    ;       pzti_word(string).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

:- import_module result.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
