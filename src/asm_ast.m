%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module asm_ast.
%
% AST for PZ Textual representation.
%
% Copyright (C) 2015 Paul Bone
% All rights reserved
%
%-----------------------------------------------------------------------%

:- interface.

:- import_module list.
:- import_module string.

:- import_module context.

%-----------------------------------------------------------------------%

:- type asm
    --->    asm(
                asm_entries     :: asm_entries
            ).

:- type asm_entries
    == list(asm_entry).

:- type asm_entry
    --->    asm_entry(
                asme_name       :: string,
                asme_context    :: context,
                asme_type       :: entry_type
            ).

:- type entry_type
    --->    asm_proc(
                asme_insts      :: list(pzt_instruction)
            ).

%-----------------------------------------------------------------------%

:- type pzt_instruction
    --->    pzti_load_immediate(pzt_value)
    ;       pzti_call(string).

:- type pzt_value
    --->    pztv_string(string).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

:- import_module result.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
