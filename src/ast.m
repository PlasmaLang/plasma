%-----------------------------------------------------------------------%
% Plasma AST
% vim: ts=4 sw=4 et
%
% Copyright (C) 2015 Paul Bone
% Distributed under the terms of the GPLv2 see ../LICENSE.tools
%
% This program compiles plasma modules.
%
%-----------------------------------------------------------------------%
:- module ast.
%-----------------------------------------------------------------------%

:- interface.

:- import_module list.
:- import_module maybe.
:- import_module string.

:- import_module symtab.

:- type plasma_ast
    --->    plasma_ast(
                pa_module_name      :: string,
                pa_maybe_exports    :: maybe(list(string)),
                pa_entries          :: list(past_entry)
            ).

:- type past_entry
    --->    past_import(
                pai_name            :: symbol
            )
    ;       past_function(
                paf_name            :: symbol,
                paf_signature       :: past_signature,
                paf_body            :: list(past_statement)
            ).

:- type past_signature
    --->    past_signature(
                pas_args            :: list(past_arg),
                pas_return          :: list(past_arg),
                pas_usage           :: list(past_usage)
            ).

:- type past_arg
    --->    past_arg.

:- type past_usage
    --->    past_usage.

:- type past_statement
    --->    past_statement.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

%-----------------------------------------------------------------------%


%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
