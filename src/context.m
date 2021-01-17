%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module context.
%
% A location in a source file
%
% Copyright (C) 2015-2016, 2019-2020 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
%-----------------------------------------------------------------------%

:- interface.

:- import_module string.

%-----------------------------------------------------------------------%

:- type context
    --->    context(
                c_file          :: string,
                c_line          :: int,
                c_col           :: int
            ).

:- func context(string) = context.

:- func context(string, int) = context.

%-----------------------------------------------------------------------%

:- func nil_context = context.
:- pred is_nil_context(context::in) is semidet.

:- func builtin_context = context.

:- func command_line_context = context.

:- func context_string(context) = string.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

:- import_module list.

%-----------------------------------------------------------------------%

context(Name) = context(Name, 0, 0).

context(Name, Line) = context(Name, Line, 0).

%-----------------------------------------------------------------------%

nil_context = context("").

is_nil_context(context("", _, _)).

builtin_context = context("builtin").

command_line_context = context("Command line").

%-----------------------------------------------------------------------%

% We do not print the character information, I'm pretty sure that they're
% inaccurate because whitespace is not included in their calculation (see
% the tokenize and tokenize_line predicates).  But we still store them to
% make comparing contexts feasible.
context_string(context(File, Line, _)) =
    ( if Line = 0 then
        File
    else
        format("%s:%d", [s(File), i(Line)])
    ).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
