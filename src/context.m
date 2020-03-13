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

%-----------------------------------------------------------------------%

:- func nil_context = context.
:- pred is_nil_context(context::in) is semidet.

:- func context_string(context) = string.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

:- import_module list.

%-----------------------------------------------------------------------%

nil_context = context("", 0, 0).

is_nil_context(context("", _, _)).

%-----------------------------------------------------------------------%

% We do not print the character information, I'm pretty sure that they're
% inaccurate because whitespace is not included in their calculation (see
% the tokenize and tokenize_line predicates).  But we still store them to
% make comparing contexts feasible.
context_string(context(File, Line, _)) =
    format("%s:%d", [s(File), i(Line)]).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
