%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module context.
%
% A location in a source file
%
% Copyright (C) 2015-2016 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
%-----------------------------------------------------------------------%

:- interface.

:- import_module int.
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

:- func context_string(context) = string.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

:- import_module list.

%-----------------------------------------------------------------------%

nil_context = context("", 0, 0).

%-----------------------------------------------------------------------%

% We do not print the character information, I'm pretty sure that they're
% inaccuruate because whitespace is not included in their calculation (see
% the tokenize and tokenize_line predicates).  But we still store them to
% make comparing contexts feasable.
context_string(context(File, Line, _)) =
    format("%s:%d", [s(File), i(Line)]).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
