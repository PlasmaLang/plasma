%-----------------------------------------------------------------------%
% Plasma parser
% vim: ts=4 sw=4 et
%
% Copyright (C) 2015 Paul Bone
% Distributed under the terms of the GPLv2 see ../LICENSE.tools
%
% This program compiles plasma modules.
%
%-----------------------------------------------------------------------%
:- module parse.
%-----------------------------------------------------------------------%

:- interface.

:- import_module io.
:- import_module string.

:- import_module result.

:- import_module ast.

%-----------------------------------------------------------------------%

:- type parse_error.

:- instance error(parse_error).

%-----------------------------------------------------------------------%

:- pred parse(string::in, result(plasma_ast, parse_error)::out,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

:- import_module require.

%-----------------------------------------------------------------------%

:- type parse_error
    ---> parse_error.

:- instance error(parse_error) where [
    error_or_warning(_) = error,
    to_string(_) = "error"
].

%-----------------------------------------------------------------------%

parse(_, _, !IO) :-
    error("unimplemented").

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
