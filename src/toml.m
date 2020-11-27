%-----------------------------------------------------------------------%
% Plasma builder
% vim: ts=4 sw=4 et
%
% Copyright (C) 2020 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
% This library parses a reduced version of toml syntax.
%
%-----------------------------------------------------------------------%
:- module toml.
%-----------------------------------------------------------------------%

:- interface.

:- import_module io.
:- import_module map.
:- import_module maybe.

%-----------------------------------------------------------------------%

:- type toml == map(toml_key, toml_value).

:- type toml_key == string.
:- type toml_value
    ---> tv_string(string).

:- pred parse_toml(input_stream::in, maybe_error(toml)::out,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module string.

%-----------------------------------------------------------------------%

parse_toml(Stream, Result, !IO) :-
    parse_lines(Stream, init, Result, !IO).

:- pred parse_lines(input_stream::in, toml::in,
    maybe_error(toml)::out, io::di, io::uo) is det.

parse_lines(Stream, !.Toml, Result, !IO) :-
    read_line_as_string(Stream, LineRes, !IO),
    ( LineRes = ok(Line0),
        Line = strip(strip_comment(Line0)),
        ( if
            Line = ""
        then
            parse_lines(Stream, !.Toml, Result, !IO)
        else if
            [LHS, RHS] = split_at_char('=', Line),
            LHS \= "",
            RHS \= ""
        then
            ( if insert(strip(LHS), tv_string(strip(RHS)), !Toml) then
                parse_lines(Stream, !.Toml, Result, !IO)
            else
                Result = error(
                    format("Duplicate key `%s`", [s(strip(LHS))]))
            )
        else
            Result = error("Syntax error")
        )
    ; LineRes = eof,
        Result = ok(!.Toml)
    ; LineRes = error(Error),
        Result = error(error_message(Error))
    ).

:- func strip_comment(string) = string.

strip_comment(Line) =
    left(Line, prefix_length(
        pred(C::in) is semidet :- C \= '#', Line)).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
