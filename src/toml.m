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
:- import_module list.
:- import_module map.
:- import_module maybe.

%-----------------------------------------------------------------------%

:- type toml == map(toml_key, toml_value).

:- type toml_key == string.
:- type toml_value
    --->    tv_string(string)
    ;       tv_array(list(toml_value)).

:- pred parse_toml(input_stream::in, maybe_error(toml)::out,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

:- import_module string.

:- import_module util.
:- import_module util.exception.

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
            Value = parse_value(strip(RHS)),
            ( if insert(strip(LHS), Value, !Toml) then
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

:- func parse_value(string) = toml_value.

parse_value(String) = Value :-
    ( if
        append("[", Rest0, String),
        append(Rest, "]", Rest0)
    then
        ( if contains_char(Rest, '[') then
            % We actually need a proper parser here to do nested structures.
            % So we don't support nested lists.
            util.exception.sorry($file, $pred,
                "Nested arrays")
        else
            Value = tv_array(map(parse_value, map(strip,
                split_at_char(',', Rest))))
        )
    else
        Value = tv_string(String)
    ).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
