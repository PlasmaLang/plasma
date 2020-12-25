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
    ;       tv_array(list(toml_value))
    ;       tv_table(toml).

:- pred parse_toml(input_stream::in, maybe_error(toml)::out,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module string.

:- import_module util.
:- import_module util.exception.

%-----------------------------------------------------------------------%

parse_toml(Stream, Result, !IO) :-
    parse_lines(Stream, 1, no_table, init, Result, !IO).

:- type parse_table
    --->    no_table
    ;       table(string, toml).

:- pred parse_lines(input_stream::in, int::in, parse_table::in, toml::in,
    maybe_error(toml)::out, io::di, io::uo) is det.

parse_lines(Stream, LineNum, !.Table, !.Toml, Result, !IO) :-
    read_line_as_string(Stream, LineRes, !IO),
    ( LineRes = ok(Line0),
        Line = strip(strip_comment(Line0)),
        ( if
            Line = ""
        then
            parse_lines(Stream, LineNum + 1, !.Table, !.Toml, Result, !IO)
        else if
            % The beginning of a table.
            remove_prefix("[", Line, Name0),
            remove_suffix(Name0, "]", Name)
        then
            end_table(!.Table, !.Toml, Result0),
            ( Result0 = ok(!:Toml),
                parse_lines(Stream, LineNum + 1, table(strip(Name), init),
                    !.Toml, Result, !IO)
            ; Result0 = error(_),
                Result = Result0
            )
        else if
            [LHS, RHS] = split_at_char('=', Line),
            LHS \= "",
            RHS \= ""
        then
            Value = parse_value(strip(RHS)),
            ( if toml_insert(strip(LHS), Value, !Table, !Toml) then
                parse_lines(Stream, LineNum + 1, !.Table, !.Toml, Result, !IO)
            else
                Result = error(
                    format("Duplicate key `%s`", [s(strip(LHS))]))
            )
        else
            Result = error(format("Syntax error (line %d)", [i(LineNum)]))
        )
    ; LineRes = eof,
        end_table(!.Table, !.Toml, Result)
    ; LineRes = error(Error),
        Result = error(error_message(Error))
    ).

:- pred toml_insert(toml_key::in, toml_value::in,
    parse_table::in, parse_table::out, toml::in, toml::out) is semidet.

toml_insert(Key, Value, no_table, no_table, !Toml) :-
    insert(Key, Value, !Toml).
toml_insert(Key, Value, table(Name, Table0), table(Name, Table), !Toml) :-
    insert(Key, Value, Table0, Table).

:- pred end_table(parse_table::in, toml::in, maybe_error(toml)::out) is det.

end_table(no_table, Toml, ok(Toml)).
end_table(table(Name, Table), !.Toml, Result) :-
    ( if insert(Name, tv_table(Table), !Toml) then
        Result = ok(!.Toml)
    else
        Result = error("Duplidate table: " ++ Name)
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
