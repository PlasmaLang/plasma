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
:- import_module pair.

:- import_module context.
:- import_module util.
:- import_module util.result.

%-----------------------------------------------------------------------%

:- type toml == map(toml_key, pair(toml_value, context)).

:- type toml_key == string.
:- type toml_value
    --->    tv_string(string)
    ;       tv_array(list(toml_value))
    ;       tv_table(toml).

:- pred parse_toml(input_stream::in, string::in, result(toml, string)::out,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module string.

:- import_module util.exception.

%-----------------------------------------------------------------------%

parse_toml(Stream, Filename, Result, !IO) :-
    parse_lines(Stream, Filename, 1, no_table, init, Result, !IO).

:- type parse_table
    --->    no_table
    ;       table(
                pt_context      :: context,
                pt_name         :: string,
                pt_map          :: toml
            ).

:- pred parse_lines(input_stream::in, string::in, int::in, parse_table::in,
    toml::in, result(toml, string)::out, io::di, io::uo) is det.

parse_lines(Stream, Filename, LineNum, !.Table, !.Toml, Result, !IO) :-
    Context = context(Filename, LineNum),
    read_line_as_string(Stream, LineRes, !IO),
    ( LineRes = ok(Line0),
        Line = strip(strip_comment(Line0)),
        ( if
            Line = ""
        then
            parse_lines(Stream, Filename, LineNum + 1, !.Table,
                !.Toml, Result, !IO)
        else if
            % The beginning of a table.
            remove_prefix("[", Line, Name0),
            remove_suffix(Name0, "]", Name)
        then
            end_table(!.Table, !.Toml, Result0),
            ( Result0 = ok(!:Toml),
                parse_lines(Stream, Filename, LineNum + 1,
                    table(Context, strip(Name), init), !.Toml, Result, !IO)
            ; Result0 = errors(_),
                Result = Result0
            )
        else if
            [LHS, RHS] = split_at_char('=', Line),
            LHS \= "",
            RHS \= ""
        then
            Value = parse_value(strip(RHS)),
            ( if toml_insert(strip(LHS), Value, Context, !Table, !Toml) then
                parse_lines(Stream, Filename, LineNum + 1, !.Table,
                    !.Toml, Result, !IO)
            else
                Result = return_error(Context,
                    format("Duplicate key `%s`", [s(strip(LHS))]))
            )
        else
            Result = return_error(Context, "Unrecognised TOML line")
        )
    ; LineRes = eof,
        end_table(!.Table, !.Toml, Result)
    ; LineRes = error(Error),
        Result = return_error(context(Filename), error_message(Error))
    ).

:- pred toml_insert(toml_key::in, toml_value::in, context::in,
    parse_table::in, parse_table::out, toml::in, toml::out) is semidet.

toml_insert(Key, Value, Context, no_table, no_table, !Toml) :-
    insert(Key, Value - Context, !Toml).
toml_insert(Key, Value, Context, !Table, !Toml) :-
    insert(Key, Value - Context, !.Table ^ pt_map, Map),
    !Table ^ pt_map := Map.

:- pred end_table(parse_table::in, toml::in,
    result(toml, string)::out) is det.

end_table(no_table,                    Toml,   ok(Toml)).
end_table(table(Context, Name, Table), !.Toml, Result) :-
    ( if insert(Name, tv_table(Table) - Context, !Toml) then
        Result = ok(!.Toml)
    else
        Result = return_error(Context, "Duplidate table: " ++ Name)
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
