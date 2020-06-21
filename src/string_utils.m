%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module string_utils.
%
% String manipulation utils.
%
% Copyright (C) 2016, 2020 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
% Routines for escaping and unescaping strings.
%
%-----------------------------------------------------------------------%
:- interface.

%-----------------------------------------------------------------------%

:- func escape_string(string) = string.

:- func unescape_string(string) = string.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
:- implementation.

:- import_module char.
:- import_module int.
:- import_module list.
:- import_module string.

:- import_module util.
:- import_module util.exception.

%-----------------------------------------------------------------------%

escape_string(String) = from_char_list(['"'] ++
    escape_chars(to_char_list(String), []) ++ ['"']).

:- func escape_chars(list(char), list(char)) = list(char).

escape_chars([], Done) = reverse(Done).
escape_chars([C0 | Cs], Done0) = Done :-
    ( if escape_char(C, C0) then
        Done1 = [C, '\\' | Done0]
    else if C0 = ('\"') then
        Done1 = ['"', '\\' | Done0]
    else
        Done1 = [C0 | Done0]
    ),
    Done = escape_chars(Cs, Done1).

unescape_string(S0) = from_char_list(C) :-
    between(S0, 1, length(S0) - 1, S1),
    C1 = to_char_list(S1),
    unescape_string_loop(C1) = C.

:- func unescape_string_loop(list(char)) = list(char).

unescape_string_loop([]) = [].
unescape_string_loop([C | Cs0]) = Cs :-
    ( if C = ('\\') then
        Cs = unescape_string_loop_do_escape(Cs0)
    else
        Cs = [C | unescape_string_loop(Cs0)]
    ).

:- func unescape_string_loop_do_escape(list(char)) = list(char).

unescape_string_loop_do_escape([]) =
    util.exception.sorry($file, $pred,
        "Lexer does not support escaping the double quote").
unescape_string_loop_do_escape([C0 | Cs0]) = Cs :-
    ( if escape_char(C0, C1) then
        C = C1
    else
        % Interpret the escaped character as if it was not escaped.
        C = C0
    ),
    Cs = [C | unescape_string_loop(Cs0)].

:- pred escape_char(char, char).
:- mode escape_char(in, out) is semidet.
:- mode escape_char(out, in) is semidet.

escape_char('n', '\n').
escape_char('r', '\r').
escape_char('t', '\t').
escape_char('v', '\v').
escape_char('f', '\f').
escape_char('b', '\b').
escape_char('\\', '\\').

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
