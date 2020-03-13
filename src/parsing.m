%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module parsing.
%
% Parsing utils.
%
% Copyright (C) 2015, 2019-2020 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
%-----------------------------------------------------------------------%

:- interface.

:- import_module list.
:- import_module maybe.
:- import_module unit.

:- import_module context.

%-----------------------------------------------------------------------%

:- type token(T)
    --->    token(
                t_terminal      :: T,
                t_data          :: string,
                t_context       :: context
            ).

:- type parse_res(R)
    --->    ok(R)
    ;       error(context, pe_got :: string, pe_expect :: string).

:- inst res_ok for parse_res/1
    --->    ok(ground).

:- inst res_error for parse_res/1
    --->    error(ground, ground, ground).

:- type parser(R, T) == pred(parse_res(R), list(token(T)), list(token(T))).
:- inst parser == ( pred(out, in, out) is det ).

:- type parser_last_error(R, T) == pred(parse_res(R), parse_res(unit),
    list(token(T)), list(token(T))).
:- inst parser_last_error == ( pred(out, out(res_error), in, out) is det ).

%-----------------------------------------------------------------------%

:- pred optional(parser(R, T)::in(parser),
    parse_res(maybe(R))::out(res_ok),
    list(token(T))::in, list(token(T))::out) is det.

:- pred optional_last_error(parser(R, T)::in(parser),
    parse_res(maybe(R))::out(res_ok),
    parse_res(unit)::out(res_error),
    list(token(T))::in, list(token(T))::out) is det.

:- pred zero_or_more(parser(R, T)::in(parser),
    parse_res(list(R))::out(res_ok),
    list(token(T))::in, list(token(T))::out) is det.

:- pred zero_or_more_last_error(parser(R, T)::in(parser),
    parse_res(list(R))::out(res_ok),
    parse_res(unit)::out(res_error),
    list(token(T))::in, list(token(T))::out) is det.

:- pred zero_or_more_delimited(T::in, parser(R, T)::in(parser),
    parse_res(list(R))::out(res_ok),
    list(token(T))::in, list(token(T))::out) is det.

:- pred one_or_more(parser(R, T)::in(parser),
    parse_res(list(R))::out,
    list(token(T))::in, list(token(T))::out) is det.

:- pred one_or_more_last_error(parser(R, T)::in(parser),
    parse_res(list(R))::out,
    parse_res(unit)::out(res_error),
    list(token(T))::in, list(token(T))::out) is det.

:- pred one_or_more_delimited(T::in, parser(R, T)::in(parser),
    parse_res(list(R))::out,
    list(token(T))::in, list(token(T))::out) is det.

:- pred or(list(parser(R, T))::in(list(parser)), parse_res(R)::out,
    list(token(T))::in, list(token(T))::out) is det.

:- pred within(T::in, parser(R, T)::in(parser), T::in,
    parse_res(R)::out, list(token(T))::in, list(token(T))::out) is det.

:- pred within_use_last_error(T::in,
    parser_last_error(R, T)::in(parser_last_error),
    T::in, parse_res(R)::out, list(token(T))::in, list(token(T))::out) is det.

%-----------------------------------------------------------------------%

:- pred parse_map(func(A) = B, parser(A, T), parse_res(B),
    list(token(T)), list(token(T))).
:- mode parse_map(func(in) = out is det, in(parser), out, in, out) is det.

%-----------------------------------------------------------------------%

:- func combine_errors_2(parse_res(R1), parse_res(R2)) = parse_res(R).

:- func combine_errors_3(parse_res(R1), parse_res(R2), parse_res(R3)) =
    parse_res(R).

:- func combine_errors_4(parse_res(R1), parse_res(R2), parse_res(R3),
        parse_res(R4)) =
    parse_res(R).

:- func combine_errors_5(parse_res(R1), parse_res(R2), parse_res(R3),
        parse_res(R4), parse_res(R5)) =
    parse_res(R).

:- func combine_errors_6(parse_res(R1), parse_res(R2), parse_res(R3),
        parse_res(R4), parse_res(R5), parse_res(R6)) =
    parse_res(R).

:- func latest_error(parse_res(R1), parse_res(R2)) = parse_res(R).
:- mode latest_error(in, in(res_error)) = out(res_error) is det.
:- mode latest_error(in(res_error), in) = out(res_error) is det.

:- func map(func(X) = Y, parse_res(X)) = parse_res(Y).

%-----------------------------------------------------------------------%

:- pred match_token(T::in, parse_res(string)::out,
    list(token(T))::in, list(token(T))::out) is det.

:- pred match_tokens(list(T)::in, parse_res(unit)::out,
    list(token(T))::in, list(token(T))::out) is det.

:- type token_and_string(T)
    --->    token_and_string(T, string).

:- pred next_token(string::in, parse_res(token_and_string(T))::out,
    list(token(T))::in, list(token(T))::out) is det.

:- pred peek_token(list(token(T))::in, maybe(T)::out) is det.

%-----------------------------------------------------------------------%

:- pred get_context(list(token(T))::in, context::out) is det.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

:- import_module require.
:- import_module string.

%-----------------------------------------------------------------------%

optional(Parse, Result, !Tokens) :-
    optional_last_error(Parse, Result, _, !Tokens).

optional_last_error(Parse, Result, LastError, !Tokens) :-
    StartTokens = !.Tokens,
    Parse(Result0, !Tokens),
    ( Result0 = ok(X),
        Result = ok(yes(X)),
        LastError = error(nil_context, "", "")
    ; Result0 = error(C, G, E),
        !:Tokens = StartTokens,
        Result = ok(no),
        LastError = error(C, G, E)
    ).

%-----------------------------------------------------------------------%

zero_or_more(Parse, Result, !Tokens) :-
    zero_or_more_last_error(Parse, Result, _, !Tokens).

zero_or_more_last_error(Parse, Result, LastError, !Tokens) :-
    StartTokens = !.Tokens,
    Parse(ResultX, !Tokens),
    ( ResultX = ok(X),
        zero_or_more_last_error(Parse, ResultXS, LastError, !Tokens),
        ResultXS = ok(XS),
        Result = ok([X | XS])
    ; ResultX = error(C, G, E),
        !:Tokens = StartTokens,
        Result = ok([]),
        LastError = error(C, G, E)
    ).

zero_or_more_delimited(Delim, Parse, Result, !Tokens) :-
    Parse(ResultX, !Tokens),
    ( ResultX = ok(X),
        delimited_list(Delim, Parse, ok(Xs), !Tokens),
        Result = ok([X | Xs])
    ; ResultX = error(_, _, _),
        Result = ok([])
    ).

one_or_more(Parse, Result, !Tokens) :-
    StartTokens = !.Tokens,
    Parse(ResultX, !Tokens),
    ( ResultX = ok(X),
        zero_or_more(Parse, ok(XS), !Tokens),
        Result = ok([X | XS])
    ; ResultX = error(C, G, E),
        !:Tokens = StartTokens,
        Result = error(C, G, E)
    ).

one_or_more_last_error(Parse, Result, LastError, !Tokens) :-
    StartTokens = !.Tokens,
    Parse(ResultX, !Tokens),
    ( ResultX = ok(X),
        zero_or_more_last_error(Parse, ResultXS, LastError, !Tokens),
        ResultXS = ok(XS),
        Result = ok([X | XS])
    ; ResultX = error(C, G, E),
        !:Tokens = StartTokens,
        Result = error(C, G, E),
        LastError = error(C, G, E)
    ).

one_or_more_delimited(Delim, Parse, Result, !Tokens) :-
    StartTokens = !.Tokens,
    Parse(ResultX, !Tokens),
    ( ResultX = ok(X),
        delimited_list(Delim, Parse, ok(XS), !Tokens),
        Result = ok([X | XS])
    ; ResultX = error(C, G, E),
        !:Tokens = StartTokens,
        Result = error(C, G, E)
    ).

:- pred delimited_list(T::in, parser(R, T)::in(parser),
    parse_res(list(R))::out(res_ok),
    list(token(T))::in, list(token(T))::out) is det.

delimited_list(Delim, Parse, Result, !Tokens) :-
    StartTokens = !.Tokens,
    match_token(Delim, DelimMatch, !Tokens),
    Parse(ResultX, !Tokens),
    ( if
        DelimMatch = ok(_),
        ResultX = ok(X)
    then
        delimited_list(Delim, Parse, ok(Xs), !Tokens),
        Result = ok([X | Xs])
    else
        !:Tokens = StartTokens,
        Result = ok([])
    ).

%-----------------------------------------------------------------------%

or(Parsers, Result, !Tokens) :-
    or_loop(error(nil_context, "", ""), Parsers, Result, !Tokens).

:- pred or_loop(parse_res(R)::in(res_error),
    list(parser(R, T))::in(list(parser)), parse_res(R)::out,
    list(token(T))::in, list(token(T))::out) is det.

or_loop(PrevError, [], PrevError, !Tokens).
or_loop(PrevError, [Parser | Parsers], Result, !Tokens) :-
    StartTokens = !.Tokens,
    Parser(Result0, !Tokens),
    ( Result0 = ok(_),
        Result = Result0
    ; Result0 = error(_, _, _),
        !:Tokens = StartTokens,
        NextError = latest_error(PrevError, Result0),
        or_loop(NextError, Parsers, Result, !Tokens)
    ).

within(Open, Parser, Close, Result, !Tokens) :-
    match_token(Open, OpenResult, !Tokens),
    ( OpenResult = ok(_),
        Parser(Result0, !Tokens),
        match_token(Close, CloseResult, !Tokens),
        ( if
            OpenResult = ok(_),
            Result0 = ok(X),
            CloseResult = ok(_)
        then
            Result = ok(X)
        else
            Result = combine_errors_2(Result0, CloseResult)
        )
    ; OpenResult = error(C, G, E),
        Result = error(C, G, E)
    ).

within_use_last_error(Open, Parser, Close, Result, !Tokens) :-
    match_token(Open, OpenResult, !Tokens),
    ( OpenResult = ok(_),
        Parser(Result0, LastError, !Tokens),
        match_token(Close, CloseResult, !Tokens),
        ( if
            OpenResult = ok(_),
            Result0 = ok(X),
            CloseResult = ok(_)
        then
            Result = ok(X)
        else
            Result = combine_errors_2(
                latest_error(Result0, LastError) `with_type` parse_res(unit),
                CloseResult)
        )
    ; OpenResult = error(C, G, E),
        Result = error(C, G, E)
    ).

%-----------------------------------------------------------------------%

parse_map(Map, Parser, Result, !Tokens) :-
    Parser(Result0, !Tokens),
    ( Result0 = ok(A),
        Result = ok(Map(A))
    ; Result0 = error(C, G, E),
        Result = error(C, G, E)
    ).

%-----------------------------------------------------------------------%

combine_errors_2(ok(_), ok(_)) = unexpected($pred, "not an error").
combine_errors_2(ok(_), error(C, G, E)) = error(C, G, E).
combine_errors_2(error(C, G, E), _) = error(C, G, E).

combine_errors_3(ok(_), ok(_), ok(_)) = unexpected($pred, "not an error").
combine_errors_3(ok(_), ok(_), error(C, G, E)) = error(C, G, E).
combine_errors_3(ok(_), error(C, G, E), _) = error(C, G, E).
combine_errors_3(error(C, G, E), _, _) = error(C, G, E).

combine_errors_4(ok(_), ok(_), ok(_), ok(_)) =
    unexpected($pred, "not an error").
combine_errors_4(ok(_), ok(_), ok(_), error(C, G, E)) = error(C, G, E).
combine_errors_4(ok(_), ok(_), error(C, G, E), _) = error(C, G, E).
combine_errors_4(ok(_), error(C, G, E), _, _) = error(C, G, E).
combine_errors_4(error(C, G, E), _, _, _) = error(C, G, E).

combine_errors_5(ok(_), ok(_), ok(_), ok(_), ok(_)) =
    unexpected($pred, "not an error").
combine_errors_5(ok(_), ok(_), ok(_), ok(_), error(C, G, E)) = error(C, G, E).
combine_errors_5(ok(_), ok(_), ok(_), error(C, G, E), _) = error(C, G, E).
combine_errors_5(ok(_), ok(_), error(C, G, E), _, _) = error(C, G, E).
combine_errors_5(ok(_), error(C, G, E), _, _, _) = error(C, G, E).
combine_errors_5(error(C, G, E), _, _, _, _) = error(C, G, E).

combine_errors_6(ok(_), ok(_), ok(_), ok(_), ok(_), ok(_)) =
    unexpected($pred, "not an error").
combine_errors_6(ok(_), ok(_), ok(_), ok(_), ok(_), error(C, G, E)) =
    error(C, G, E).
combine_errors_6(ok(_), ok(_), ok(_), ok(_), error(C, G, E), _) =
    error(C, G, E).
combine_errors_6(ok(_), ok(_), ok(_), error(C, G, E), _, _) = error(C, G, E).
combine_errors_6(ok(_), ok(_), error(C, G, E), _, _, _) = error(C, G, E).
combine_errors_6(ok(_), error(C, G, E), _, _, _, _) = error(C, G, E).
combine_errors_6(error(C, G, E), _, _, _, _, _) = error(C, G, E).

latest_error(ok(_), error(C, G, E)) = error(C, G, E).
latest_error(error(C, G, E), ok(_)) = error(C, G, E).
latest_error(error(C1, G1, E1), error(C2, G2, E2)) = Err :-
    compare(CR, C1, C2),
    (
        ( CR = (>)
        ; CR = (=)
        ),
        Err = error(C1, G1, E1)
    ;
        CR = (<),
        Err = error(C2, G2, E2)
    ).

map(M, ok(X)) = ok(M(X)).
map(_, error(C, G, E)) = error(C, G, E).

%-----------------------------------------------------------------------%

match_token(TA, error(nil_context, "EOF", string(TA)), [], []).
match_token(TA, Result, Ts0@[token(TB, String, Context) | Ts1], Ts) :-
    ( if TA = TB then
        Result = ok(String),
        Ts = Ts1
    else
        Result = error(Context, string(TB), string(TA)),
        Ts = Ts0
    ).

match_tokens([], ok(unit), !Tokens).
match_tokens([T|Ts], Result, !Tokens) :-
    match_token(T, Result0, !Tokens),
    ( Result0 = ok(_),
        match_tokens(Ts, Result, !Tokens)
    ; Result0 = error(C, G, E),
        Result = error(C, G, E)
    ).

next_token(Expect, error(nil_context, "EOF", Expect), [], []).
next_token(_, ok(token_and_string(Token, String)),
    [token(Token, String, _) | Tokens], Tokens).

peek_token([], no).
peek_token([token(Token, _, _) | _], yes(Token)).

%-----------------------------------------------------------------------%

get_context([], nil_context).
get_context([token(_, _, Context) | _], Context).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
