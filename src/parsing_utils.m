%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module parsing_utils.
%
% Parsing utils.
%
% Copyright (C) 2015 Paul Bone
% Distributed under the terms of the GPLv2 see ../LICENSE.tools
%
%-----------------------------------------------------------------------%

:- interface.

:- import_module list.
:- import_module maybe.
:- import_module string.
:- import_module unit.

:- import_module context.

%-----------------------------------------------------------------------%

:- type token(T)
    --->    token(T, context).

%-----------------------------------------------------------------------%

:- pred token_context(list(token(T))::in, context::out) is det.

%-----------------------------------------------------------------------%

    % Parsers are deterministic and return one of the three options.
    % no_match is inteded for looking ahead and error for returning parse
    % errors.
    %
:- type parse_result(X, T)
    --->    match(X, context)
    ;       no_match
    ;       error(parser_error(T), context).

:- inst match_or_error
    --->    match(ground, ground)
    ;       error(ground, ground).

:- inst match_or_nomatch
    --->    match(ground, ground)
    ;       no_match.

:- inst match
    --->    match(ground, ground).

:- type parser_error(T)
    --->    pe_unexpected_eof(string)
    ;       pe_unexpected_token(string, T)
    ;       pe_other(string).

    % The parser combinators below take parsers of this general form,
    %
:- type parser(T, X) ==
    pred(parse_result(X, T), list(token(T)), list(token(T))).
:- inst parser ==
    (pred(out, in, out) is det).
:- inst parser(I) ==
    (pred(out(I), in, out) is det).

%-----------------------------------------------------------------------%

    % match(T, Result, !Tokens),
    %
    % Read a token !Tokens test if it matches T.  If it does not then the
    % Result is no_match.
    %
:- pred match(T, parse_result(unit, T),
    list(token(T)), list(token(T))).
:- mode match(in, out(match_or_nomatch), in, out) is det.

    % match_ho(Pred, Result, !Tokens),
    %
    % Read a token that satisfies Pred from !Tokens.  If it does not then
    % the Result is no_match.
    %
:- pred match_ho(pred(T, X), parse_result(X, T),
    list(token(T)), list(token(T))).
:- mode match_ho(pred(in, out) is semidet, out(match_or_nomatch), in, out)
    is det.

    % consume(T, Result, !Tokens),
    %
    % Read a token T from !Tokens and raise an error if !Tokens is empty or
    % the next token does not match T.
    %
:- pred consume(T, parse_result(unit, T), list(token(T)), list(token(T))).
:- mode consume(in, out(match_or_error), in, out) is det.

    % consume_ho(Expected, Pred, Result, !Tokens),
    %
    % Read a token that satisfies Pred from !Tokens and raise an error if
    % !Tokens is empty or if Pred was not satisfied.
    %
:- pred consume_ho(string, pred(T, X), parse_result(X, T),
    list(token(T)), list(token(T))).
:- mode consume_ho(in, pred(in, out) is semidet, out(match_or_error),
    in, out) is det.

%-----------------------------------------------------------------------%

    % peek(T, Tokens) is true if tokens begins with T.
    %
:- pred peek(T::in, list(token(T))::in) is semidet.

%-----------------------------------------------------------------------%

    % brackets(Open, Close, ParseContents, Result, !Tokens)
    %
    % Recongize Open ++ Contents ++ Close where ParseContents recognizes
    % Contents.
    %
:- pred brackets(T, T, parser(T, X),
    parse_result(X, T), list(token(T)), list(token(T))).
:- mode brackets(in, in, in(parser(match_or_error)), out(match_or_error),
    in, out) is det.
:- mode brackets(in, in, in(parser), out, in, out) is det.

%-----------------------------------------------------------------------%

:- pred parse_results(list(parse_result(X, T)), parse_result(list(X), T)).
:- mode parse_results(in(list(match_or_error)), out(match_or_error)) is det.
:- mode parse_results(in, out) is det.

%-----------------------------------------------------------------------%

    % parse_N(Result1, ..., ResultN, Result),
    %
    % Parse a sequence of things.
    %
:- pred parse_2(parse_result(X1, T), parse_result(X2, T),
    parse_result({X1, X2}, T)).
:- mode parse_2(in(match_or_error), in(match_or_error),
    out(match_or_error)) is det.
:- mode parse_2(in, in, out) is det.

:- pred parse_3(parse_result(X1, T), parse_result(X2, T),
    parse_result(X3, T), parse_result({X1, X2, X3}, T)).
:- mode parse_3(in(match_or_error), in(match_or_error), in(match_or_error),
    out(match_or_error)) is det.
:- mode parse_3(in, in, in, out) is det.

:- pred parse_4(parse_result(X1, T), parse_result(X2, T),
    parse_result(X3, T), parse_result(X4, T),
    parse_result({X1, X2, X3, X4}, T)).
:- mode parse_4(in(match_or_error), in(match_or_error),
    in(match_or_error), in(match_or_error),
    out(match_or_error)) is det.
:- mode parse_4(in, in, in, in, out) is det.

:- pred parse_5(parse_result(X1, T), parse_result(X2, T),
    parse_result(X3, T), parse_result(X4, T), parse_result(X5, T),
    parse_result({X1, X2, X3, X4, X5}, T)).
:- mode parse_5(in(match_or_error), in(match_or_error),
    in(match_or_error), in(match_or_error), in(match_or_error),
    out(match_or_error)) is det.
:- mode parse_5(in, in, in, in, in, out) is det.

%-----------------------------------------------------------------------%

    % Recognize zero or more instances of some parser.
    %
:- pred zero_or_more(parser(T, X)::in(parser),
    parse_result(list(X), T)::out(match_or_error),
    list(token(T))::in, list(token(T))::out) is det.

    % Recognize at least one instance of some parser.
    %
:- pred one_or_more(parser(T, X), parse_result(list(X), T),
    list(token(T)), list(token(T))).
:- mode one_or_more(in(parser(match_or_error)), out(match_or_error),
    in, out) is det.
:- mode one_or_more(in(parser), out, in, out) is det.

    % Recognize an optional instance of some parser.
    %
:- pred optional(parser(T, X)::in(parser),
    parse_result(maybe(X), T)::out(match_or_error),
    list(token(T))::in, list(token(T))::out) is det.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

:- import_module require.

%-----------------------------------------------------------------------%

token_context([], nil_context).
token_context([token(_, Context) | _], Context).

%-----------------------------------------------------------------------%

match(X, Result, !Tokens) :-
    (
        !.Tokens = [token(Token, Context) | !:Tokens],
        Token = X
    ->
        Result = match(unit, Context)
    ;
        Result = no_match
    ).

match_ho(Pred, Result, !Tokens) :-
    (
        !.Tokens = [token(Token, Context) | !:Tokens],
        Pred(Token, X)
    ->
        Result = match(X, Context)
    ;
        Result = no_match
    ).

consume(X, Result, !Tokens) :-
    ( !.Tokens = [token(Token, Context) | TokensPrime],
        ( Token = X ->
            Result = match(unit, Context),
            !:Tokens = TokensPrime
        ;
            Result = error(pe_unexpected_token(string(X), Token), Context)
        )
    ; !.Tokens = [],
        Result = error(pe_unexpected_eof(string(X)), nil_context)
    ).

consume_ho(Expected, Pred, Result, !Tokens) :-
    ( !.Tokens = [token(Token, Context) | TokensPrime],
        ( Pred(Token, X) ->
            Result = match(X, Context),
            !:Tokens = TokensPrime
        ;
            Result = error(pe_unexpected_token(Expected, Token), Context)
        )
    ; !.Tokens = [],
        Result = error(pe_unexpected_eof(Expected), nil_context)
    ).

%-----------------------------------------------------------------------%

peek(Token, [token(Token, _) | _]).

%-----------------------------------------------------------------------%

brackets(Open, Close, Parser, Result, !Tokens) :-
    TokensBefore = !.Tokens,
    token_context(!.Tokens, Context),
    consume(Open, OpenResult, !Tokens),
    Parser(ParserResult, !Tokens),
    consume(Close, CloseResult, !Tokens),
    (
        OpenResult = match(_, _),
        ParserResult = match(X, _),
        CloseResult = match(_, _)
    ->
        Result = match(X, Context)
    ;
        parse_results([cast_result(OpenResult), cast_result(ParserResult),
            cast_result(CloseResult)], Result0),
        Result = cast_result_error(Result0),
        !:Tokens = TokensBefore
    ).

%-----------------------------------------------------------------------%

parse_results([], match([], nil_context)).
parse_results([Res | Ress], Result) :-
    ( Res = match(X, Context),
        parse_results_2(Ress, Context, [X], Result)
    ; Res = no_match,
        Result = no_match
    ; Res = error(E, C),
        Result = error(E, C)
    ).

:- pred parse_results_2(list(parse_result(X, T)), context,
    list(X), parse_result(list(X), T)).
:- mode parse_results_2(in(list(match_or_error)), in,
    in, out(match_or_error)) is det.
:- mode parse_results_2(in, in, in, out) is det.

parse_results_2([], Context, Xs0, match(reverse(Xs0), Context)).
parse_results_2([Res | Ress], Context, Xs0, Results) :-
    ( Res = match(X, _),
        parse_results_2(Ress, Context, [X | Xs0], Results)
    ; Res = no_match,
        Results = no_match
    ; Res = error(E, C),
        Results = error(E, C)
    ).

:- func cast_result(parse_result(X, T)) = parse_result(unit, T).
:- mode cast_result(in(match_or_error)) = out(match_or_error) is det.
:- mode cast_result(in) = out is det.

cast_result(match(_, C)) = match(unit, C).
cast_result(no_match) = no_match.
cast_result(error(E, C)) = error(E, C).

:- func cast_result_error(parse_result(X, T)) = parse_result(Y, T).
:- mode cast_result_error(in(match_or_error)) = out(match_or_error) is det.
:- mode cast_result_error(in) = out is det.

cast_result_error(match(_, _)) = func_error("Parse results all matched").
cast_result_error(no_match) = no_match.
cast_result_error(error(E, C)) = error(E, C).

%-----------------------------------------------------------------------%

parse_2(RA, RB, R) :-
    ( RA = match(XA, Context),
        ( RB = match(XB, _),
            R = match({XA, XB}, Context)
        ; RB = no_match,
            R = no_match
        ; RB = error(E, C),
            R = error(E, C)
        )
    ; RA = no_match,
        R = no_match
    ; RA = error(E, C),
        R = error(E, C)
    ).

parse_3(RA, RB, RC, R) :-
    ( RA = match(XA, Context),
        ( RB = match(XB, _),
            ( RC = match(XC, _),
                R = match({XA, XB, XC}, Context)
            ; RC = no_match,
                R = no_match
            ; RC = error(E, C),
                R = error(E, C)
            )
        ; RB = no_match,
            R = no_match
        ; RB = error(E, C),
            R = error(E, C)
        )
    ; RA = no_match,
        R = no_match
    ; RA = error(E, C),
        R = error(E, C)
    ).

parse_4(RA, RB, RC, RD, R) :-
    ( RA = match(XA, Context),
        ( RB = match(XB, _),
            ( RC = match(XC, _),
                ( RD = match(XD, _),
                    R = match({XA, XB, XC, XD}, Context)
                ; RD = no_match,
                    R = no_match
                ; RD = error(E, C),
                    R = error(E, C)
                )
            ; RC = no_match,
                R = no_match
            ; RC = error(E, C),
                R = error(E, C)
            )
        ; RB = no_match,
            R = no_match
        ; RB = error(E, C),
            R = error(E, C)
        )
    ; RA = no_match,
        R = no_match
    ; RA = error(E, C),
        R = error(E, C)
    ).

parse_5(RA, RB, RC, RD, RE, R) :-
    ( RA = match(XA, Context),
        ( RB = match(XB, _),
            ( RC = match(XC, _),
                ( RD = match(XD, _),
                    ( RE = match(XE, _),
                        R = match({XA, XB, XC, XD, XE}, Context)
                    ; RE = no_match,
                        R = no_match
                    ; RE = error(E, C),
                        R = error(E, C)
                    )
                ; RD = no_match,
                    R = no_match
                ; RD = error(E, C),
                    R = error(E, C)
                )
            ; RC = no_match,
                R = no_match
            ; RC = error(E, C),
                R = error(E, C)
            )
        ; RB = no_match,
            R = no_match
        ; RB = error(E, C),
            R = error(E, C)
        )
    ; RA = no_match,
        R = no_match
    ; RA = error(E, C),
        R = error(E, C)
    ).

%-----------------------------------------------------------------------%

zero_or_more(Parser, Result, !Tokens) :-
    TokensStart = !.Tokens,
    Parser(Result0, !Tokens),
    ( Result0 = match(X, Context),
        zero_or_more_2(Parser, [], Result1, !Tokens),
        ( Result1 = match(Xs, _),
            Result = match([X | Xs], Context)
        ; Result1 = error(E, C),
            Result = error(E, C),
            !:Tokens = TokensStart
        )
    ; Result0 = no_match,
        Result = match([], C),
        !:Tokens = TokensStart,
        token_context(!.Tokens, C)
    ; Result0 = error(E, C),
        Result = error(E, C),
        !:Tokens = TokensStart
    ).

:- pred zero_or_more_2(parser(T, X)::in(parser), list(X)::in,
    parse_result(list(X), T)::out(match_or_error),
    list(token(T))::in, list(token(T))::out) is det.

zero_or_more_2(Parser, Xs, Result, !Tokens) :-
    Parser(ParserResult, !.Tokens, NextTokens),
    ( ParserResult = match(X, _),
        !:Tokens = NextTokens,
        zero_or_more_2(Parser, [X | Xs], Result, !Tokens)
    ; ParserResult = no_match,
        Result = match(reverse(Xs), nil_context)
    ; ParserResult = error(E, C),
        Result = error(E, C)
    ).

%-----------------------------------------------------------------------%

one_or_more(Parser, Result, !Tokens) :-
    InitialTokens = !.Tokens,
    Parser(FirstResult, !Tokens),
    ( FirstResult = match(X, Context),
        zero_or_more(Parser, RestResult, !Tokens),
        ( RestResult = match(Xs, _),
            Result = match([X | Xs], Context)
        ; RestResult = error(E, C),
            Result = error(E, C),
            !:Tokens = InitialTokens
        )
    ;
        ( FirstResult = no_match,
            Result = no_match
        ; FirstResult = error(E, C),
            Result = error(E, C)
        ),
        !:Tokens = InitialTokens
    ).

%-----------------------------------------------------------------------%

optional(Parser, Result, !Tokens) :-
    Tokens0 = !.Tokens,
    token_context(!.Tokens, Context0),
    Parser(Result0, !Tokens),
    ( Result0 = match(X, Context),
        Result = match(yes(X), Context)
    ;
        ( Result0 = no_match,
            Result = match(no, Context0)
        ; Result0 = error(E, C),
            Result = error(E, C)
        ),
        !:Tokens = Tokens0
    ).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
