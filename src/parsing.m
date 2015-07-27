%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module parsing.
%
% Parsing utils.
%
% Copyright (C) 2015 Paul Bone
% Distributed under the terms of the GPLv2 see ../LICENSE.tools
%
%-----------------------------------------------------------------------%

:- interface.

:- import_module list.
:- import_module string.
:- import_module unit.

:- import_module context.

%-----------------------------------------------------------------------%

:- type token(T)
    --->    token(T, context).

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

:- type parser_error(T)
    --->    pe_unexpected_eof(string)
    ;       pe_unexpected_token(string, T)
    ;       pe_other(string).

    % The parser combinators below take parsers of this general form,
    %
:- type parser(T, X) ==
    pred(context, parse_result(X, T), list(token(T)), list(token(T))).
:- inst parser ==
    (pred(in, out, in, out) is det).
:- inst parser(I) ==
    (pred(in, out(I), in, out) is det).

%-----------------------------------------------------------------------%

    % match(T, Context, Result, !Tokens),
    %
    % Read a token T from !Tokens and raise an error if !Tokens is empty or
    % the next token does not match T.
    %
:- pred match(T, context, parse_result(unit, T),
    list(token(T)), list(token(T))).
:- mode match(in, in, out(match_or_error), in, out) is det.

%-----------------------------------------------------------------------%

    % brackets(Open, Close, ParseContents, Context, Result, !Tokens)
    %
    % Recongize Open ++ Contents ++ Close where ParseContents recognizes
    % Contents.
    %
:- pred brackets(T, T, parser(T, X), context,
    parse_result(X, T), list(token(T)), list(token(T))).
:- mode brackets(in, in, in(parser(match_or_error)), in, out(match_or_error),
    in, out) is det.
:- mode brackets(in, in, in(parser), in, out, in, out) is det.

%-----------------------------------------------------------------------%

    % parse_N(Parser1, ..., ParserN, Context, Result, !Tokens),
    %
    % Parse a sequence of things.
    %
:- pred parse_2(parser(T, X1), parser(T, X2),
    context, parse_result({X1, X2}, T), list(token(T)), list(token(T))).
:- mode parse_2(in(parser(match_or_error)), in(parser(match_or_error)),
    in, out(match_or_error), in, out) is det.
:- mode parse_2(in(parser), in(parser),
    in, out, in, out) is det.

:- pred parse_3(parser(T, X1), parser(T, X2), parser(T, X3),
    context, parse_result({X1, X2, X3}, T), list(token(T)), list(token(T))).
:- mode parse_3(in(parser(match_or_error)), in(parser(match_or_error)),
    in(parser(match_or_error)),
    in, out(match_or_error), in, out) is det.
:- mode parse_3(in(parser), in(parser), in(parser),
    in, out, in, out) is det.

:- pred parse_4(parser(T, X1), parser(T, X2), parser(T, X3), parser(T, X4),
    context, parse_result({X1, X2, X3, X4}, T), list(token(T)), list(token(T))).
:- mode parse_4(in(parser(match_or_error)), in(parser(match_or_error)),
    in(parser(match_or_error)), in(parser(match_or_error)),
    in, out(match_or_error), in, out) is det.
:- mode parse_4(in(parser), in(parser), in(parser), in(parser),
    in, out, in, out) is det.

%-----------------------------------------------------------------------%

    % Recognize zero or more instances of some parser.
    %
:- pred zero_or_more(parser(T, X)::in(parser), context::in,
    parse_result(list(X), T)::out(match_or_error),
    list(token(T))::in, list(token(T))::out) is det.

    % Recognize at least one instance of some parser.
    %
:- pred at_least_1(parser(T, X)::in(parser), context::in,
    parse_result(list(X), T)::out,
    list(token(T))::in, list(token(T))::out) is det.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

%-----------------------------------------------------------------------%

match(X, Context0, Result, !Tokens) :-
    ( !.Tokens = [token(Token, Context) | !:Tokens],
        ( Token = X ->
            Result = match(unit, Context)
        ;
            Result = error(pe_unexpected_token(string(X), Token), Context)
        )
    ; !.Tokens = [],
        Result = error(pe_unexpected_eof(string(X)), Context0)
    ).

%-----------------------------------------------------------------------%

brackets(Open, Close, Parser, Context0, Result, !Tokens) :-
    match(Open, Context0, OpenResult, !Tokens),
    ( OpenResult = match(_, Context1),
        Parser(Context1, ParserResult, !Tokens),
        ( ParserResult = match(X, Context2),
            match(Close, Context2, CloseResult, !Tokens),
            ( CloseResult = match(_, Context),
                Result = match(X, Context)
            ; CloseResult = error(E, C),
                Result = error(E, C)
            )
        ; ParserResult = no_match,
            Result = no_match
        ; ParserResult = error(E, C),
            Result = error(E, C)
        )
    ; OpenResult = error(E, C),
        Result = error(E, C)
    ).

%-----------------------------------------------------------------------%

parse_2(PA, PB, C0, R, !Tokens) :-
    PA(C0, R0, !Tokens),
    ( R0 = match(XA, C1),
        PB(C1, R1, !Tokens),
        ( R1 = match(XB, C2),
            R = match({XA, XB}, C2)
        ; R1 = no_match,
            R = no_match
        ; R1 = error(E, C),
            R = error(E, C)
        )
    ; R0 = no_match,
        R = no_match
    ; R0 = error(E, C),
        R = error(E, C)
    ).

parse_3(PA, PB, PC, C0, R, !Tokens) :-
    PA(C0, R0, !Tokens),
    ( R0 = match(XA, C1),
        PB(C1, R1, !Tokens),
        ( R1 = match(XB, C2),
            PC(C2, R2, !Tokens),
            ( R2 = match(XC, C3),
                R = match({XA, XB, XC}, C3)
            ; R2 = no_match,
                R = no_match
            ; R2 = error(E, C),
                R = error(E, C)
            )
        ; R1 = no_match,
            R = no_match
        ; R1 = error(E, C),
            R = error(E, C)
        )
    ; R0 = no_match,
        R = no_match
    ; R0 = error(E, C),
        R = error(E, C)
    ).

parse_4(PA, PB, PC, PD, C0, R, !Tokens) :-
    PA(C0, R0, !Tokens),
    ( R0 = match(XA, C1),
        PB(C1, R1, !Tokens),
        ( R1 = match(XB, C2),
            PC(C2, R2, !Tokens),
            ( R2 = match(XC, C3),
                PD(C3, R3, !Tokens),
                    ( R3 = match(XD, C4),
                        R = match({XA, XB, XC, XD}, C4)
                    ; R3 = no_match,
                        R = no_match
                    ; R3 = error(E, C),
                        R = error(E, C)
                    )
            ; R2 = no_match,
                R = no_match
            ; R2 = error(E, C),
                R = error(E, C)
            )
        ; R1 = no_match,
            R = no_match
        ; R1 = error(E, C),
            R = error(E, C)
        )
    ; R0 = no_match,
        R = no_match
    ; R0 = error(E, C),
        R = error(E, C)
    ).

%-----------------------------------------------------------------------%

zero_or_more(Parser, Context, Result, !Tokens) :-
    zero_or_more_2(Parser, Context, [], Result, !Tokens).

:- pred zero_or_more_2(parser(T, X)::in(parser), context::in, list(X)::in,
    parse_result(list(X), T)::out(match_or_error),
    list(token(T))::in, list(token(T))::out) is det.

zero_or_more_2(Parser, C0, Xs, Result, !Tokens) :-
    Parser(C0, ParserResult, !.Tokens, NextTokens),
    ( ParserResult = match(X, C),
        !:Tokens = NextTokens,
        zero_or_more_2(Parser, C, [X | Xs], Result, !Tokens)
    ; ParserResult = no_match,
        Result = match(reverse(Xs), C0)
    ; ParserResult = error(E, C),
        Result = error(E, C)
    ).

%-----------------------------------------------------------------------%

at_least_1(Parser, Context0, Result, !Tokens) :-
    InitialTokens = !.Tokens,
    Parser(Context0, FirstResult, !Tokens),
    ( FirstResult = match(X, Context1),
        zero_or_more(Parser, Context1, RestResult, !Tokens),
        ( RestResult = match(Xs0, Context),
            Xs = [X | Xs0],
            Result = match(Xs, Context)
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
%-----------------------------------------------------------------------%
