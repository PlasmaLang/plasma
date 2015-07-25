%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module parsing.
%
% Parsing utils.
%
% Copyright (C) 2015 Paul Bone
% All rights reserved
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
    ;       pe_unexpected_token(string, T).

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

    % then_parse(Parser, LastResult, Result, !Tokens),
    %
    % If LastResult is match(_, _) then run Parser and combine both results
    % (if match) into Result.
    %
:- pred then_parse(parser(T, X1),
    parse_result(X0, T), parse_result({X0, X1}, T),
    list(token(T)), list(token(T))).
:- mode then_parse(in(parser(match_or_error)),
    in(match_or_error), out(match_or_error), in, out) is det.
:- mode then_parse(in(parser),
    in, out, in, out) is det.

%-----------------------------------------------------------------------%

    % Recognize zero or more instances of some parser.
    %
:- pred zero_or_more(parser(T, X)::in(parser), context::in,
    parse_result(list(X), T)::out(match_or_error),
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

then_parse(_, no_match, no_match, !Tokens).
then_parse(_, error(E, C), error(E, C), !Tokens).
then_parse(P, match(X1, C), Result, !Tokens) :-
    P(C, Result0, !Tokens),
    ( Result0 = match(X2, C2),
        Result = match({X1, X2}, C2)
    ; Result0 = no_match,
        Result = no_match
    ; Result0 = error(E, C2),
        Result = error(E, C2)
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
        Result = match(Xs, C0)
    ; ParserResult = error(E, C),
        Result = error(E, C)
    ).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
