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

:- type plasma_parse_error.

:- instance error(plasma_parse_error).

%-----------------------------------------------------------------------%

:- pred parse(string::in, result(plasma_ast, plasma_parse_error)::out,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module list.
:- import_module maybe.
:- import_module unit.

:- import_module lex.
:- import_module parsing_utils.

:- import_module ast.
:- import_module context.

%-----------------------------------------------------------------------%

:- type plasma_parse_error
    --->    ppe_io_error(string)
    ;       ppe_tokeniser_error(string)
    ;       ppe_parse_unexpected_eof(string)
    ;       ppe_parse_unexpected_token(string, string)
    ;       ppe_parse_other(string).

:- instance error(plasma_parse_error) where [
    func(error_or_warning/1) is ppe_error_or_warning,
    func(to_string/1) is ppe_to_string
].

%-----------------------------------------------------------------------%

parse(Filename, Result, !IO) :-
    io.open_input(Filename, OpenResult, !IO),
    ( OpenResult = ok(File),
        Lexer = lex.init(lexemes, lex.read_from_string, ignore_tokens),
        tokenize(File, Lexer, Filename, 1, [], TokensResult, !IO),
        io.close_input(File, !IO),
        ( TokensResult = ok(Tokens),
            parse_tokens(Tokens, Result)
        ; TokensResult = errors(Errors),
            Result = errors(Errors)
        )
    ; OpenResult = error(IOError),
        Result = return_error(context(Filename, 0),
            ppe_io_error(error_message(IOError)))
    ).

%-----------------------------------------------------------------------%

:- pred tokenize(text_input_stream::in, lexer(token, string)::in,
    string::in, int::in,
    list(p_token)::in,
    result(list(p_token), plasma_parse_error)::out,
    io::di, io::uo) is det.

tokenize(File, Lexer, Filename, Line, RevTokens0, MaybeTokens, !IO) :-
    Context = context(Filename, Line),
    io.read_line_as_string(File, ReadResult, !IO),
    ( ReadResult = ok(String0),
        copy(String0 ++ "\n", String),
        LS0 = lex.start(Lexer, String),
        tokenize_line(Context, [], MaybeTokens0, LS0, LS),
        _ = lex.stop(LS),
        ( MaybeTokens0 = ok(NewTokens),
            RevTokens = NewTokens ++ RevTokens0,
            tokenize(File, Lexer, Filename, Line+1, RevTokens, MaybeTokens, !IO)
        ; MaybeTokens0 = errors(_),
            MaybeTokens = MaybeTokens0
        )
    ; ReadResult = eof,
        MaybeTokens = ok(reverse(RevTokens0))
    ; ReadResult = error(IOError),
        MaybeTokens = return_error(Context,
            ppe_io_error(error_message(IOError)))
    ).

:- pred tokenize_line(context::in, list(p_token)::in,
    result(list(p_token), plasma_parse_error)::out,
    lexer_state(token, string)::di,
    lexer_state(token, string)::uo) is det.

tokenize_line(Context, RevTokens, MaybeTokens, !LS) :-
    lex.read(MaybeToken, !LS),
    ( MaybeToken = ok(Token),
        TAC = token(Token, Context),
        tokenize_line(Context, [TAC | RevTokens], MaybeTokens, !LS)
    ; MaybeToken = eof,
        MaybeTokens = ok(RevTokens)
    ; MaybeToken = error(Message, _Line),
        MaybeTokens = return_error(Context, ppe_tokeniser_error(Message))
    ).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- type p_token == token(token).

:- type token
    --->    module_
    ;       import
    ;       using
    ;       observing
    ;       ident(string)
    ;       string(string)
    ;       l_curly
    ;       r_curly
    ;       l_paren
    ;       r_paren
    ;       comma
    ;       period
    ;       semicolon
    ;       colon
    ;       bang
    ;       newline
    ;       comment
    ;       whitespace.

:- func lexemes = list(lexeme(token)).

lexemes = [
        ("module"           -> lex.return(module_)),
        ("import"           -> lex.return(import)),
        ("using"            -> lex.return(using)),
        ("observing"        -> lex.return(observing)),
        ("{"                -> lex.return(l_curly)),
        ("}"                -> lex.return(r_curly)),
        ("("                -> lex.return(l_paren)),
        (")"                -> lex.return(r_paren)),
        (";"                -> lex.return(semicolon)),
        (":"                -> lex.return(colon)),
        (","                -> lex.return(comma)),
        ("."                -> lex.return(period)),
        ("!"                -> lex.return(bang)),
        (lex.identifier     -> (func(S) = ident(S))),
        % TODO: escapes
        ("\"" ++ *(anybut("\"")) ++ "\""
                            -> (func(S0) = string(S) :-
                                    between(S0, 1, length(S0) - 1, S))),

        (("#" ++ *(anybut("\n")))
                            -> lex.return(comment)),
        ("\n"               -> lex.return(newline)),
        (any(" \t\v\f")     -> lex.return(whitespace))
    ].

:- pred ignore_tokens(token::in) is semidet.

ignore_tokens(whitespace).
ignore_tokens(comment).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- pred parse_tokens(list(p_token)::in,
    result(plasma_ast, plasma_parse_error)::out) is det.

parse_tokens(!.Tokens, Result) :-
    consume_newlines(context("", 0), match(_, Context0), !Tokens),
    parse_2(parse_module_decl, zero_or_more(parse_toplevel_item), Context0,
        Result0, !Tokens),
    ( Result0 = match({{ModuleName, MaybeExports}, _Items}, _Context1),
        ( !.Tokens = [],
            Result = ok(plasma_ast(ModuleName, MaybeExports))
        ; !.Tokens = [token(Token, Context) | _],
            Result = return_error(Context,
                ppe_parse_unexpected_token("EOF", string(Token)))
        )
    ; Result0 = no_match,
        Result = return_error(Context0,
            ppe_parse_other("Modules must begin with a module declration"))
    ; Result0 = error(Error0, Context),
        parse_error_to_plasma_parse_error(Error0, Error),
        Result = return_error(Context, Error)
    ).

:- pred parse_module_decl(context::in,
    parse_result({string, maybe(list(string))}, token)::out,
    list(p_token)::in, list(p_token)::out) is det.

parse_module_decl(Context0, Result, !Tokens) :-
    parse_5(match(module_), consume_newlines, consume_ident,
        optional(parse_export_list), consume_eos, Context0, Result0,
        !Tokens),
    ( Result0 = match({_, _, ModuleName, MaybeExports, _}, Context),
        Result = match({ModuleName, MaybeExports}, Context)
    ; Result0 = no_match,
        Result = no_match
    ; Result0 = error(Error, Context),
        Result = error(Error, Context)
    ).

:- pred parse_export_list(context::in,
    parse_result(list(string), token)::out(match_or_error),
    list(p_token)::in, list(p_token)::out) is det.

parse_export_list(Context, Result, !Tokens) :-
    brackets(l_curly, r_curly, parse_export_list_items, Context, Result,
        !Tokens).

:- pred parse_export_list_items(context::in,
    parse_result(list(string), token)::out(match_or_error),
    list(p_token)::in, list(p_token)::out) is det.

parse_export_list_items(Context0, Result, !Tokens) :-
    consume_newlines(Context0, match(_, Context1), !Tokens),
    parse_3(zero_or_more(parse_export_list_item), optional(match_ident),
        consume_newlines, Context1, Result0, !Tokens),
    ( Result0 = match({Items, MaybeLastItem, _}, Context),
        ( MaybeLastItem = yes(LastItem),
            Result = match(Items ++ [LastItem], Context)
        ; MaybeLastItem = no,
            Result = match(Items, Context)
        )
    ; Result0 = error(E, C),
        Result = error(E, C)
    ).

:- pred parse_export_list_item(context::in,
    parse_result(string, token)::out,
    list(p_token)::in, list(p_token)::out) is det.

parse_export_list_item(Context0, Result, !Tokens) :-
    parse_4(match_ident, consume_newlines, match(comma), consume_newlines,
        Context0, Result0, !Tokens),
    ( Result0 = match({String, _, _, _}, Context),
        Result = match(String, Context)
    ; Result0 = no_match,
        Result = no_match
    ; Result0 = error(E, C),
        Result = error(E, C)
    ).

:- pred parse_toplevel_item(context::in,
    parse_result(string, token)::out(match_or_error),
    list(p_token)::in, list(p_token)::out) is det.

%-----------------------------------------------------------------------%

:- pred consume_eos(context::in,
    parse_result(unit, token)::out(match_or_error),
    list(p_token)::in, list(p_token)::out) is det.

consume_eos(Context, Result, !Tokens) :-
    consume_ho("end of statement", is_eos, Context, Result, !Tokens).

:- pred is_eos(token::in) is semidet.

is_eos(semicolon).
is_eos(newline).

:- pred consume_newlines(context::in, parse_result(unit, token)::out(match),
    list(p_token)::in, list(p_token)::out) is det.

consume_newlines(Context0, Result, !Tokens) :-
    ( !.Tokens = [token(newline, Context) | !:Tokens] ->
        consume_newlines(Context, Result, !Tokens)
    ;
        Result = match(unit, Context0)
    ).

:- pred consume_ident(context::in,
    parse_result(string, token)::out(match_or_error),
    list(p_token)::in, list(p_token)::out) is det.

consume_ident(Context0, Result, !Tokens) :-
    ( !.Tokens = [token(Token, Context) | !:Tokens],
        ( Token = ident(Ident) ->
            Result = match(Ident, Context)
        ;
            Result = error(pe_unexpected_token("identifier", Token),
                Context)
        )
    ; !.Tokens = [],
        Result = error(pe_unexpected_eof("identifier"), Context0)
    ).

:- pred match_ident(context::in,
    parse_result(string, token)::out(match_or_nomatch),
    list(p_token)::in, list(p_token)::out) is det.

match_ident(_, Result, !Tokens) :-
    (
        !.Tokens = [token(Token, Context) | !:Tokens],
        Token = ident(Ident)
    ->
        Result = match(Ident, Context)
    ;
        Result = no_match
    ).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- func ppe_error_or_warning(plasma_parse_error) = error_or_warning.

ppe_error_or_warning(ppe_io_error(_)) = error.
ppe_error_or_warning(ppe_tokeniser_error(_)) = error.
ppe_error_or_warning(ppe_parse_unexpected_eof(_)) = error.
ppe_error_or_warning(ppe_parse_unexpected_token(_, _)) = error.
ppe_error_or_warning(ppe_parse_other(_)) = error.

:- func ppe_to_string(plasma_parse_error) = string.

ppe_to_string(ppe_io_error(Message)) = Message.
ppe_to_string(ppe_tokeniser_error(Message)) =
    format("Tokenizer error, %s", [s(Message)]).
ppe_to_string(ppe_parse_unexpected_eof(Expected)) =
    format("Unexpected EOF, expected %s", [s(Expected)]).
ppe_to_string(ppe_parse_unexpected_token(Expected, Got)) =
    format("Parse error, read %s expected %s", [s(Got), s(Expected)]).
ppe_to_string(ppe_parse_other(Message)) =
    format("Parse error: %s", [s(Message)]).

:- pred parse_error_to_plasma_parse_error(parser_error(token)::in,
    plasma_parse_error::out) is det.

parse_error_to_plasma_parse_error(pe_unexpected_eof(Str),
    ppe_parse_unexpected_eof(Str)).
parse_error_to_plasma_parse_error(pe_unexpected_token(Str, T),
    ppe_parse_unexpected_token(Str, string(T))).
parse_error_to_plasma_parse_error(pe_other(Str), ppe_parse_other(Str)).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
