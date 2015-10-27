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

:- type parse_error.

:- instance error(parse_error).

%-----------------------------------------------------------------------%

:- pred parse(string::in, result(plasma_ast, parse_error)::out,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module list.

:- import_module lex.
:- import_module parsing_utils.

:- import_module ast.
:- import_module context.

%-----------------------------------------------------------------------%

:- type parse_error
    ---> pe_io_error(string)
    ;    pe_tokeniser_error(string).

:- instance error(parse_error) where [
    func(error_or_warning/1) is pe_error_or_warning,
    func(to_string/1) is pe_to_string
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
            pe_io_error(error_message(IOError)))
    ).

%-----------------------------------------------------------------------%

:- pred tokenize(text_input_stream::in, lexer(token, string)::in,
    string::in, int::in,
    list(p_token)::in,
    result(list(p_token), parse_error)::out,
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
            pe_io_error(error_message(IOError)))
    ).

:- pred tokenize_line(context::in, list(p_token)::in,
    result(list(p_token), parse_error)::out,
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
        MaybeTokens = return_error(Context, pe_tokeniser_error(Message))
    ).

%-----------------------------------------------------------------------%

:- type p_token == token(token).

:- type token
    --->    module_
    ;       import
    ;       using
    ;       observing
    ;       ident(string)
    ;       string(string)
    ;       l_brace
    ;       r_brace
    ;       l_paren
    ;       r_paren
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
        ("{"                -> lex.return(l_brace)),
        ("}"                -> lex.return(r_brace)),
        ("("                -> lex.return(l_paren)),
        (")"                -> lex.return(r_paren)),
        (";"                -> lex.return(semicolon)),
        (":"                -> lex.return(colon)),
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
        (lex.whitespace     -> lex.return(whitespace))
    ].

:- pred ignore_tokens(token::in) is semidet.

ignore_tokens(whitespace).
ignore_tokens(comment).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- import_module require.

:- pred parse_tokens(list(p_token)::in, result(plasma_ast, parse_error)::out).

parse_tokens(_, _) :-
    error("undefined").

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- func pe_error_or_warning(parse_error) = error_or_warning.

pe_error_or_warning(pe_io_error(_)) = error.
pe_error_or_warning(pe_tokeniser_error(_)) = error.

:- func pe_to_string(parse_error) = string.

pe_to_string(pe_io_error(Message)) = Message.
pe_to_string(pe_tokeniser_error(Message)) =
    format("Tokenizer error, %s", [s(Message)]).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
