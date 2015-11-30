%-----------------------------------------------------------------------%
% Utilities for lexical analysis.
% vim: ts=4 sw=4 et
%
% Copyright (C) 2015 Paul Bone
% Distributed under the terms of the GPLv2 see ../LICENSE.tools
%
% This module contains some utilities that are useful when lex.m and
% parsing.m are used together.
%
%-----------------------------------------------------------------------%
:- module lex_util.
%-----------------------------------------------------------------------%

:- interface.

:- import_module io.
:- import_module list.
:- import_module maybe.
:- import_module string.

:- import_module lex.
:- import_module parsing.
:- import_module parsing.bnf.
:- import_module result.

%-----------------------------------------------------------------------%

    % A token during lexical analysis.  Context information is added later
    % and the pzt_token type is then used.
    %
:- type lex_token(T)
    --->    lex_token(T, maybe(string)).

:- func return_simple(T) = token_creator(lex_token(T)).

:- func return_string(T) = token_creator(lex_token(T)).

%-----------------------------------------------------------------------%

:- type read_src_error
    --->    rse_io_error(string)
    ;       rse_tokeniser_error(string)
    ;       rse_parse_unexpected_eof(list(string))
    ;       rse_parse_unexpected_token(list(string), string)
    ;       rse_parse_junk_at_end(string).

:- instance error(read_src_error).

:- func parse_error_to_read_src_error(parse_error(T)) =
    read_src_error.

%-----------------------------------------------------------------------%

:- pred parse_file(string, list(lexeme(lex_token(T))),
        lex.ignore_pred(lex_token(T)), bnf(T, NT, R),
        result(R, read_src_error), io, io)
    <= token_to_result(T, R).
:- mode parse_file(in, in, in(ignore_pred), in, out, di, uo) is det.

%-----------------------------------------------------------------------%

:- pred tokenize(text_input_stream::in,
    lexer(lex_token(T), string)::in,
    string::in, int::in, list(token(T))::in,
    result(list(token(T)), read_src_error)::out,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module require.

:- import_module context.
:- import_module parsing.gen.

%-----------------------------------------------------------------------%

return_simple(T) = lex.return(lex_token(T, no)).

return_string(T) = (func(S) = lex_token(T, yes(S))).

%-----------------------------------------------------------------------%

:- instance error(read_src_error) where [
    func(error_or_warning/1) is rse_error_or_warning,
    func(to_string/1) is rse_to_string
].

:- func rse_error_or_warning(read_src_error) = error_or_warning.

rse_error_or_warning(rse_io_error(_)) = error.
rse_error_or_warning(rse_tokeniser_error(_)) = error.
rse_error_or_warning(rse_parse_unexpected_eof(_)) = error.
rse_error_or_warning(rse_parse_unexpected_token(_, _)) = error.
rse_error_or_warning(rse_parse_junk_at_end(_)) = error.

:- func rse_to_string(read_src_error) = string.

rse_to_string(rse_io_error(Message)) = Message.
rse_to_string(rse_tokeniser_error(Message)) =
    format("Tokenizer error, %s", [s(Message)]).
rse_to_string(rse_parse_unexpected_eof(Expected)) =
    format("Unexpected EOF, expected %s", [s(str_list_or(Expected))]).
rse_to_string(rse_parse_unexpected_token(Expected, Got)) =
    format("Parse error, read %s expected %s", [s(Got),
        s(str_list_or(Expected))]).
rse_to_string(rse_parse_junk_at_end(Got)) =
    format("Parse error: junk at end of input: %s", [s(Got)]).

:- func str_list_or(list(string)) = string.

str_list_or(Strs) = join_list(", ", Strs).

%-----------------------------------------------------------------------%

parse_error_to_read_src_error(pe_unexpected_eof(Tokens)) =
        rse_parse_unexpected_eof(Strs) :-
    Strs = map(string, Tokens).
parse_error_to_read_src_error(pe_unexpected_token(ExpTokens, GotToken)) =
    rse_parse_unexpected_token(map(string, ExpTokens), string(GotToken)).
parse_error_to_read_src_error(pe_junk_at_end(Token)) =
    rse_parse_junk_at_end(string(Token)).

%-----------------------------------------------------------------------%

parse_file(Filename, Lexemes, IgnoreTokens, BNF, Result, !IO) :-
    io.open_input(Filename, OpenResult, !IO),
    ( OpenResult = ok(File),
        Lexer = lex.init(Lexemes, lex.read_from_string, IgnoreTokens),
        tokenize(File, Lexer, Filename, 1, [], TokensResult, !IO),
        io.close_input(File, !IO),
        ( TokensResult = ok(Tokens),
            parsing.parse(make_parser(BNF), Tokens, Result0),
            ( Result0 = ok(Node),
                Result = ok(Node)
            ; Result0 = errors(Errors0),
                Errors = errors_map(parse_error_to_read_src_error, Errors0),
                Result = errors(Errors)
            )
        ; TokensResult = errors(Errors),
            Result = errors(Errors)
        )
    ; OpenResult = error(IOError),
        Result = return_error(context(Filename, 0),
            rse_io_error(error_message(IOError)))
    ).

%-----------------------------------------------------------------------%

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
            rse_io_error(error_message(IOError)))
    ).

:- pred tokenize_line(context::in, list(token(T))::in,
    result(list(token(T)), read_src_error)::out,
    lexer_state(lex_token(T), string)::di,
    lexer_state(lex_token(T), string)::uo) is det.

tokenize_line(Context, RevTokens, MaybeTokens, !LS) :-
    lex.read(MaybeToken, !LS),
    ( MaybeToken = ok(lex_token(Token, MaybeString)),
        TAC = token(Token, MaybeString, Context),
        tokenize_line(Context, [TAC | RevTokens], MaybeTokens, !LS)
    ; MaybeToken = eof,
        MaybeTokens = ok(RevTokens)
    ; MaybeToken = error(Message, _Line),
        MaybeTokens = return_error(Context, rse_tokeniser_error(Message))
    ).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
