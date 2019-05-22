%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module parse_util.
%
% Parsing and lexing utils.
%
% Copyright (C) 2015, 2017 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
%-----------------------------------------------------------------------%
:- interface.

:- import_module io.
:- import_module list.
:- import_module string.

:- import_module lex.
:- import_module parsing.
:- import_module result.

%-----------------------------------------------------------------------%

:- pred parse_file(string, list(lexeme(lex_token(T))),
        lex.ignore_pred(lex_token(T)),
        pred(list(token(T)), result(R, read_src_error)),
        result(R, read_src_error), io, io).
:- mode parse_file(in, in, in(ignore_pred), pred(in, out) is det, out,
    di, uo) is det.

%-----------------------------------------------------------------------%

    % A token during lexical analysis.  Context information is added later
    % and the pzt_token type is then used.
    %
:- type lex_token(T)
    --->    lex_token(T, string).

:- func return(T) = token_creator(lex_token(T)).

%-----------------------------------------------------------------------%

:- type read_src_error
    --->    rse_io_error(string)
    ;       rse_tokeniser_error(string)
    ;       rse_parse_error(pe_got :: string, pe_expect :: string)
    ;       rse_parse_junk_at_end(string).

:- instance error(read_src_error).

%-----------------------------------------------------------------------%

:- pred tokenize(text_input_stream::in, lexer(lex_token(T), string)::in,
    ignore_pred(lex_token(T))::in(ignore_pred), string::in, int::in,
    list(token(T))::in, result(list(token(T)), read_src_error)::out,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
:- implementation.

:- import_module int.
:- import_module maybe.

:- import_module context.

%-----------------------------------------------------------------------%

parse_file(Filename, Lexemes, IgnoreTokens, Parse, Result, !IO) :-
    io.open_input(Filename, OpenResult, !IO),
    ( OpenResult = ok(File),
        Lexer = lex.init(Lexemes, lex.read_from_string, ignore_nothing),
        tokenize(File, Lexer, IgnoreTokens, Filename, 1, [], TokensResult, !IO),
        io.close_input(File, !IO),
        ( TokensResult = ok(Tokens),
            Parse(Tokens, Result0),
            ( Result0 = ok(Node),
                Result = ok(Node)
            ; Result0 = errors(Errors),
                Result = errors(Errors)
            )
        ; TokensResult = errors(Errors),
            Result = errors(Errors)
        )
    ; OpenResult = error(IOError),
        Result = return_error(context(Filename, 0, 0),
            rse_io_error(error_message(IOError)))
    ).

%-----------------------------------------------------------------------%

return(T) = (func(S) = lex_token(T, S)).

%-----------------------------------------------------------------------%

:- instance error(read_src_error) where [
    func(error_or_warning/1) is rse_error_or_warning,
    func(to_string/1) is rse_to_string
].

:- func rse_error_or_warning(read_src_error) = error_or_warning.

rse_error_or_warning(rse_io_error(_)) = error.
rse_error_or_warning(rse_tokeniser_error(_)) = error.
rse_error_or_warning(rse_parse_error(_, _)) = error.
rse_error_or_warning(rse_parse_junk_at_end(_)) = error.

:- func rse_to_string(read_src_error) = string.

rse_to_string(rse_io_error(Message)) = Message.
rse_to_string(rse_tokeniser_error(Message)) =
    format("Tokenizer error, %s", [s(Message)]).
rse_to_string(rse_parse_error(Got, Expected)) =
    format("Parse error, read %s expected %s", [s(Got),
        s(Expected)]).
rse_to_string(rse_parse_junk_at_end(Got)) =
    format("Parse error: junk at end of input: %s", [s(Got)]).

%-----------------------------------------------------------------------%

tokenize(File, Lexer, IgnoreTokens, Filename, Line, RevTokens0, MaybeTokens,
        !IO) :-
    io.read_line_as_string(File, ReadResult, !IO),
    ( ReadResult = ok(String0),
        copy(String0 ++ "\n", String),
        LS0 = lex.start(Lexer, String),
        tokenize_line(IgnoreTokens, Filename, Line, 1, [], MaybeTokens0, LS0, LS),
        _ = lex.stop(LS),
        ( MaybeTokens0 = ok(NewTokens),
            RevTokens = NewTokens ++ RevTokens0,
            tokenize(File, Lexer, IgnoreTokens, Filename, Line+1, RevTokens,
                MaybeTokens, !IO)
        ; MaybeTokens0 = errors(_),
            MaybeTokens = MaybeTokens0
        )
    ; ReadResult = eof,
        MaybeTokens = ok(reverse(RevTokens0))
    ; ReadResult = error(IOError),
        MaybeTokens = return_error(context(Filename, Line, 1),
            rse_io_error(error_message(IOError)))
    ).

:- pred tokenize_line(ignore_pred(lex_token(T))::in(ignore_pred), string::in,
    int::in, int::in, list(token(T))::in,
    result(list(token(T)), read_src_error)::out,
    lexer_state(lex_token(T), string)::di,
    lexer_state(lex_token(T), string)::uo) is det.

tokenize_line(IgnoreTokens, Filename, Line, Col, RevTokens0, MaybeTokens, !LS) :-
    Context = context(Filename, Line, Col),
    lex.read(MaybeToken, !LS),
    ( MaybeToken = ok(LexToken@lex_token(Token, String)),
        TAC = token(Token, String, Context),
        ( if IgnoreTokens(LexToken) then
            RevTokens = RevTokens0
        else
            RevTokens = [TAC | RevTokens0]
        ),
        tokenize_line(IgnoreTokens, Filename, Line, Col + length(String),
            RevTokens, MaybeTokens, !LS)
    ; MaybeToken = eof,
        MaybeTokens = ok(RevTokens0)
    ; MaybeToken = error(Message, _Line),
        MaybeTokens = return_error(Context, rse_tokeniser_error(Message))
    ).

%-----------------------------------------------------------------------%

:- pred ignore_nothing(Token::in) is semidet.

ignore_nothing(_) :- semidet_false.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
