%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module parse_util.
%
% Parsing and lexing utils.
%
% Copyright (C) 2015, 2017, 2019-2020 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
%-----------------------------------------------------------------------%
:- interface.

:- import_module io.
:- import_module list.
:- import_module maybe.

:- import_module lex.
:- import_module parsing.
:- import_module result.

%-----------------------------------------------------------------------%

:- type parser(T, R) == pred(list(token(T)), result(R, read_src_error)).
:- inst parser == ( pred(in, out) is det ).

:- type check_token(T) == pred(token(T), maybe(read_src_error)).
:- inst check_token == ( pred(in, out) is det ).

    % parse_file(FileName, Lexemes, IgnoreToken, CheckToken, Parser, Result),
    %
:- pred parse_file(string::in,
        list(lexeme(lex_token(T)))::in, lex.ignore_pred(T)::in(ignore_pred),
        check_token(T)::in(check_token),
        parse_util.parser(T, R)::in(parse_util.parser),
        result(R, read_src_error)::out, io::di, io::uo) is det.

    % parse_file(FileName, Lexemes, IgnoreToken, Parser, Result),
    %
:- pred parse_file(string::in,
        list(lexeme(lex_token(T)))::in, lex.ignore_pred(T)::in(ignore_pred),
        parse_util.parser(T, R)::in(parse_util.parser),
        result(R, read_src_error)::out, io::di, io::uo) is det.

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
    ;       rse_tokeniser_greedy_comment
    ;       rse_tokeniser_starstarslash_comment
    ;       rse_parse_error(pe_got :: string, pe_expect :: string)
    ;       rse_parse_junk_at_end(string).

:- instance error(read_src_error).

%-----------------------------------------------------------------------%

:- pred tokenize(text_input_stream::in, lexer(lex_token(T), string)::in,
    ignore_pred(T)::in(ignore_pred), check_token(T)::in(check_token),
    string::in,
    result(list(token(T)), read_src_error)::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
:- implementation.

:- import_module char.
:- import_module cord.
:- import_module int.
:- import_module string.

:- import_module context.

%-----------------------------------------------------------------------%

parse_file(Filename, Lexemes, IgnoreTokens, CheckToken, Parse, Result, !IO) :-
    io.open_input(Filename, OpenResult, !IO),
    ( OpenResult = ok(File),
        Lexer = lex.init(Lexemes, lex.read_from_string, ignore_nothing),
        tokenize(File, Lexer, IgnoreTokens, CheckToken, Filename,
            TokensResult, !IO),
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

parse_file(Filename, Lexemes, IgnoreTokens, Parse, Result, !IO) :-
    parse_file(Filename, Lexemes, IgnoreTokens, check_ok, Parse, Result,
        !IO).

:- pred check_ok(T::in, maybe(read_src_error)::out) is det.

check_ok(_, no).

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
rse_error_or_warning(rse_tokeniser_greedy_comment) = error.
rse_error_or_warning(rse_tokeniser_starstarslash_comment) = warning.
rse_error_or_warning(rse_parse_error(_, _)) = error.
rse_error_or_warning(rse_parse_junk_at_end(_)) = error.

:- func rse_to_string(read_src_error) = string.

rse_to_string(rse_io_error(Message)) = Message.
rse_to_string(rse_tokeniser_error(Message)) =
    format("Tokenizer error, %s", [s(Message)]).
rse_to_string(rse_tokeniser_greedy_comment) =
    "The tokeniser got confused, " ++
    "until we improve it please don't end comments with **/".
rse_to_string(rse_tokeniser_starstarslash_comment) =
    "The tokeniser can get confused, " ++
    "until we improve it please don't end comments with **/".
rse_to_string(rse_parse_error(Got, Expected)) =
    format("Parse error, read %s expected %s", [s(Got),
        s(Expected)]).
rse_to_string(rse_parse_junk_at_end(Got)) =
    format("Parse error: junk at end of input: %s", [s(Got)]).

%-----------------------------------------------------------------------%

tokenize(File, Lexer, IgnoreTokens, CheckToken, Filename, MaybeTokens, !IO) :-
    io.read_file_as_string(File, ReadResult, !IO),
    ( ReadResult = ok(String0),
        copy(String0, String),
        tokenize_string(Filename, Lexer, IgnoreTokens, CheckToken, String,
            MaybeTokens)
    ; ReadResult = error(_, IOError),
        MaybeTokens = return_error(context(Filename, -1, -1),
            rse_io_error(error_message(IOError)))
    ).

:- pred tokenize_string(string::in, lexer(lex_token(T), string)::in,
    ignore_pred(T)::in(ignore_pred), check_token(T)::in(check_token),
    string::di, result(list(token(T)), read_src_error)::out) is det.

tokenize_string(Filename, Lexer, IgnoreToken, CheckToken, String,
        MaybeTokens) :-
    LS0 = lex.start(Lexer, String),
    tokenize_string(IgnoreToken, CheckToken, Filename, pos(1, 1), [],
        init, MaybeTokens, LS0, LS),
    _ = lex.stop(LS).

:- pred tokenize_string(ignore_pred(T)::in(ignore_pred),
    check_token(T)::in(check_token), string::in, pos::in, list(token(T))::in,
    errors(read_src_error)::in,
    result(list(token(T)), read_src_error)::out,
    lexer_state(lex_token(T), string)::di,
    lexer_state(lex_token(T), string)::uo) is det.

tokenize_string(IgnoreTokens, CheckToken, Filename, Pos0, RevTokens0,
        !.Errors, MaybeTokens, !LS) :-
    pos(Line, Col) = Pos0,
    Context = context(Filename, Line, Col),
    lex.read(MaybeToken, !LS),
    ( MaybeToken = ok(lex_token(Token, String)),
        advance_position(String, Pos0, Pos),
        TAC = token(Token, String, Context),
        CheckToken(TAC, CheckRes),
        ( CheckRes = no
        ; CheckRes = yes(Error),
            add_error(Context, Error, !Errors)
        ),
        ( if IgnoreTokens(Token) then
            RevTokens = RevTokens0
        else
            RevTokens = [TAC | RevTokens0]
        ),
        tokenize_string(IgnoreTokens, CheckToken, Filename, Pos, RevTokens,
            !.Errors, MaybeTokens, !LS)
    ; MaybeToken = eof,
        ( if is_empty(!.Errors) then
            MaybeTokens = ok(reverse(RevTokens0))
        else
            MaybeTokens = errors(!.Errors)
        )
    ; MaybeToken = error(Message, _Line),
        MaybeTokens = return_error(Context, rse_tokeniser_error(Message))
    ).

%-----------------------------------------------------------------------%

:- type pos
    --->    pos(
                p_line  :: int,
                p_col   :: int
            ).

:- pred advance_position(string::in, pos::in, pos::out) is det.

advance_position(String, !Pos) :-
    foldl(advance_position_char, String, !Pos).

:- pred advance_position_char(char::in, pos::in, pos::out) is det.

advance_position_char(Char, !Pos) :-
    ( if Char = '\n' ; Char = '\r' then
        !:Pos = pos(!.Pos ^ p_line + 1, 1)
    else
        !:Pos = pos(!.Pos ^ p_line, !.Pos ^ p_col + 1)
    ).

%-----------------------------------------------------------------------%

:- pred ignore_nothing(Token::in) is semidet.

ignore_nothing(_) :- semidet_false.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
