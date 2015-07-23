%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module pzt_parse.
%
% Parse the PZ textual representation.
%
% Copyright (C) 2015 Paul Bone
% All rights reserved
%
%-----------------------------------------------------------------------%

:- interface.

%-----------------------------------------------------------------------%

:- import_module io.
:- import_module string.

:- import_module asm_ast.
:- import_module asm_error.
:- import_module result.

:- pred parse(string::in, result(asm, asm_error)::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

:- import_module cord.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module unit.

:- import_module context.
:- import_module lex.

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
            e_io_error(error_message(IOError)))
    ).

%-----------------------------------------------------------------------%

    % tokenize(File, Lexer, Filename, Line, TokensAcc, Result, !LS, !IO)
    %
:- pred tokenize(text_input_stream::in, lexer(token_basic, string)::in,
    string::in, int::in,
    list(token)::in,
    result(list(token), asm_error)::out,
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
        MaybeTokens = return_error(Context, e_io_error(error_message(IOError)))
    ).

:- pred tokenize_line(context::in, list(token)::in,
    result(list(token), asm_error)::out,
    lexer_state(token_basic, string)::di,
    lexer_state(token_basic, string)::uo) is det.

tokenize_line(Context, RevTokens, MaybeTokens, !LS) :-
    lex.read(MaybeToken, !LS),
    ( MaybeToken = ok(Token),
        TAC = token(Token, Context),
        tokenize_line(Context, [TAC | RevTokens], MaybeTokens, !LS)
    ; MaybeToken = eof,
        MaybeTokens = ok(RevTokens)
    ; MaybeToken = error(Message, _Line),
        MaybeTokens = return_error(Context, e_tokeniser_error(Message))
    ).

%-----------------------------------------------------------------------%

:- type token
    --->    token(
                tc_token        :: token_basic,
                tc_context      :: context
            ).

:- type token_basic
    --->    proc
    ;       w32
    ;       ptr
    ;       open_curly
    ;       close_curly
    ;       open_paren
    ;       close_paren
    ;       dash
    ;       identifier(string)
    ;       string_constant(string)
    ;       comment
    ;       whitespace.

:- func lexemes = list(lexeme(token_basic)).

lexemes = [
        ("proc"             -> lex.return(proc)),
        ("w32"              -> lex.return(w32)),
        ("ptr"              -> lex.return(ptr)),
        ("{"                -> lex.return(open_curly)),
        ("}"                -> lex.return(close_curly)),
        ("("                -> lex.return(open_paren)),
        (")"                -> lex.return(close_paren)),
        ("-"                -> lex.return(dash)),
        (lex.identifier     -> (func(S) = identifier(S))),
        ('"' ++ (*(anybut("\""))) ++ '"'
                            -> (func(S) = string_constant(S))),
        ("//" ++ (*(anybut("\n")))
                            -> lex.return(comment)),
        (lex.whitespace     -> lex.return(whitespace))
    ].

:- pred ignore_tokens(token_basic::in) is semidet.

ignore_tokens(whitespace).
ignore_tokens(comment).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- pred parse_tokens(list(token)::in, result(asm, asm_error)::out)
    is det.

parse_tokens(Tokens0, MaybePZT) :-
    zero_or_more(parse_toplevel_entry, context("", 0), MaybeEntries,
        Tokens0, Tokens),
    ( MaybeEntries = match(Entries, _),
        ( Tokens = [],
            MaybePZT = ok(asm(Entries))
        ; Tokens = [token(Token, Context) | _],
            MaybePZT = return_error(Context,
                e_parse_error("toplevel entry", string(Token)))
        )
    ; MaybeEntries = error(Error, Context),
        MaybePZT = return_error(Context, Error)
    ).

:- pred parse_toplevel_entry(context::in,
    parse_result(asm_entry, asm_error)::out,
    list(token)::in, list(token)::out) is det.

parse_toplevel_entry(_, Result, !Tokens) :-
    ( !.Tokens = [token(Token, Context) | !:Tokens],
        ( Token = proc ->
            parse_name(Context, MaybeName, !Tokens),
            then_parse(parse_proc, MaybeName, MaybeNameProc, !Tokens),
            ( MaybeNameProc = match({Name, Proc}, ProcContext),
                Result = match(asm_entry(Name, Context, Proc), ProcContext)
            ; MaybeNameProc = error(E, C),
                Result = error(E, C)
            )
        ;
            Result = error(e_parse_error("toplevel entry", string(Token)),
                Context)
        )
    ; !.Tokens = [],
        Result = no_match
    ).

:- pred parse_name(context::in,
    parse_result(string, asm_error)::out(match_or_error),
    list(token)::in, list(token)::out) is det.

parse_name(Context0, MaybeName, !Tokens) :-
    ( !.Tokens = [token(Token, Context) | !:Tokens],
        ( Token = identifier(Name) ->
            MaybeName = match(Name, Context)
        ;
            MaybeName = error(e_parse_error("Identifier", string(Token)),
                Context)
        )
    ; !.Tokens = [],
        MaybeName = error(e_parse_error_eof("Identifier"), Context0)
    ).

:- pred parse_proc(context::in,
    parse_result(entry_type, asm_error)::out(match_or_error),
    list(token)::in, list(token)::out) is det.

parse_proc(Context0, Result, !Tokens) :-
    parse_signature(Context0, SigResult, !Tokens),
    then_parse(parse_proc_body, SigResult, Result0, !Tokens),
    ( Result0 = match({Sig, Body}, C),
        Result = match(asm_proc(Sig, Body), C)
    ; Result0 = error(E, C),
        Result = error(E, C)
    ).

:- import_module require.

:- pred parse_signature(context::in,
    parse_result(pzt_signature, asm_error)::out(match_or_error),
    list(token)::in, list(token)::out) is det.

parse_signature(Context0, Result, !Tokens) :-
    brackets(open_paren, close_paren, parse_signature2, Context0, Result,
        !Tokens).

:- pred parse_signature2(context::in,
    parse_result(pzt_signature, asm_error)::out(match_or_error),
    list(token)::in, list(token)::out) is det.

parse_signature2(Context0, Result, !Tokens) :-
    zero_or_more(parse_data_size, Context0, ResultInput, !Tokens),
    ( ResultInput = match(Input, Context1),
        match(dash, Context1, ResultDash, !Tokens),
        ( ResultDash = match(_, Context2),
            zero_or_more(parse_data_size, Context2, ResultOutput, !Tokens),
            ( ResultOutput = match(Output, Context),
                Result = match(pzt_signature(Input, Output), Context)
            ; ResultOutput = error(E, Context),
                Result = error(E, Context)
            )
        ; ResultDash = error(E, C),
            Result = error(E, C)
        )
    ; ResultInput = error(E, C),
        Result = error(E, C)
    ).

:- pred parse_data_size(context::in,
    parse_result(pzt_data, asm_error)::out,
    list(token)::in, list(token)::out) is det.

parse_data_size(Context0, Result, !Tokens) :-
    ( !.Tokens = [token(Token, Context) | !:Tokens],
        ( Token = dash ->
            Result = no_match
        ; Token = close_paren ->
            Result = no_match
        ; Token = w32 ->
            Result = match(w32, Context)
        ;
            Result = error(
                e_parse_error("data width, dash or close paren", string(Token)),
                Context)
        )
    ; !.Tokens = [],
        Result = error(e_parse_error_eof("data width"), Context0)
    ).

:- pred parse_proc_body(context::in,
    parse_result(list(pzt_instruction), asm_error)::out(match_or_error),
    list(token)::in, list(token)::out) is det.

parse_proc_body(Context0, Result, !Tokens) :-
    brackets(open_curly, close_curly, zero_or_more(parse_instr),
        Context0, Result, !Tokens).

:- pred parse_instr(context::in,
    parse_result(pzt_instruction, asm_error)::out,
    list(token)::in, list(token)::out) is det.

parse_instr(Context0, Result, !Tokens) :-
    ( !.Tokens = [token(Token, Context) | !:Tokens],
        ( Token = identifier(Name) ->
            Result = match(pzti_call(Name), Context)
        ; Token = string_constant(String) ->
            Result = match(pzti_load_immediate(pztv_string(String)), Context)
        ; Token = close_curly ->
            Result = no_match
        ;
            Result = error(e_parse_error("instruction or close-curly",
                string(Token)), Context)
        )
    ; !.Tokens = [],
        Result = error(e_parse_error_eof("instruction or close-curly"),
            Context0)
    ).

%-----------------------------------------------------------------------%

:- type parse_result(T, E)
    --->    match(T, context)
    ;       no_match
    ;       error(E, context).

:- inst match_or_error
    --->    match(ground, ground)
    ;       error(ground, ground).

:- type parser(X, E) ==
    pred(context, parse_result(X, E), list(token), list(token)).
:- inst parser ==
    (pred(in, out, in, out) is det).
:- inst parser(I) ==
    (pred(in, out(I), in, out) is det).

:- pred match(token_basic, context, parse_result(unit, asm_error),
    list(token), list(token)).
:- mode match(in, in, out(match_or_error), in, out) is det.

match(X, Context0, Result, !Tokens) :-
    ( !.Tokens = [token(Token, Context) | !:Tokens],
        ( Token = X ->
            Result = match(unit, Context)
        ;
            Result = error(e_parse_error(string(Token), string(X)), Context)
        )
    ; !.Tokens = [],
        Result = error(e_parse_error_eof(string(X)), Context0)
    ).

:- pred brackets(token_basic, token_basic, parser(T, asm_error), context,
    parse_result(T, asm_error), list(token), list(token)).
:- mode brackets(in, in, in(parser(match_or_error)), in,
    out(match_or_error), in, out) is det.
:- mode brackets(in, in, in(parser), in, out, in, out) is det.

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

:- pred then_parse(parser(T1, E),
    parse_result(T0, E), parse_result({T0, T1}, E), list(token), list(token)).
:- mode then_parse(in(parser(match_or_error)),
    in(match_or_error), out(match_or_error), in, out) is det.
:- mode then_parse(in(parser),
    in, out, in, out) is det.

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

:- pred zero_or_more(parser(T, E)::in(parser), context::in,
    parse_result(list(T), E)::out(match_or_error),
    list(token)::in, list(token)::out) is det.

zero_or_more(Parser, Context, Result, !Tokens) :-
    zero_or_more_2(Parser, Context, [], Result, !Tokens).

:- pred zero_or_more_2(parser(T, E)::in(parser), context::in, list(T)::in,
    parse_result(list(T), E)::out(match_or_error),
    list(token)::in, list(token)::out) is det.

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
