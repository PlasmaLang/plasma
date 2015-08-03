%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module pzt_parse.
%
% Parse the PZ textual representation.
%
% Copyright (C) 2015 Paul Bone
% Distributed under the terms of the GPLv2 see ../LICENSE.tools
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
:- import_module parsing.
:- import_module pz.
:- import_module lex.
:- import_module symtab.

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
    list(pzt_token)::in,
    result(list(pzt_token), asm_error)::out,
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

:- pred tokenize_line(context::in, list(pzt_token)::in,
    result(list(pzt_token), asm_error)::out,
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

:- type pzt_token == token(token_basic).

:- type token_basic
    --->    proc
    ;       data
    ;       array
    ;       u8 ; u16 ; u32 ; u64
    ;       s8 ; s16 ; s32 ; s64
    ;       ptr
    ;       open_curly
    ;       close_curly
    ;       open_paren
    ;       close_paren
    ;       dash
    ;       equals
    ;       semicolon
    ;       comma
    ;       period
    ;       identifier(string)
    ;       number(int)
    ;       comment
    ;       whitespace.

:- func lexemes = list(lexeme(token_basic)).

lexemes = [
        ("proc"             -> lex.return(proc)),
        ("data"             -> lex.return(data)),
        ("array"            -> lex.return(array)),
        ("u8"               -> lex.return(u8)),
        ("u16"              -> lex.return(u16)),
        ("u32"              -> lex.return(u32)),
        ("u64"              -> lex.return(u64)),
        ("s8"               -> lex.return(s8)),
        ("s16"              -> lex.return(s16)),
        ("s32"              -> lex.return(s32)),
        ("s64"              -> lex.return(s64)),
        ("ptr"              -> lex.return(ptr)),
        ("{"                -> lex.return(open_curly)),
        ("}"                -> lex.return(close_curly)),
        ("("                -> lex.return(open_paren)),
        (")"                -> lex.return(close_paren)),
        ("-"                -> lex.return(dash)),
        ("="                -> lex.return(equals)),
        (","                -> lex.return(comma)),
        ("."                -> lex.return(period)),
        (";"                -> lex.return(semicolon)),
        (lex.identifier     -> (func(S) = identifier(S))),
        (lex.nat            -> (func(S) = number(det_to_int(S)))),
        ("//" ++ (*(anybut("\n")))
                            -> lex.return(comment)),
        (lex.whitespace     -> lex.return(whitespace))
    ].

:- pred ignore_tokens(token_basic::in) is semidet.

ignore_tokens(whitespace).
ignore_tokens(comment).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- pred parse_tokens(list(pzt_token)::in, result(asm, asm_error)::out)
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
    ; MaybeEntries = error(Error0, Context),
        parser_error_to_asm_error(Error0, Error),
        MaybePZT = return_error(Context, Error)
    ).

:- pred parser_error_to_asm_error(parser_error(token_basic)::in,
    asm_error::out) is det.

parser_error_to_asm_error(pe_unexpected_token(Expected, Got),
    e_parse_error(Expected, string(Got))).
parser_error_to_asm_error(pe_unexpected_eof(Expected),
    e_parse_error_eof(Expected)).
parser_error_to_asm_error(pe_other(Message),
    e_parse_error_other(Message)).

%-----------------------------------------------------------------------%

:- pred parse_toplevel_entry(context::in,
    parse_result(asm_entry, token_basic)::out,
    list(pzt_token)::in, list(pzt_token)::out) is det.

parse_toplevel_entry(_, Result, !Tokens) :-
    ( !.Tokens = [token(Token, Context) | !:Tokens],
        ( Token = proc ->
            parse_2(parse_name, parse_proc, Context, MaybeNameProc, !Tokens),
            ( MaybeNameProc = match({Name, Proc}, ProcContext),
                Result = match(asm_entry(Name, Context, Proc), ProcContext)
            ; MaybeNameProc = error(E, C),
                Result = error(E, C)
            )
        ; Token = data ->
            parse_2(parse_name, parse_data, Context, MaybeNameData, !Tokens),
            ( MaybeNameData = match({Name, Data}, DataContext),
                Result = match(asm_entry(Name, Context, Data), DataContext)
            ; MaybeNameData = error(E, C),
                Result = error(E, C)
            )
        ;
            Result = error(pe_unexpected_token("toplevel entry", Token),
                Context)
        )
    ; !.Tokens = [],
        Result = no_match
    ).

%-----------------------------------------------------------------------%

:- pred parse_name(context::in,
    parse_result(symbol, token_basic)::out(match_or_error),
    list(pzt_token)::in, list(pzt_token)::out) is det.

parse_name(Context0, MaybeName, !Tokens) :-
    ( !.Tokens = [token(Token, Context1) | !:Tokens],
        ( Token = identifier(Name) ->
            Tokens0 = !.Tokens,
            parse_dot_name(Context1, ResultQualname, !Tokens),
            ( ResultQualname = match(QualName, Context),
                MaybeName = match(symbol(Name, QualName), Context)
            ; ResultQualname = no_match,
                MaybeName = match(symbol(Name), Context1),
                !:Tokens = Tokens0
            ; ResultQualname = error(E, C),
                MaybeName = error(E, C)
            )
        ;
            MaybeName = error(pe_unexpected_token("Identifier", Token),
                Context1)
        )
    ; !.Tokens = [],
        MaybeName = error(pe_unexpected_eof("Identifier"), Context0)
    ).

    % Prase a period followed by an identifier (part of a qualified name).
    %
:- pred parse_dot_name(context::in,
    parse_result(string, token_basic)::out,
    list(pzt_token)::in, list(pzt_token)::out) is det.

parse_dot_name(_, Result, !Tokens) :-
    (
        !.Tokens =
            [token(DotToken, _), token(NameToken, Context) | !:Tokens],
        DotToken = period,
        NameToken = identifier(Name)
    ->
        Result = match(Name, Context)
    ;
        Result = no_match
    ).

%-----------------------------------------------------------------------%

:- pred parse_proc(context::in,
    parse_result(entry_type, token_basic)::out(match_or_error),
    list(pzt_token)::in, list(pzt_token)::out) is det.

parse_proc(Context0, Result, !Tokens) :-
    parse_3(parse_signature, optional(parse_proc_body), match(semicolon),
        Context0, Result0, !Tokens),
    ( Result0 = match({Sig, MaybeBody, _}, C),
        ( MaybeBody = yes(Body),
            Result = match(asm_proc(Sig, Body), C)
        ; MaybeBody = no,
            Result = match(asm_proc_decl(Sig), C)
        )
    ; Result0 = error(E, C),
        Result = error(E, C)
    ).

:- pred parse_signature(context::in,
    parse_result(pzt_signature, token_basic)::out(match_or_error),
    list(pzt_token)::in, list(pzt_token)::out) is det.

parse_signature(Context0, Result, !Tokens) :-
    brackets(open_paren, close_paren, parse_signature2, Context0, Result,
        !Tokens).

:- pred parse_signature2(context::in,
    parse_result(pzt_signature, token_basic)::out(match_or_error),
    list(pzt_token)::in, list(pzt_token)::out) is det.

parse_signature2(Context0, Result, !Tokens) :-
    zero_or_more(parse_data_size_in_list, Context0, ResultInput, !Tokens),
    ( ResultInput = match(Input, Context1),
        match(dash, Context1, ResultDash, !Tokens),
        ( ResultDash = match(_, Context2),
            zero_or_more(parse_data_size_in_list, Context2, ResultOutput,
                !Tokens),
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

:- pred parse_data_size_in_list(context::in,
    parse_result(pz_data_width, token_basic)::out,
    list(pzt_token)::in, list(pzt_token)::out) is det.

parse_data_size_in_list(Context0, Result, !Tokens) :-
    ( !.Tokens = [token(Token, Context) | !:Tokens],
        ( Token = dash ->
            Result = no_match
        ; Token = close_paren ->
            Result = no_match
        ; token_is_data_width(Token, DataWidth) ->
            Result = match(DataWidth, Context)
        ;
            Result = error(
                pe_unexpected_token("data width, dash or close paren", Token),
                Context)
        )
    ; !.Tokens = [],
        Result = error(pe_unexpected_eof("data width"), Context0)
    ).

:- pred parse_proc_body(context::in,
    parse_result(list(pzt_instruction), token_basic)::out,
    list(pzt_token)::in, list(pzt_token)::out) is det.

parse_proc_body(Context0, Result, !Tokens) :-
    ( peek(semicolon, !.Tokens) ->
        Result = no_match
    ;
        brackets(open_curly, close_curly, zero_or_more(parse_instr),
            Context0, Result, !Tokens)
    ).

:- pred parse_instr(context::in,
    parse_result(pzt_instruction, token_basic)::out,
    list(pzt_token)::in, list(pzt_token)::out) is det.

parse_instr(Context0, Result, !Tokens) :-
    ( !.Tokens = [token(Token, Context1) | !:Tokens],
        ( Token = identifier(Name) ->
            Tokens0 = !.Tokens,
            parse_dot_name(Context1, ResultQualname, !Tokens),
            ( ResultQualname = match(QualName, Context),
                Result = match(pzti_word(symbol(Name, QualName)), Context)
            ; ResultQualname = no_match,
                Result = match(pzti_word(symbol(Name)), Context1),
                !:Tokens = Tokens0
            ; ResultQualname = error(E, C),
                Result = error(E, C)
            )
        ; Token = number(Num) ->
            Result = match(pzti_load_immediate(Num), Context1)
        ; Token = close_curly ->
            Result = no_match
        ;
            Result = error(pe_unexpected_token("instruction or close-curly",
                Token), Context1)
        )
    ; !.Tokens = [],
        Result = error(pe_unexpected_eof("instruction or close-curly"),
            Context0)
    ).

%-----------------------------------------------------------------------%

:- pred parse_data(context::in,
    parse_result(entry_type, token_basic)::out(match_or_error),
    list(pzt_token)::in, list(pzt_token)::out) is det.

parse_data(Context0, Result, !Tokens) :-
    parse_4(match(equals), parse_type, parse_value, match(semicolon),
        Context0, MaybeTuple, !Tokens),
    ( MaybeTuple = match({_, Type, Value, _}, Context),
        Result = match(asm_data(Type, Value), Context)
    ; MaybeTuple = error(E, C),
        Result = error(E, C)
    ).

:- pred parse_type(context::in,
    parse_result(pz_data_type, token_basic)::out(match_or_error),
    list(pzt_token)::in, list(pzt_token)::out) is det.

parse_type(Context0, Result, !Tokens) :-
    ( !.Tokens = [token(Token, Context1) | !:Tokens],
        ( token_is_data_width(Token, DataWidth) ->
            Result = match(type_data(DataWidth), Context1)
        ; Token = array ->
            brackets(open_paren, close_paren, parse_data_size,
                Context1, Result1, !Tokens),
            ( Result1 = match(DataType, Context),
                Result = match(type_array(DataType), Context)
            ; Result1 = error(E, C),
                Result = error(E, C)
            )
        ;
            Result = error(pe_unexpected_token("data type", Token), Context1)
        )
    ; !.Tokens = [],
        Result = error(pe_unexpected_eof("data type"), Context0)
    ).

:- pred parse_data_size(context::in,
    parse_result(pz_data_width, token_basic)::out(match_or_error),
    list(pzt_token)::in, list(pzt_token)::out) is det.

parse_data_size(Context0, Result, !Tokens) :-
    ( !.Tokens = [token(Token, Context) | !:Tokens],
        ( token_is_data_width(Token, DataWidth) ->
            Result = match(DataWidth, Context)
        ;
            Result = error(
                pe_unexpected_token("data width, dash or close paren", Token),
                Context)
        )
    ; !.Tokens = [],
        Result = error(pe_unexpected_eof("data width"), Context0)
    ).

:- pred parse_value(context::in,
    parse_result(pz_data_value, token_basic)::out(match_or_error),
    list(pzt_token)::in, list(pzt_token)::out) is det.

parse_value(Context0, Result, !Tokens) :-
    ( !.Tokens = [token(Token, Context1) | !:Tokens],
        ( Token = number(Num) ->
            Result = match(pzv_num(Num), Context1)
        ; Token = open_curly ->
            parse_2(at_least_1(parse_number_in_sequence), match(close_curly),
                Context1, ResultList, !Tokens),
            ( ResultList = match({List, _}, Context),
                Result = match(pzv_sequence(List), Context)
            ; ResultList = no_match,
                Result = error(pe_other("empty sequence of values"), Context1)
            ; ResultList = error(E, C),
                Result = error(E, C)
            )
        ;
            Result = error(pe_unexpected_token("value", Token), Context1)
        )
    ; !.Tokens = [],
        Result = error(pe_unexpected_eof("value"), Context0)
    ).

:- pred parse_number_in_sequence(context::in,
    parse_result(int, token_basic)::out,
    list(pzt_token)::in, list(pzt_token)::out) is det.

parse_number_in_sequence(Context0, Result, !Tokens) :-
    ( !.Tokens = [token(Token, Context) | !:Tokens],
        ( Token = number(Num) ->
            Result = match(Num, Context)
        ; Token = close_curly ->
            Result = no_match
        ;
            Result = error(pe_unexpected_token("value", Token), Context)
        )
    ; !.Tokens = [],
        Result = error(pe_unexpected_eof("value"), Context0)
    ).

%-----------------------------------------------------------------------%

:- pred token_is_data_width(token_basic::in, pz_data_width::out) is semidet.

token_is_data_width(u8, w8).
token_is_data_width(s8, w8).
token_is_data_width(u16, w16).
token_is_data_width(s16, w16).
token_is_data_width(u32, w32).
token_is_data_width(s32, w32).
token_is_data_width(u64, w64).
token_is_data_width(s64, w64).
token_is_data_width(ptr, ptr).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
