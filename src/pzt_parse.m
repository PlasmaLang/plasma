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
:- import_module parsing_utils.
:- import_module pz.
:- import_module pz.code.
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
    ;       block
    ;       data
    ;       array
    ;       w ; w8 ; w16 ; w32 ; w64 ; w_ptr ; ptr
    ;       open_curly
    ;       close_curly
    ;       open_paren
    ;       close_paren
    ;       dash
    ;       equals
    ;       semicolon
    ;       comma
    ;       period
    ;       plus
    ;       star
    ;       slash
    ;       identifier(string)
    ;       number(int)
    ;       comment
    ;       whitespace.

:- func lexemes = list(lexeme(token_basic)).

lexemes = [
        ("proc"             -> lex.return(proc)),
        ("block"            -> lex.return(block)),
        ("data"             -> lex.return(data)),
        ("array"            -> lex.return(array)),
        ("w"                -> lex.return(w)),
        ("w8"               -> lex.return(w8)),
        ("w16"              -> lex.return(w16)),
        ("w32"              -> lex.return(w32)),
        ("w64"              -> lex.return(w64)),
        ("w_ptr"            -> lex.return(w_ptr)),
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
        ("+"                -> lex.return(plus)),
        ("*"                -> lex.return(star)),
        ("/"                -> lex.return(slash)),
        (lex.identifier     -> (func(S) = identifier(S))),
        (?("-") ++ lex.nat  -> (func(S) = number(det_to_int(S)))),
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
    zero_or_more(parse_toplevel_entry, MaybeEntries, Tokens0, Tokens),
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

:- pred parse_toplevel_entry(
    parse_result(asm_entry, token_basic)::out,
    list(pzt_token)::in, list(pzt_token)::out) is det.

parse_toplevel_entry(Result, !Tokens) :-
    StartTokens = !.Tokens,
    ( !.Tokens = [token(Token, Context) | !:Tokens],
        ( Token = proc ->
            parse_name(NameResult, !Tokens),
            parse_proc(ProcResult, !Tokens),
            parse_2(NameResult, ProcResult, MaybeNameProc),
            ( MaybeNameProc = match({Name, Proc}, _),
                Result = match(asm_entry(Name, Context, Proc), Context)
            ; MaybeNameProc = error(E, C),
                Result = error(E, C),
                !:Tokens = StartTokens
            )
        ; Token = data ->
            parse_name(NameResult, !Tokens),
            parse_data(DataResult, !Tokens),
            parse_2(NameResult, DataResult, MaybeNameData),
            ( MaybeNameData = match({Name, Data}, _),
                Result = match(asm_entry(Name, Context, Data), Context)
            ; MaybeNameData = error(E, C),
                Result = error(E, C),
                !:Tokens = StartTokens
            )
        ;
            Result = error(pe_unexpected_token("toplevel entry", Token),
                Context)
        )
    ; !.Tokens = [],
        Result = no_match
    ).

%-----------------------------------------------------------------------%

:- pred parse_name(
    parse_result(symbol, token_basic)::out(match_or_error),
    list(pzt_token)::in, list(pzt_token)::out) is det.

parse_name(MaybeName, !Tokens) :-
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
        MaybeName = error(pe_unexpected_eof("Identifier"), nil_context)
    ).

    % Parse a period followed by an identifier (part of a qualified name).
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

:- pred parse_identifier(
    parse_result(string, token_basic)::out,
    list(pzt_token)::in, list(pzt_token)::out) is det.

parse_identifier(Result, !Tokens) :-
    ( !.Tokens = [token(identifier(Name), Context) | !:Tokens] ->
        Result = match(Name, Context)
    ;
        Result = no_match
    ).

%-----------------------------------------------------------------------%

:- pred parse_proc(
    parse_result(entry_type, token_basic)::out(match_or_error),
    list(pzt_token)::in, list(pzt_token)::out) is det.

parse_proc(Result, !Tokens) :-
    parse_signature(SigResult, !Tokens),
    optional(parse_proc_body, BodyResult, !Tokens),
    consume(semicolon, SemiResult, !Tokens),
    parse_3(SigResult, BodyResult, SemiResult, Result0),
    ( Result0 = match({Sig, MaybeBody, _}, C),
        ( MaybeBody = yes(Body),
            Result = match(asm_proc(Sig, Body), C)
        ; MaybeBody = no,
            Result = match(asm_proc_decl(Sig), C)
        )
    ; Result0 = error(E, C),
        Result = error(E, C)
    ).

:- pred parse_signature(
    parse_result(pz_signature, token_basic)::out(match_or_error),
    list(pzt_token)::in, list(pzt_token)::out) is det.

parse_signature(Result, !Tokens) :-
    brackets(open_paren, close_paren, parse_signature2, Result,
        !Tokens).

:- pred parse_signature2(
    parse_result(pz_signature, token_basic)::out(match_or_error),
    list(pzt_token)::in, list(pzt_token)::out) is det.

parse_signature2(Result, !Tokens) :-
    zero_or_more(parse_data_size_in_list, ResultInput, !Tokens),
    consume(dash, ResultDash, !Tokens),
    zero_or_more(parse_data_size_in_list, ResultOutput, !Tokens),
    parse_3(ResultInput, ResultDash, ResultOutput, Result0),
    ( Result0 = match({Input, _, Output}, Context),
        Result = match(pz_signature(Input, Output), Context)
    ; Result0 = error(E, C),
        Result = error(E, C)
    ).

:- pred parse_data_size_in_list(
    parse_result(pz_data_width, token_basic)::out,
    list(pzt_token)::in, list(pzt_token)::out) is det.

parse_data_size_in_list(Result, !Tokens) :-
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
        Result = error(pe_unexpected_eof("data width"), nil_context)
    ).

:- pred parse_proc_body(
    parse_result(list(pzt_block), token_basic)::out,
    list(pzt_token)::in, list(pzt_token)::out) is det.

parse_proc_body(Result, !Tokens) :-
    ( peek(semicolon, !.Tokens) ->
        Result = no_match
    ;
        brackets(open_curly, close_curly,
            (pred(R::out, T0::in, T::out) is det :-
                one_or_more(parse_block, R, T0, T)
            ), ResultPrime, !Tokens),
        ResultPrime \= no_match
    ->
        Result = ResultPrime
    ;
        brackets(open_curly, close_curly, zero_or_more(parse_instr),
            Result0, !Tokens),
        ( Result0 = match(Instrs, Context),
            Result = match([pzt_block("_", Instrs, Context)], Context)
        ; Result0 = error(E, C),
            Result = error(E, C)
        )
    ).

:- pred parse_block(
    parse_result(pzt_block, token_basic)::out,
    list(pzt_token)::in, list(pzt_token)::out) is det.

parse_block(Result, !Tokens) :-
    match(block, BlockResult, !.Tokens, NextTokens),
    ( BlockResult = match(_, _),
        !:Tokens = NextTokens,
        parse_identifier(IdentResult, !Tokens),
        parse_block_body(BodyResult, !Tokens),
        parse_2(IdentResult, BodyResult, Result0),
        ( Result0 = match({Name, Body}, Context),
            Result = match(pzt_block(Name, Body, Context), Context)
        ; Result0 = no_match,
            Result = no_match
        ; Result0 = error(E, C),
            Result = error(E, C)
        )
    ; BlockResult = no_match,
        Result = no_match
    ).

:- pred parse_block_body(
    parse_result(list(pzt_instruction), token_basic)::out,
    list(pzt_token)::in, list(pzt_token)::out) is det.

parse_block_body(Result, !Tokens) :-
    brackets(open_curly, close_curly, zero_or_more(parse_instr),
        Result, !Tokens).

:- pred parse_instr(
    parse_result(pzt_instruction, token_basic)::out,
    list(pzt_token)::in, list(pzt_token)::out) is det.

parse_instr(Result, !Tokens) :-
    ( !.Tokens = [token(Token, Context1) | !:Tokens],
        ( Token = identifier(Name) ->
            ( Name = "cjmp" ->
                parse_jmp_target(Context1, Result, !Tokens)
            ; builtin_instr(Name, Instr) ->
                Result = match(pzt_instruction(Instr, Context1), Context1)
            ;
                Tokens0 = !.Tokens,
                parse_dot_name(Context1, ResultQualname, !Tokens),
                ( ResultQualname = match(QualName, Context),
                    Instr = pzt_instruction(pzti_word(symbol(Name,
                        QualName)), Context),
                    Result = match(Instr, Context)
                ; ResultQualname = no_match,
                    Instr = pzt_instruction(pzti_word(symbol(Name)),
                        Context1),
                    Result = match(Instr, Context1),
                    !:Tokens = Tokens0
                ; ResultQualname = error(E, C),
                    Result = error(E, C)
                )
            )
        ;
            ( Token = number(Num),
                Instr = pzt_instruction(pzti_load_immediate(Num), Context1)
            ; Token = plus,
                Instr = pzt_instruction(pzti_add, Context1)
            ; Token = dash,
                Instr = pzt_instruction(pzti_sub, Context1)
            ; Token = star,
                Instr = pzt_instruction(pzti_mul, Context1)
            ; Token = slash,
                Instr = pzt_instruction(pzti_div, Context1)
            )
        ->
            Result = match(Instr, Context1)
        ; Token = close_curly ->
            Result = no_match
        ;
            Result = error(pe_unexpected_token("instruction or close-curly",
                Token), Context1)
        )
    ; !.Tokens = [],
        Result = error(pe_unexpected_eof("instruction or close-curly"),
            nil_context)
    ).

:- pred parse_jmp_target(context::in,
    parse_result(pzt_instruction, token_basic)::out,
    list(pzt_token)::in, list(pzt_token)::out) is det.

parse_jmp_target(Context0, Result, !Tokens) :-
    ( !.Tokens = [token(Token, Context) | !:Tokens],
        ( Token = identifier(JumpTarget) ->
            Instr = pzt_instruction(pzti_cjmp(JumpTarget), Context),
            Result = match(Instr, Context)
        ;
            Result = error(pe_unexpected_token("identifier", Token),
                Context)
        )
    ; !.Tokens = [],
        Result = error(pe_unexpected_eof("identifier"), Context0)
    ).

    % Identifiers that are builtin instructions.
    %
:- pred builtin_instr(string::in, pzt_instruction_code::out) is semidet.

builtin_instr("dup",    pzti_dup).
builtin_instr("drop",   pzti_drop).
builtin_instr("swap",   pzti_swap).
builtin_instr("ret",    pzti_ret).
builtin_instr("lt_u",   pzti_lt_u).
builtin_instr("lt_s",   pzti_lt_s).
builtin_instr("gt_u",   pzti_gt_u).
builtin_instr("gt_s",   pzti_gt_s).

%-----------------------------------------------------------------------%

:- pred parse_data(
    parse_result(entry_type, token_basic)::out(match_or_error),
    list(pzt_token)::in, list(pzt_token)::out) is det.

parse_data(Result, !Tokens) :-
    consume(equals, EqualsResult, !Tokens),
    parse_type(TypeResult, !Tokens),
    parse_value(ValueResult, !Tokens),
    consume(semicolon, SemiResult, !Tokens),
    parse_4(EqualsResult, TypeResult, ValueResult, SemiResult, Result0),
    ( Result0 = match({_, Type, Value, _}, Context),
        Result = match(asm_data(Type, Value), Context)
    ; Result0 = error(E, C),
        Result = error(E, C)
    ).

:- pred parse_type(
    parse_result(pz_data_type, token_basic)::out(match_or_error),
    list(pzt_token)::in, list(pzt_token)::out) is det.

parse_type(Result, !Tokens) :-
    ( !.Tokens = [token(Token, Context1) | !:Tokens],
        ( token_is_data_width(Token, DataWidth) ->
            Result = match(type_basic(DataWidth), Context1)
        ; Token = array ->
            brackets(open_paren, close_paren, parse_data_size,
                Result1, !Tokens),
            ( Result1 = match(DataType, Context),
                Result = match(type_array(DataType), Context)
            ; Result1 = error(E, C),
                Result = error(E, C)
            )
        ;
            Result = error(pe_unexpected_token("data type", Token), Context1)
        )
    ; !.Tokens = [],
        Result = error(pe_unexpected_eof("data type"), nil_context)
    ).

:- pred parse_data_size(
    parse_result(pz_data_width, token_basic)::out(match_or_error),
    list(pzt_token)::in, list(pzt_token)::out) is det.

parse_data_size(Result, !Tokens) :-
    ( !.Tokens = [token(Token, Context) | !:Tokens],
        ( token_is_data_width(Token, DataWidth) ->
            Result = match(DataWidth, Context)
        ;
            Result = error(
                pe_unexpected_token("data width, dash or close paren", Token),
                Context)
        )
    ; !.Tokens = [],
        Result = error(pe_unexpected_eof("data width"), nil_context)
    ).

:- pred parse_value(
    parse_result(pz_data_value, token_basic)::out(match_or_error),
    list(pzt_token)::in, list(pzt_token)::out) is det.

parse_value(Result, !Tokens) :-
    ( !.Tokens = [token(Token, Context1) | !:Tokens],
        ( Token = number(Num) ->
            Result = match(pzv_num(Num), Context1)
        ; Token = open_curly ->
            one_or_more(parse_number_in_sequence, ListResult, !Tokens),
            consume(close_curly, CloseResult, !Tokens),
            parse_2(ListResult, CloseResult, Result0),
            ( Result0 = match({List, _}, Context),
                Result = match(pzv_sequence(List), Context)
            ; Result0 = no_match,
                ( !.Tokens = [token(NextToken, C) | _],
                    Result = error(pe_unexpected_token("number", NextToken), C)
                ; !.Tokens = [],
                    Result = error(pe_unexpected_eof("number"), nil_context)
                )
            ; Result0 = error(E, C),
                Result = error(E, C)
            )
        ;
            Result = error(pe_unexpected_token("value", Token), Context1)
        )
    ; !.Tokens = [],
        Result = error(pe_unexpected_eof("value"), nil_context)
    ).

:- pred parse_number_in_sequence(
    parse_result(int, token_basic)::out,
    list(pzt_token)::in, list(pzt_token)::out) is det.

parse_number_in_sequence(Result, !Tokens) :-
    ( !.Tokens = [token(Token, Context) | !:Tokens],
        ( Token = number(Num) ->
            Result = match(Num, Context)
        ; Token = close_curly ->
            Result = no_match
        ;
            Result = error(pe_unexpected_token("value", Token), Context)
        )
    ; !.Tokens = [],
        Result = error(pe_unexpected_eof("value"), nil_context)
    ).

%-----------------------------------------------------------------------%

:- pred token_is_data_width(token_basic::in, pz_data_width::out) is semidet.

token_is_data_width(w,      w_fast).
token_is_data_width(w8,     w8).
token_is_data_width(w16,    w16).
token_is_data_width(w32,    w32).
token_is_data_width(w64,    w64).
token_is_data_width(w_ptr,  w_ptr).
token_is_data_width(ptr,    ptr).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
