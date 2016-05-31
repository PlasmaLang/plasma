%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module pzt_parse.
%
% Parse the PZ textual representation.
%
% Copyright (C) 2015 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
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
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module require.
:- import_module unit.

:- import_module context.
:- import_module lex.
:- import_module parse_util.
:- import_module parsing.
:- import_module pz.
:- import_module pz.code.
:- import_module symtab.

%-----------------------------------------------------------------------%

parse(Filename, Result, !IO) :-
    parse_file(Filename, lexemes, ignore_tokens, parse_pzt, Result0, !IO),
    ( Result0 = ok(AST),
        Result = ok(AST)
    ; Result0 = errors(Errors),
        Result = errors(map(
            (func(error(C, E)) = error(C, e_read_src_error(E))), Errors))
    ).

%-----------------------------------------------------------------------%

:- type pzt_token == token(token_basic).

:- type pzt_tokens == list(pzt_token).

:- type token_basic
    --->    proc
    ;       block
    ;       data
    ;       array
    ;       cjmp
    ;       roll
    ;       pick
    ;       w ; w8 ; w16 ; w32 ; w64 ; w_ptr ; ptr
    ;       open_curly
    ;       close_curly
    ;       open_paren
    ;       close_paren
    ;       dash
    ;       equals
    ;       semicolon
    ;       colon
    ;       comma
    ;       period
    ;       identifier
    ;       number
    ;       comment
    ;       whitespace
    ;       eof.

:- func lexemes = list(lexeme(lex_token(token_basic))).

lexemes = [
        ("proc"             -> return(proc)),
        ("block"            -> return(block)),
        ("data"             -> return(data)),
        ("array"            -> return(array)),
        ("cjmp"             -> return(cjmp)),
        ("roll"             -> return(roll)),
        ("pick"             -> return(pick)),
        ("w"                -> return(w)),
        ("w8"               -> return(w8)),
        ("w16"              -> return(w16)),
        ("w32"              -> return(w32)),
        ("w64"              -> return(w64)),
        ("w_ptr"            -> return(w_ptr)),
        ("ptr"              -> return(ptr)),
        ("{"                -> return(open_curly)),
        ("}"                -> return(close_curly)),
        ("("                -> return(open_paren)),
        (")"                -> return(close_paren)),
        ("-"                -> return(dash)),
        ("="                -> return(equals)),
        (","                -> return(comma)),
        ("."                -> return(period)),
        (";"                -> return(semicolon)),
        (":"                -> return(colon)),
        (lex.identifier     -> return(identifier)),
        (?("-") ++ lex.nat  -> return(number)),
        ("//" ++ (*(anybut("\n")))
                            -> return(comment)),
        (lex.whitespace     -> return(whitespace))
    ].

:- pred ignore_tokens(lex_token(token_basic)::in) is semidet.

ignore_tokens(lex_token(whitespace, _)).
ignore_tokens(lex_token(comment, _)).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- pred parse_pzt(pzt_tokens::in, result(asm, read_src_error)::out)
    is det.

parse_pzt(Tokens, Result) :-
    zero_or_more_last_error(or([parse_proc, parse_data]), ok(Items),
        LastError, Tokens, EmptyTokens),
    ( EmptyTokens = [],
        Result = ok(asm(Items))
    ; EmptyTokens = [token(Tok, _, TokCtxt) | _],
        LastError = error(LECtxt, Got, Expect),
        ( if compare((<), LECtxt, TokCtxt) then
            Result = return_error(TokCtxt, rse_parse_junk_at_end(string(Tok)))
        else
            Result = return_error(LECtxt, rse_parse_error(Got, Expect))
        )
    ).

:- pred parse_data(parse_res(asm_entry)::out,
    pzt_tokens::in, pzt_tokens::out) is det.

parse_data(Result, !Tokens) :-
    get_context(!.Tokens, Context),
    match_token(data, MatchData, !Tokens),
    parse_ident(IdentResult, !Tokens),
    match_token(equals, MatchEquals, !Tokens),
    parse_data_type(TypeResult, !Tokens),
    parse_data_value(ValueResult, !Tokens),
    match_token(semicolon, MatchSemi, !Tokens),
    ( if
        MatchData = ok(_),
        IdentResult = ok(Ident),
        MatchEquals = ok(_),
        TypeResult = ok(Type),
        ValueResult = ok(Value),
        MatchSemi = ok(_)
    then
        Result = ok(asm_entry(symbol(Ident), Context, asm_data(Type, Value)))
    else
        Result = combine_errors_6(MatchData, IdentResult, MatchEquals,
            TypeResult, ValueResult, MatchSemi)
    ).

:- pred parse_data_type(parse_res(pz_data_type)::out,
    pzt_tokens::in, pzt_tokens::out) is det.

parse_data_type(Result, !Tokens) :-
    % Only arrays are implemented.
    match_tokens([array, open_paren], StartMatch, !Tokens),
    parse_data_width(WidthResult, !Tokens),
    match_token(close_paren, CloseMatch, !Tokens),
    ( if
        StartMatch = ok(_),
        WidthResult = ok(Width),
        CloseMatch = ok(_)
    then
        Result = ok(type_array(Width))
    else
        Result = combine_errors_3(StartMatch, WidthResult, CloseMatch)
    ).

:- pred parse_data_value(parse_res(pz_data_value)::out,
    pzt_tokens::in, pzt_tokens::out) is det.

parse_data_value(Result, !Tokens) :-
    % Only sequences are implemented.
    within(open_curly, zero_or_more(parse_number), close_curly, Result0,
        !Tokens),
    Result = map((func(Nums) = pzv_sequence(Nums)), Result0).

:- pred parse_proc(parse_res(asm_entry)::out,
    pzt_tokens::in, pzt_tokens::out) is det.

parse_proc(Result, !Tokens) :-
    get_context(StartTokens, Context),
    StartTokens = !.Tokens,
    match_token(proc, MatchProc, !Tokens),
    parse_qname(QNameResult, !Tokens),
    parse_sig(SigResult, !Tokens),
    optional_last_error(parse_body, ok(MaybeBody), BodyLastError, !Tokens),
    match_token(semicolon, MatchSemicolon, !Tokens),
    ( if
        MatchProc = ok(_),
        QNameResult = ok(QName),
        SigResult = ok(Sig),
        MatchSemicolon = ok(_)
    then
        ( MaybeBody = yes(Body),
            Result = ok(asm_entry(QName, Context, asm_proc(Sig, Body)))
        ; MaybeBody = no,
            Result = ok(asm_entry(QName, Context, asm_proc_decl(Sig)))
        )
    else
        !:Tokens = StartTokens,
        Result = combine_errors_4(MatchProc, QNameResult, SigResult,
            latest_error(BodyLastError, MatchSemicolon) `with_type`
                parse_res(unit))
    ).

:- pred parse_sig(parse_res(pz_signature)::out,
    pzt_tokens::in, pzt_tokens::out) is det.

parse_sig(Result, !Tokens) :-
    get_context(!.Tokens, Context),
    match_token(open_paren, MatchOpen, !Tokens),
    zero_or_more(parse_data_width, ok(Inputs), !Tokens),
    match_token(dash, MatchDash, !Tokens),
    zero_or_more(parse_data_width, ok(Outputs), !Tokens),
    match_token(close_paren, MatchClose, !Tokens),
    ( if
        MatchOpen = ok(_),
        MatchDash = ok(_),
        MatchClose = ok(_)
    then
        Result = ok(pz_signature(Inputs, Outputs))
    else
        Result = error(Context, "malformed signature", "Inputs '-' Outputs")
    ).

:- pred parse_body(parse_res(list(pzt_block))::out,
    pzt_tokens::in, pzt_tokens::out) is det.

parse_body(Result, !Tokens) :-
    within(open_curly, or([one_or_more(parse_block), parse_instrs]),
        close_curly, Result, !Tokens).

:- pred parse_block(parse_res(pzt_block)::out,
    pzt_tokens::in, pzt_tokens::out) is det.

parse_block(Result, !Tokens) :-
    get_context(!.Tokens, Context),
    match_token(block, MatchBlock, !Tokens),
    parse_ident(IdentResult, !Tokens),
    within(open_curly, zero_or_more(parse_instr), close_curly, InstrsResult,
        !Tokens),
    ( if
        MatchBlock = ok(_),
        IdentResult = ok(Ident),
        InstrsResult = ok(Instrs)
    then
        Result = ok(pzt_block(Ident, Instrs, Context))
    else
        Result = combine_errors_3(MatchBlock, IdentResult, InstrsResult)
    ).

:- pred parse_instrs(parse_res(list(pzt_block))::out,
    pzt_tokens::in, pzt_tokens::out) is det.

parse_instrs(Result, !Tokens) :-
    get_context(!.Tokens, Context),
    one_or_more(parse_instr, InstrsResult, !Tokens),
    Result = map((func(Instrs) = [pzt_block("", Instrs, Context)]),
        InstrsResult).

:- pred parse_instr(parse_res(pzt_instruction)::out,
    pzt_tokens::in, pzt_tokens::out) is det.

parse_instr(Result, !Tokens) :-
    get_context(!.Tokens, Context),
    parse_instr_code(Result0, !Tokens),
    optional(parse_full_width_suffix, ok(MaybeWidth), !Tokens),
    ( Result0 = ok(Code),
        ( MaybeWidth = no,
            Width = no
        ; MaybeWidth = yes({Width1, no}),
            Width = one_width(Width1)
        ; MaybeWidth = yes({Width1, yes(Width2)}),
            Width = two_widths(Width1, Width2)
        ),
        Result = ok(pzt_instruction(Code, Width, Context))
    ; Result0 = error(C, G, E),
        Result = error(C, G, E)
    ).

:- pred parse_instr_code(parse_res(pzt_instruction_code)::out,
    pzt_tokens::in, pzt_tokens::out) is det.

parse_instr_code(Result, !Tokens) :-
    or([parse_ident_instr, parse_number_instr, parse_cjmp_instr,
            parse_imm_instr],
        Result, !Tokens).

:- pred parse_ident_instr(parse_res(pzt_instruction_code)::out,
    pzt_tokens::in, pzt_tokens::out) is det.

parse_ident_instr(Result, !Tokens) :-
    parse_ident(Result0, !Tokens),
    Result = map((func(S) = pzti_word(symbol(S))), Result0).

:- pred parse_number_instr(parse_res(pzt_instruction_code)::out,
    pzt_tokens::in, pzt_tokens::out) is det.

parse_number_instr(Result, !Tokens) :-
    parse_number(ResNumber, !Tokens),
    Result = map((func(N) = pzti_load_immediate(N)), ResNumber).

:- pred parse_cjmp_instr(parse_res(pzt_instruction_code)::out,
    pzt_tokens::in, pzt_tokens::out) is det.

parse_cjmp_instr(Result, !Tokens) :-
    match_token(cjmp, MatchCjmp, !Tokens),
    parse_ident(DestResult, !Tokens),
    ( if
        MatchCjmp = ok(_),
        DestResult = ok(Dest)
    then
        Result = ok(pzti_cjmp(Dest))
    else
        Result = combine_errors_2(MatchCjmp, DestResult)
    ).

:- pred parse_imm_instr(parse_res(pzt_instruction_code)::out,
    pzt_tokens::in, pzt_tokens::out) is det.

parse_imm_instr(Result, !Tokens) :-
    next_token("instruction", InstrResult, !Tokens),
    parse_number(ImmResult, !Tokens),
    ( if
        InstrResult = ok(token_and_string(Instr, _)),
        ImmResult = ok(Imm)
    then
        ( if Instr = roll then
            Result = ok(pzti_roll(Imm))
        else if Instr = pick then
            Result = ok(pzti_pick(Imm))
        else
            unexpected($file, $pred, "instruction token")
        )
    else
        Result = combine_errors_2(InstrResult, ImmResult)
    ).

:- pred parse_full_width_suffix(
    parse_res({pz_data_width, maybe(pz_data_width)})::out,
    pzt_tokens::in, pzt_tokens::out) is det.

parse_full_width_suffix(Result, !Tokens) :-
    parse_width_suffix(WidthResult, !Tokens),
    optional(parse_width_suffix, ok(MaybeWidth2), !Tokens),
    Result = map((func(W) = {W, MaybeWidth2}), WidthResult).

:- pred parse_width_suffix(parse_res(pz_data_width)::out,
    pzt_tokens::in, pzt_tokens::out) is det.

parse_width_suffix(Result, !Tokens) :-
    match_token(colon, MatchColon, !Tokens),
    parse_data_width(Result0, !Tokens),
    ( MatchColon = ok(_),
        Result = Result0
    ; MatchColon = error(C, G, E),
        Result = error(C, G, E)
    ).

:- pred parse_data_width(parse_res(pz_data_width)::out,
    pzt_tokens::in, pzt_tokens::out) is det.

parse_data_width(Result, !Tokens) :-
    get_context(!.Tokens, Context),
    next_token("data width", TokenResult, !Tokens),
    ( TokenResult = ok(token_and_string(Token, TokenString)),
        ( if token_is_data_width(Token, Width) then
            Result = ok(Width)
        else
            Result = error(Context, TokenString, "data width")
        )
    ; TokenResult = error(C, G, E),
        Result = error(C, G, E)
    ).

:- pred parse_qname(parse_res(symbol)::out,
    pzt_tokens::in, pzt_tokens::out) is det.

parse_qname(Result, !Tokens) :-
    StartTokens = !.Tokens,
    parse_ident(IdentResult, !Tokens),
    ( IdentResult = ok(Ident),
        UnQualTokens = !.Tokens,
        match_token(period, MatchPeriod, !Tokens),
        parse_ident(Ident2Result, !Tokens),
        ( if
            MatchPeriod = ok(_),
            Ident2Result = ok(Ident2)
        then
            Result = ok(symbol([Ident], Ident2))
        else
            !:Tokens = UnQualTokens,
            Result = ok(symbol(Ident))
        )
    ; IdentResult = error(C, G, E),
        !:Tokens = StartTokens,
        Result = error(C, G, E)
    ).

:- pred parse_ident(parse_res(string)::out,
    pzt_tokens::in, pzt_tokens::out) is det.

parse_ident(Result, !Tokens) :-
    match_token(identifier, Result0, !Tokens),
    ( Result0 = ok(String),
        Result = ok(String)
    ; Result0 = error(C, G, E),
        Result = error(C, G, E)
    ).

:- pred parse_number(parse_res(int)::out,
    pzt_tokens::in, pzt_tokens::out) is det.

parse_number(Result, !Tokens) :-
    match_token(number, Result0, !Tokens),
    Result = map(det_to_int, Result0).

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
