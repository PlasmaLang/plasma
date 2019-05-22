%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module pzt_parse.
%
% Parse the PZ textual representation.
%
% Copyright (C) 2015, 2017-2019 Plasma Team
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

:- import_module common_types.
:- import_module context.
:- import_module lex.
:- import_module parse_util.
:- import_module parsing.
:- import_module pz.
:- import_module pz.code.
:- import_module q_name.

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
    --->    import
    ;       proc
    ;       block
    ;       struct
    ;       data
    ;       array
    ;       closure
    ;       global_env
    ;       entry
    ;       initialise_
    ;       finalise_
    ;       jmp
    ;       cjmp
    ;       call
    ;       tcall
    ;       roll
    ;       pick
    ;       alloc
    ;       make_closure
    ;       load
    ;       load_named
    ;       store
    % TODO: we can probably remove the w_ptr token.
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
        ("import"           -> return(import)),
        ("proc"             -> return(proc)),
        ("block"            -> return(block)),
        ("struct"           -> return(struct)),
        ("data"             -> return(data)),
        ("array"            -> return(array)),
        ("closure"          -> return(closure)),
        ("global_env"       -> return(global_env)),
        ("entry"            -> return(entry)),
        ("initialise"       -> return(initialise_)),
        ("initialize"       -> return(initialise_)),
        ("finalise"         -> return(finalise_)),
        ("finalize"         -> return(finalise_)),
        ("jmp"              -> return(jmp)),
        ("cjmp"             -> return(cjmp)),
        ("call"             -> return(call)),
        ("tcall"            -> return(tcall)),
        ("roll"             -> return(roll)),
        ("pick"             -> return(pick)),
        ("alloc"            -> return(alloc)),
        ("make_closure"     -> return(make_closure)),
        ("load"             -> return(load)),
        ("load_named"       -> return(load_named)),
        ("store"            -> return(store)),
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

:- pred ignore_tokens(token_basic::in) is semidet.

ignore_tokens(whitespace).
ignore_tokens(comment).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- pred parse_pzt(pzt_tokens::in, result(asm, read_src_error)::out)
    is det.

parse_pzt(Tokens, Result) :-
    zero_or_more_last_error(or([parse_import, parse_proc, parse_struct,
            parse_data, parse_closure, parse_entry]),
        ok(Items), LastError, Tokens, EmptyTokens),
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

%-----------------------------------------------------------------------%

:- pred parse_struct(parse_res(asm_item)::out,
    pzt_tokens::in, pzt_tokens::out) is det.

parse_struct(Result, !Tokens) :-
    get_context(!.Tokens, Context),
    match_token(struct, MatchStruct, !Tokens),
    ( MatchStruct = ok(_),
        parse_ident(IdentResult, !Tokens),
        within(open_curly, one_or_more(parse_width), close_curly,
            FieldsResult, !Tokens),
        match_token(semicolon, MatchSemi, !Tokens),
        ( if
            IdentResult = ok(Ident),
            FieldsResult = ok(Fields),
            MatchSemi = ok(_)
        then
            Result = ok(asm_item(q_name(Ident), Context,
                asm_struct(Fields)))
        else
            Result = combine_errors_3(IdentResult, FieldsResult, MatchSemi)
        )
    ; MatchStruct = error(C, G, E),
        Result = error(C, G, E)
    ).

%-----------------------------------------------------------------------%

:- pred parse_data(parse_res(asm_item)::out,
    pzt_tokens::in, pzt_tokens::out) is det.

parse_data(Result, !Tokens) :-
    get_context(!.Tokens, Context),
    match_token(data, MatchData, !Tokens),
    parse_ident(IdentResult, !Tokens),
    match_token(equals, MatchEquals, !Tokens),
    parse_data_type(TypeResult, !Tokens),
    parse_data_values(ValuesResult, !Tokens),
    match_token(semicolon, MatchSemi, !Tokens),
    ( if
        MatchData = ok(_),
        IdentResult = ok(Ident),
        MatchEquals = ok(_),
        TypeResult = ok(Type),
        ValuesResult = ok(Values),
        MatchSemi = ok(_)
    then
        Result = ok(asm_item(q_name(Ident), Context, asm_data(Type, Values)))
    else
        Result = combine_errors_6(MatchData, IdentResult, MatchEquals,
            TypeResult, ValuesResult, MatchSemi)
    ).

:- pred parse_data_type(parse_res(asm_data_type)::out,
    pzt_tokens::in, pzt_tokens::out) is det.

parse_data_type(Result, !Tokens) :-
    % Only arrays are implemented.
    or([parse_data_type_array, parse_data_type_struct], Result, !Tokens).

:- pred parse_data_type_array(parse_res(asm_data_type)::out,
    pzt_tokens::in, pzt_tokens::out) is det.

parse_data_type_array(Result, !Tokens) :-
    match_tokens([array, open_paren], StartMatch, !Tokens),
    parse_width(WidthResult, !Tokens),
    match_token(close_paren, CloseMatch, !Tokens),
    ( if
        StartMatch = ok(_),
        WidthResult = ok(Width),
        CloseMatch = ok(_)
    then
        Result = ok(asm_dtype_array(Width))
    else
        Result = combine_errors_3(StartMatch, WidthResult, CloseMatch)
    ).

:- pred parse_data_type_struct(parse_res(asm_data_type)::out,
    pzt_tokens::in, pzt_tokens::out) is det.

parse_data_type_struct(Result, !Tokens) :-
    parse_ident(IdentResult, !Tokens),
    ( IdentResult = ok(Ident),
        Result = ok(asm_dtype_struct(Ident))
    ; IdentResult = error(C, G, E),
        Result = error(C, G, E)
    ).

:- pred parse_data_values(parse_res(list(asm_data_value))::out,
    pzt_tokens::in, pzt_tokens::out) is det.

parse_data_values(Result, !Tokens) :-
    within(open_curly,
        zero_or_more(or([
            parse_data_value_num,
            parse_data_value_name])),
        close_curly, Result, !Tokens).

:- pred parse_data_value_num(parse_res(asm_data_value)::out,
    pzt_tokens::in, pzt_tokens::out) is det.

parse_data_value_num(Result, !Tokens) :-
    parse_number(NumResult, !Tokens),
    Result = map((func(Num) = asm_dvalue_num(Num)), NumResult).

:- pred parse_data_value_name(parse_res(asm_data_value)::out,
    pzt_tokens::in, pzt_tokens::out) is det.

parse_data_value_name(Result, !Tokens) :-
    parse_qname(NameResult, !Tokens),
    Result = map((func(Name) = asm_dvalue_name(Name)), NameResult).

%-----------------------------------------------------------------------%

:- pred parse_closure(parse_res(asm_item)::out,
    pzt_tokens::in, pzt_tokens::out) is det.

parse_closure(Result, !Tokens) :-
    get_context(!.Tokens, Context),
    match_token(closure, ClosureMatch, !Tokens),
    parse_qname(QNameResult, !Tokens),
    match_token(equals, EqualsMatch, !Tokens),
    parse_ident(ProcResult, !Tokens),
    parse_ident(DataResult, !Tokens),
    match_token(semicolon, SemicolonMatch, !Tokens),
    ( if
        ClosureMatch = ok(_),
        QNameResult = ok(QName),
        EqualsMatch = ok(_),
        ProcResult = ok(Proc),
        DataResult = ok(Data),
        SemicolonMatch = ok(_)
    then
        Closure = asm_closure(Proc, Data),
        Result = ok(asm_item(QName, Context, Closure))
    else
        Result = combine_errors_6(ClosureMatch, QNameResult, EqualsMatch,
            ProcResult, DataResult, SemicolonMatch)
    ).

%-----------------------------------------------------------------------%

:- pred parse_entry(parse_res(asm_item)::out,
    pzt_tokens::in, pzt_tokens::out) is det.

parse_entry(Result, !Tokens) :-
    get_context(!.Tokens, Context),
    match_token(entry, MatchEntry, !Tokens),
    parse_qname(NameResult, !Tokens),
    match_token(semicolon, MatchSemicolon, !Tokens),
    ( if
        MatchEntry = ok(_),
        NameResult = ok(Name),
        MatchSemicolon = ok(_)
    then
        Result = ok(asm_entrypoint(Context, Name))
    else
        Result = combine_errors_3(MatchEntry, NameResult, MatchSemicolon)
    ).

%-----------------------------------------------------------------------%

:- pred parse_import(parse_res(asm_item)::out,
    pzt_tokens::in, pzt_tokens::out) is det.

parse_import(Result, !Tokens) :-
    get_context(StartTokens, Context),
    StartTokens = !.Tokens,
    match_token(import, MatchImport, !Tokens),
    parse_qname(QNameResult, !Tokens),
    parse_sig(SigResult, !Tokens),
    match_token(semicolon, MatchSemicolon, !Tokens),
    ( if
        MatchImport = ok(_),
        QNameResult = ok(QName),
        SigResult = ok(Sig),
        MatchSemicolon = ok(_)
    then
        Result = ok(asm_item(QName, Context, asm_import(Sig)))
    else
        !:Tokens = StartTokens,
        Result = combine_errors_4(MatchImport, QNameResult, SigResult,
            MatchSemicolon)
    ).

%-----------------------------------------------------------------------%

:- pred parse_proc(parse_res(asm_item)::out,
    pzt_tokens::in, pzt_tokens::out) is det.

parse_proc(Result, !Tokens) :-
    get_context(StartTokens, Context),
    StartTokens = !.Tokens,
    match_token(proc, MatchProc, !Tokens),
    parse_qname(QNameResult, !Tokens),
    parse_sig(SigResult, !Tokens),
    parse_body(BodyResult, !Tokens),
    match_token(semicolon, MatchSemicolon, !Tokens),
    ( if
        MatchProc = ok(_),
        QNameResult = ok(QName),
        SigResult = ok(Sig),
        BodyResult = ok(Body),
        MatchSemicolon = ok(_)
    then
        Result = ok(asm_item(QName, Context, asm_proc(Sig, Body)))
    else
        !:Tokens = StartTokens,
        Result = combine_errors_5(MatchProc, QNameResult, SigResult,
            BodyResult, MatchSemicolon)
    ).

:- pred parse_sig(parse_res(pz_signature)::out,
    pzt_tokens::in, pzt_tokens::out) is det.

parse_sig(Result, !Tokens) :-
    get_context(!.Tokens, Context),
    match_token(open_paren, MatchOpen, !Tokens),
    zero_or_more(parse_width, ok(Inputs), !Tokens),
    match_token(dash, MatchDash, !Tokens),
    zero_or_more(parse_width, ok(Outputs), !Tokens),
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
    or([parse_ident_instr,
        parse_number_instr,
        parse_token_ident_instr(jmp, (func(Dest) = pzti_jmp(Dest))),
        parse_token_ident_instr(cjmp, (func(Dest) = pzti_cjmp(Dest))),
        parse_token_qname_instr(call, (func(Dest) = pzti_call(Dest))),
        parse_token_qname_instr(tcall, (func(Dest) = pzti_tcall(Dest))),
        parse_token_ident_instr(alloc, (func(Struct) = pzti_alloc(Struct))),
        parse_token_qname_instr(make_closure,
            (func(Proc) = pzti_make_closure(Proc))),
        parse_loadstore_instr,
        parse_load_named_instr,
        parse_imm_instr],
        Result, !Tokens).

:- pred parse_ident_instr(parse_res(pzt_instruction_code)::out,
    pzt_tokens::in, pzt_tokens::out) is det.

parse_ident_instr(Result, !Tokens) :-
    parse_ident(Result0, !Tokens),
    Result = map((func(S) = pzti_word(S)), Result0).

:- pred parse_number_instr(parse_res(pzt_instruction_code)::out,
    pzt_tokens::in, pzt_tokens::out) is det.

parse_number_instr(Result, !Tokens) :-
    parse_number(ResNumber, !Tokens),
    Result = map((func(N) = pzti_load_immediate(N)), ResNumber).

:- pred parse_token_ident_instr(token_basic,
    func(string) = pzt_instruction_code,
    parse_res(pzt_instruction_code), pzt_tokens, pzt_tokens).
:- mode parse_token_ident_instr(in, func(in) = (out) is det, out, in, out)
    is det.

parse_token_ident_instr(Token, F, Result, !Tokens) :-
    parse_token_something_instr(Token, parse_ident, F, Result, !Tokens).

:- pred parse_token_qname_instr(token_basic,
    func(q_name) = pzt_instruction_code,
    parse_res(pzt_instruction_code), pzt_tokens, pzt_tokens).
:- mode parse_token_qname_instr(in, func(in) = (out) is det, out, in, out)
    is det.

parse_token_qname_instr(Token, F, Result, !Tokens) :-
    parse_token_something_instr(Token, parse_qname, F, Result, !Tokens).

:- pred parse_token_something_instr(token_basic,
    pred(parse_res(T), pzt_tokens, pzt_tokens),
    func(T) = pzt_instruction_code,
    parse_res(pzt_instruction_code), pzt_tokens, pzt_tokens).
:- mode parse_token_something_instr(in, pred(out, in, out) is det,
    func(in) = (out) is det, out, in, out) is det.

parse_token_something_instr(Token, Parse, Convert, Result, !Tokens) :-
    match_token(Token, MatchToken, !Tokens),
    Parse(SomethingResult, !Tokens),
    ( if
        MatchToken = ok(_),
        SomethingResult = ok(Something)
    then
        Result = ok(Convert(Something))
    else
        Result = combine_errors_2(MatchToken, SomethingResult)
    ).

:- pred parse_loadstore_instr(parse_res(pzt_instruction_code)::out,
    pzt_tokens::in, pzt_tokens::out) is det.

parse_loadstore_instr(Result, !Tokens) :-
    get_context(!.Tokens, Context),
    next_token("instruction", MatchInstr, !Tokens),
    ( MatchInstr = ok(token_and_string(Instr, InstrString)),
        ( if
            ( Instr = load
            ; Instr = store
            )
        then
            parse_ident(StructResult, !Tokens),
            parse_number(FieldNoResult, !Tokens),
            ( if
                StructResult = ok(Struct),
                FieldNoResult = ok(FieldNo)
            then
                ( Instr = load,
                    Result = ok(pzti_load(Struct, field_num(FieldNo)))
                ; Instr = store,
                    Result = ok(pzti_store(Struct, field_num(FieldNo)))
                )
            else
                Result = combine_errors_2(StructResult, FieldNoResult)
            )
        else
            Result = error(Context, InstrString, "instruction")
        )
    ; MatchInstr = error(C, G, E),
        Result = error(C, G, E)
    ).

:- pred parse_load_named_instr(parse_res(pzt_instruction_code)::out,
    pzt_tokens::in, pzt_tokens::out) is det.

parse_load_named_instr(Result, !Tokens) :-
    get_context(!.Tokens, Context),
    next_token("instruction", MatchLoad, !Tokens),
    ( MatchLoad = ok(token_and_string(Instr, InstrString)),
        ( if Instr = load_named then
            parse_qname(NameResult, !Tokens),
            ( NameResult = ok(Name),
                Result = ok(pzti_load_named(Name))
            ; NameResult = error(C, G, E),
                Result = error(C, G, E)
            )
        else
            Result = error(Context, InstrString, "instruction")
        )
    ; MatchLoad = error(C, G, E),
        Result = error(C, G, E)
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

:- pred parse_full_width_suffix(parse_res({pz_width, maybe(pz_width)})::out,
    pzt_tokens::in, pzt_tokens::out) is det.

parse_full_width_suffix(Result, !Tokens) :-
    parse_width_suffix(WidthResult, !Tokens),
    optional(parse_width_suffix, ok(MaybeWidth2), !Tokens),
    Result = map((func(W) = {W, MaybeWidth2}), WidthResult).

:- pred parse_width_suffix(parse_res(pz_width)::out,
    pzt_tokens::in, pzt_tokens::out) is det.

parse_width_suffix(Result, !Tokens) :-
    match_token(colon, MatchColon, !Tokens),
    parse_width(Result0, !Tokens),
    ( MatchColon = ok(_),
        Result = Result0
    ; MatchColon = error(C, G, E),
        Result = error(C, G, E)
    ).

%-----------------------------------------------------------------------%

:- pred parse_width(parse_res(pz_width)::out,
    pzt_tokens::in, pzt_tokens::out) is det.

parse_width(Result, !Tokens) :-
    get_context(!.Tokens, Context),
    next_token("data width", TokenResult, !Tokens),
    ( TokenResult = ok(token_and_string(Token, TokenString)),
        ( if token_is_width(Token, Width) then
            Result = ok(Width)
        else
            Result = error(Context, TokenString, "data width")
        )
    ; TokenResult = error(C, G, E),
        Result = error(C, G, E)
    ).

:- pred parse_qname(parse_res(q_name)::out,
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
            Result = ok(q_name([Ident], Ident2))
        else
            !:Tokens = UnQualTokens,
            Result = ok(q_name(Ident))
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

:- pred token_is_width(token_basic::in, pz_width::out) is semidet.

token_is_width(w,      pzw_fast).
token_is_width(w8,     pzw_8).
token_is_width(w16,    pzw_16).
token_is_width(w32,    pzw_32).
token_is_width(w64,    pzw_64).
token_is_width(w_ptr,  pzw_ptr).
token_is_width(ptr,    pzw_ptr).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
