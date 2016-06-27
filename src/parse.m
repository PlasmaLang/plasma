%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module parse.
%
% Copyright (C) 2016 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
% Plasma parser
%
%-----------------------------------------------------------------------%

:- interface.

:- import_module io.
:- import_module string.

:- import_module ast.
:- import_module parse_util.
:- import_module result.

%-----------------------------------------------------------------------%

:- pred parse(string::in, result(plasma_ast, read_src_error)::out,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module int.
:- import_module list.
:- import_module maybe.
:- import_module require.
:- import_module solutions.
:- import_module unit.

:- import_module ast.
:- import_module context.
:- import_module lex.
:- import_module parsing.
:- import_module q_name.

%-----------------------------------------------------------------------%

parse(Filename, Result, !IO) :-
    parse_file(Filename, lexemes, ignore_tokens, parse_plasma, Result, !IO).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- type token_type
    --->    module_
    ;       export
    ;       import
    ;       type_
    ;       func_
    ;       using
    ;       observing
    ;       as
    ;       return
    ;       ident_lower
    ;       ident_upper
    ;       number
    ;       string
    ;       l_curly
    ;       r_curly
    ;       l_paren
    ;       r_paren
    ;       l_square
    ;       r_square
    ;       l_square_colon
    ;       r_square_colon
    ;       semicolon
    ;       colon
    ;       d_colon
    ;       comma
    ;       period
    ;       plus
    ;       minus
    ;       star
    ;       slash
    ;       percent
    ;       amp
    ;       bar
    ;       caret
    ;       tilda
    ;       bang
    ;       double_l_angle
    ;       double_r_angle
    ;       double_plus
    ;       equals
    ;       r_arrow
    ;       double_l_arrow
    ;       newline
    ;       comment
    ;       whitespace
    ;       eof.

:- func lexemes = list(lexeme(lex_token(token_type))).

lexemes = [
        ("module"           -> return(module_)),
        ("export"           -> return(export)),
        ("import"           -> return(import)),
        ("type"             -> return(type_)),
        ("func"             -> return(func_)),
        ("using"            -> return(using)),
        ("observing"        -> return(observing)),
        ("as"               -> return(as)),
        ("return"           -> return(return)),
        ("{"                -> return(l_curly)),
        ("}"                -> return(r_curly)),
        ("("                -> return(l_paren)),
        (")"                -> return(r_paren)),
        ("["                -> return(l_square)),
        ("]"                -> return(r_square)),
        ("[:"               -> return(l_square_colon)),
        (":]"               -> return(r_square_colon)),
        (";"                -> return(semicolon)),
        (":"                -> return(colon)),
        ("::"               -> return(d_colon)),
        (","                -> return(comma)),
        ("."                -> return(period)),
        ("+"                -> return(plus)),
        ("-"                -> return(minus)),
        ("*"                -> return(star)),
        ("/"                -> return(slash)),
        ("%"                -> return(percent)),
        ("&"                -> return(amp)),
        ("|"                -> return(bar)),
        ("^"                -> return(caret)),
        ("~"                -> return(tilda)),
        ("!"                -> return(bang)),
        ("<<"               -> return(double_l_angle)),
        (">>"               -> return(double_r_angle)),
        ("++"               -> return(double_plus)),
        ("="                -> return(equals)),
        ("->"               -> return(r_arrow)),
        ("<="               -> return(double_l_arrow)),
        (nat                -> return(number)),
        (identifier_lower   -> return(ident_lower)),
        (identifier_upper   -> return(ident_upper)),
        % TODO: don't terminate the string on a \" escape sequence.
        ("\"" ++ *(anybut("\"")) ++ "\""
                            -> return(string)),

        (("#" ++ *(anybut("\n")))
                            -> return(comment)),
        ("\n"               -> return(newline)),
        (any(" \t\v\f")     -> return(whitespace))
    ].

:- func identifier_lower = regexp.

identifier_lower = any("abcdefghijklmnopqrstuvwxyz_") ++ *(ident).

:- func identifier_upper = regexp.

identifier_upper = (any("ABCDEFGHIJKLMNOPQRSTUVWXYZ") or ('_')) ++ *(ident).

:- pred ignore_tokens(lex_token(token_type)::in) is semidet.

ignore_tokens(lex_token(whitespace, _)).
ignore_tokens(lex_token(newline, _)).
ignore_tokens(lex_token(comment, _)).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- type tokens == list(token(token_type)).

:- pred parse_plasma(tokens::in, result(plasma_ast, read_src_error)::out)
    is det.

    % I will show the EBNF in comments.  NonTerminals appear in
    % CamelCase and terminals appear in lower_underscore_case.
    %
    % Plasma := ModuleDecl ToplevelItem*
    %
    % ModuleDecl := module ident
    %
parse_plasma(!.Tokens, Result) :-
    match_token(module_, ModuleMatch, !Tokens),
    parse_ident(NameResult, !Tokens),
    zero_or_more_last_error(parse_entry, ok(Items), LastError, !Tokens),
    ( if
        ModuleMatch = ok(_),
        NameResult = ok(Name)
    then
        ( !.Tokens = [],
            Result = ok(plasma_ast(Name, Items))
        ; !.Tokens = [token(Tok, _, TokCtxt) | _],
            LastError = error(LECtxt, Got, Expect),
            ( if compare((<), LECtxt, TokCtxt) then
                Result = return_error(TokCtxt,
                    rse_parse_junk_at_end(string(Tok)))
            else
                Result = return_error(LECtxt, rse_parse_error(Got, Expect))
            )
        )
    else
        Result0 = combine_errors_2(ModuleMatch, NameResult) `with_type`
            parse_res(unit),
        ( Result0 = error(C, G, E),
            Result = return_error(C, rse_parse_error(G, E))
        ; Result0 = ok(_),
            unexpected($file, $pred, "ok/1, expecting error/1")
        )
    ).

    % ToplevelItem := ExportDirective
    %               | ImportDirective
    %               | FuncDefinition
    %               | TypeDefinition
    %
:- pred parse_entry(parse_res(past_entry)::out, tokens::in, tokens::out) is det.

parse_entry(Result, !Tokens) :-
    or([parse_export, parse_import, parse_func, parse_type], Result, !Tokens).

    % ExportDirective := export IdentList
    %                  | export '*'
    %
:- pred parse_export(parse_res(past_entry)::out, tokens::in, tokens::out)
    is det.

parse_export(Result, !Tokens) :-
    match_token(export, ExportMatch, !Tokens),
    ( ExportMatch = ok(_),
        or([parse_export_wildcard, parse_export_named], Result0, !Tokens),
        Result = Result0
    ; ExportMatch = error(C, G, E),
        Result = error(C, G, E)
    ).

:- pred parse_export_wildcard(parse_res(past_entry)::out,
    tokens::in, tokens::out) is det.

parse_export_wildcard(Result, !Tokens) :-
    match_token(star, Match, !Tokens),
    Result = map((func(_) = past_export(export_all)), Match).

:- pred parse_export_named(parse_res(past_entry)::out,
    tokens::in, tokens::out) is det.

parse_export_named(Result, !Tokens) :-
    parse_ident_list(ExportsResult, !Tokens),
    Result = map((func(Exports) = past_export(export_some(Exports))),
        ExportsResult).

    % ImportDirective := import QualifiedIdent
    %                  | import QualifiedIdent . *
    %                  | import QualifiedIdent as ident
    %
    % To aide parsing without lookahead we also accept, but discard
    % later:
    %                  | import QualifiedIdent . * as ident
    %
:- pred parse_import(parse_res(past_entry)::out, tokens::in, tokens::out)
    is det.

parse_import(Result, !Tokens) :-
    match_token(import, ImportMatch, !Tokens),
    parse_import_name(NameResult, !Tokens),
    ( if
        ImportMatch = ok(_),
        NameResult = ok(Name)
    then
        TokensAs = !.Tokens,
        match_token(as, AsMatch, !Tokens),
        parse_ident(AsIdentResult, !Tokens),
        ( AsMatch = ok(_),
            ( AsIdentResult = ok(AsIdent),
                Result = ok(past_import(Name, yes(AsIdent)))
            ; AsIdentResult = error(C, G, E),
                Result = error(C, G, E)
            )
        ; AsMatch = error(_, _, _),
            Result = ok(past_import(Name, no)),
            !:Tokens = TokensAs
        )
    else
        Result = combine_errors_2(ImportMatch, NameResult)
    ).

:- pred parse_import_name(parse_res(import_name)::out, tokens::in, tokens::out)
    is det.

parse_import_name(Result, !Tokens) :-
    parse_ident(HeadResult, !Tokens),
    parse_import_name_2(TailResult, !Tokens),
    ( if
        HeadResult = ok(Head),
        TailResult = ok(Tail)
    then
        Result = ok(dot(Head, Tail))
    else
        Result = combine_errors_2(HeadResult, TailResult)
    ).

:- pred parse_import_name_2(parse_res(import_name_2)::out,
    tokens::in, tokens::out) is det.

parse_import_name_2(Result, !Tokens) :-
    BeforeTokens = !.Tokens,
    match_token(period, MatchDot, !Tokens),
    ( MatchDot = ok(_),
        AfterDotTokens = !.Tokens,
        match_token(star, MatchStar, !Tokens),
        ( MatchStar = ok(_),
            Result = ok(star)
        ; MatchStar = error(_, _, _),
            !:Tokens = AfterDotTokens,
            parse_ident(IdentResult, !Tokens),
            parse_import_name_2(TailResult, !Tokens),
            ( if
                IdentResult = ok(Ident),
                TailResult = ok(Tail)
            then
                Result = ok(dot(Ident, Tail))
            else
                Result = combine_errors_2(IdentResult, TailResult)
            )
        )
    ; MatchDot = error(_, _, _),
        !:Tokens = BeforeTokens,
        Result = ok(nil)
    ).

:- pred parse_type(parse_res(past_entry)::out, tokens::in,
    tokens::out) is det.

parse_type(Result, !Tokens) :-
    get_context(!.Tokens, Context),
    match_token(type_, MatchType, !Tokens),
    match_token(ident_upper, NameResult, !Tokens),
    optional(within(l_paren, one_or_more_delimited(comma,
        match_token(ident_lower)), r_paren), ok(MaybeParams), !Tokens),
    match_token(equals, MatchEquals, !Tokens),
    one_or_more_delimited(bar, parse_type_constructor, CtrsResult, !Tokens),
    ( if
        MatchType = ok(_),
        NameResult = ok(Name),
        MatchEquals = ok(_),
        CtrsResult = ok(Constructors)
    then
        Params = maybe_list(MaybeParams),
        Result = ok(past_type(Name, Params, Constructors, Context))
    else
        Result = combine_errors_4(MatchType, NameResult, MatchEquals,
            CtrsResult)
    ).

:- pred parse_type_constructor(parse_res(pat_constructor)::out, tokens::in,
    tokens::out) is det.

parse_type_constructor(Result, !Tokens) :-
    get_context(!.Tokens, Context),
    match_token(ident_upper, CNameResult, !Tokens),
    optional(within(l_paren,
        one_or_more_delimited(comma, parse_type_ctr_field), r_paren),
        ok(MaybeFields), !Tokens),
    ( CNameResult = ok(CName),
        Result = ok(pat_constructor(CName, maybe_list(MaybeFields), Context))
    ; CNameResult = error(C, G, E),
        Result = error(C, G, E)
    ).

:- pred parse_type_ctr_field(parse_res(pat_field)::out, tokens::in,
    tokens::out) is det.

parse_type_ctr_field(Result, !Tokens) :-
    get_context(!.Tokens, Context),
    parse_ident(NameResult, !Tokens),
    match_token(d_colon, MatchColon, !Tokens),
    parse_type_expr(TypeResult, !Tokens),
    ( if
        NameResult = ok(Name),
        MatchColon = ok(_),
        TypeResult = ok(Type)
    then
        Result = ok(pat_field(Name, Type, Context))
    else
        Result = combine_errors_3(NameResult, MatchColon, TypeResult)
    ).

    % TypeExpr := Type
    %           | Type '(' TypeExpr ( , TypeExpr )* ')'
    %
    % Type := QualifiedIden
    %
    % TODO: Update to respect case of type names/vars
    %
:- pred parse_type_expr(parse_res(past_type_expr)::out,
    tokens::in, tokens::out) is det.

parse_type_expr(Result, !Tokens) :-
    or([parse_type_var, parse_type_construction], Result, !Tokens).

:- pred parse_type_var(parse_res(past_type_expr)::out,
    tokens::in, tokens::out) is det.

parse_type_var(Result, !Tokens) :-
    get_context(!.Tokens, Context),
    match_token(ident_lower, Result0, !Tokens),
    Result = map((func(S) = past_type_var(S, Context)), Result0).

:- pred parse_type_construction(parse_res(past_type_expr)::out,
    tokens::in, tokens::out) is det.

parse_type_construction(Result, !Tokens) :-
    get_context(!.Tokens, Context),
    parse_qual_ident(ident_upper, ConstructorResult, !Tokens),
    % TODO: We could generate more helpful parse errors here, for example by
    % returng the error from within the optional thing if the l_paren is
    % seen.
    optional(within(l_paren, one_or_more_delimited(comma, parse_type_expr),
        r_paren), ok(MaybeArgs), !Tokens),
    ( ConstructorResult = ok(qual_ident(Qualifiers, Name)),
        ( MaybeArgs = no,
            Args = []
        ; MaybeArgs = yes(Args)
        ),
        Result = ok(past_type(Qualifiers, Name, Args, Context))
    ; ConstructorResult = error(C, G, E),
        Result = error(C, G, E)
    ).

    % FuncDefinition := 'func' ident '(' ( Param ( , Param )* )? ')' ->
    %                       TypeExpr Using* Block
    % Param := ident : TypeExpr
    % Using := using IdentList
    %        | observing IdentList
:- pred parse_func(parse_res(past_entry)::out, tokens::in,
    tokens::out) is det.

parse_func(Result, !Tokens) :-
    get_context(!.Tokens, Context),
    match_token(func_, MatchFunc, !Tokens),
    ( MatchFunc = ok(_),
        parse_ident(NameResult, !Tokens),
        parse_param_list(ParamsResult, !Tokens),
        match_token(r_arrow, MatchRArrow, !Tokens),
        parse_type_expr(ReturnTypeResult, !Tokens),
        zero_or_more(parse_using, ok(Usings), !Tokens),
        parse_block(BodyResult, !Tokens),
        ( if
            NameResult = ok(Name),
            ParamsResult = ok(Params),
            MatchRArrow = ok(_),
            ReturnTypeResult = ok(ReturnType),
            BodyResult = ok(Body)
        then
            Result = ok(past_function(Name, Params, ReturnType,
                condense(Usings), Body, Context))
        else
            Result = combine_errors_5(NameResult, ParamsResult, MatchRArrow,
                ReturnTypeResult, BodyResult)
        )
    ; MatchFunc = error(C, G, E),
        Result = error(C, G, E)
    ).

:- pred parse_param_list(parse_res(list(past_param))::out,
    tokens::in, tokens::out) is det.

parse_param_list(Result, !Tokens) :-
    within(l_paren, zero_or_more_delimited(comma, parse_param), r_paren,
        Result, !Tokens).

:- pred parse_param(parse_res(past_param)::out,
    tokens::in, tokens::out) is det.

parse_param(Result, !Tokens) :-
    parse_ident(NameResult, !Tokens),
    match_token(d_colon, ColonMatch, !Tokens),
    parse_type_expr(TypeResult, !Tokens),
    ( if
        NameResult = ok(Name),
        ColonMatch = ok(_),
        TypeResult = ok(Type)
    then
        Result = ok(past_param(Name, Type))
    else
        Result = combine_errors_3(NameResult, ColonMatch, TypeResult)
    ).

:- pred parse_using(parse_res(list(past_using))::out,
    tokens::in, tokens::out) is det.

parse_using(Result, !Tokens) :-
    get_context(!.Tokens, Context),
    next_token("Using or observing clause", UsingObservingResult, !Tokens),
    ( UsingObservingResult = ok(token_and_string(UsingObserving, TokenString)),
        ( if
            ( UsingObserving = using,
                UsingType = ut_using
            ; UsingObserving = observing,
                UsingType = ut_observing
            )
        then
            parse_ident_list(ResourcesResult, !Tokens),
            Result = map((func(Rs) =
                    map((func(R) = past_using(UsingType, R)), Rs)
                ), ResourcesResult)
        else
            Result = error(Context, TokenString, "Using or observing clause")
        )
    ; UsingObservingResult = error(C, G, E),
        Result = error(C, G, E)
    ).

:- pred parse_block(parse_res(list(past_statement))::out,
    tokens::in, tokens::out) is det.

parse_block(Result, !Tokens) :-
    within_use_last_error(l_curly, zero_or_more_last_error(parse_statement),
        r_curly, Result, !Tokens).

    % Statement := CallInStmt
    %            | 'return' TupleExpr
    %            | IdentList '=' TupleExpr
    %            | Ident ArraySubscript '<=' Expr
    %
    % CallInStmt := ExprPart '!'? '(' Expr ( , Expr )* ')'
    %
    % The '!' is an optional part of the grammer even though no sensible
    % program would omit it in this context (either it would be an error
    % because the callee uses a resource or the compiler would optimise the
    % call away).
    %
:- pred parse_statement(parse_res(past_statement)::out,
    tokens::in, tokens::out) is det.

parse_statement(Result, !Tokens) :-
    or([parse_stmt_call, parse_stmt_return,
            parse_stmt_asign, parse_stmt_array_set],
        Result, !Tokens).

:- pred parse_stmt_call(parse_res(past_statement)::out,
    tokens::in, tokens::out) is det.

parse_stmt_call(Result, !Tokens) :-
    get_context(!.Tokens, Context),
    parse_call_in_stmt(CallResult, !Tokens),
    ( CallResult = ok(Call),
        Result = ok(ps_call(Call, Context))
    ; CallResult = error(C, G, E),
        Result = error(C, G, E)
    ).

    % Parse a call as it occurs within a statement.
    %
:- pred parse_call_in_stmt(parse_res(past_call)::out,
    tokens::in, tokens::out) is det.

parse_call_in_stmt(Result, !Tokens) :-
    parse_expr_2(CalleeResult, !Tokens),
    optional(match_token(bang), ok(MaybeBang), !Tokens),
    % TODO: Use last error.
    within(l_paren, zero_or_more_delimited(comma, parse_expr), r_paren,
        ArgsResult, !Tokens),
    ( if
        CalleeResult = ok(Callee),
        ArgsResult = ok(Args)
    then
        ( MaybeBang = no,
            Result = ok(past_call(Callee, Args))
        ; MaybeBang = yes(_),
            Result = ok(past_bang_call(Callee, Args))
        )
    else
        Result = combine_errors_2(CalleeResult, ArgsResult)
    ).

:- pred parse_stmt_return(parse_res(past_statement)::out,
    tokens::in, tokens::out) is det.

parse_stmt_return(Result, !Tokens) :-
    get_context(!.Tokens, Context),
    match_token(return, ReturnMatch, !Tokens),
    zero_or_more_delimited(comma, parse_expr, ok(Vals), !Tokens),
    Result = map((func(_) = ps_return_statement(Vals, Context)),
        ReturnMatch).

:- pred parse_stmt_asign(parse_res(past_statement)::out,
    tokens::in, tokens::out) is det.

parse_stmt_asign(Result, !Tokens) :-
    get_context(!.Tokens, Context),
    parse_ident_list(VarsResult, !Tokens),
    match_token(equals, EqualsMatch, !Tokens),
    one_or_more_delimited(comma, parse_expr, ValsResult, !Tokens),
    ( if
        VarsResult = ok(Vars),
        EqualsMatch = ok(_),
        ValsResult = ok(Vals)
    then
        Result = ok(ps_asign_statement(Vars, no, Vals, Context))
    else
        Result = combine_errors_3(VarsResult, EqualsMatch, ValsResult)
    ).

:- pred parse_stmt_array_set(parse_res(past_statement)::out,
    tokens::in, tokens::out) is det.

parse_stmt_array_set(Result, !Tokens) :-
    get_context(!.Tokens, Context),
    parse_ident(NameResult, !Tokens),
    within(l_square, parse_expr, r_square, IndexResult, !Tokens),
    match_token(double_l_arrow, ArrowMatch, !Tokens),
    parse_expr(ValueResult, !Tokens),
    ( if
        NameResult = ok(Name),
        IndexResult = ok(Index),
        ArrowMatch = ok(_),
        ValueResult = ok(Value)
    then
        Result = ok(ps_array_set_statement(Name, Index, Value, Context))
    else
        Result = combine_errors_4(NameResult, IndexResult, ArrowMatch,
            ValueResult)
    ).

    % Expressions may be:
    %
    % A binary and unary expressions
    %   Expr := Expr BinOp Expr
    %         | UOp Expr
    % A call
    %         | ExprPart '!'? '(' Expr ( , Expr )* ')'
    % An array subscript
    %         | ExprPart '[' Expr ']'
    % A higher precidence expression.
    %         | ExprPart
    %
    % Which may be:
    %   ExprPart := '(' Expr ')'
    % A list or array
    %             | '[' ListExpr ']'
    %             | '[:' TupleExpr? ':]'
    % A value:
    %             | QualifiedIdent
    % A constant:
    %             | const_str
    %             | const_int
    %
    % ListExpr := e
    %           | Expr ( ',' Expr )* ( ':' Expr )?
    %
    % The relative precidences of unary and binary operators is covered in
    % the reference manual
    % http://www.plasmalang.org/docs/plasma_ref.html#_expressions
    %
:- pred parse_expr(parse_res(past_expression)::out,
    tokens::in, tokens::out) is det.

parse_expr(Result, !Tokens) :-
    parse_binary_expr(max_binop_level, Result, !Tokens).

:- pred operator_table(int, token_type, past_bop).
:- mode operator_table(in, in, out) is semidet.
:- mode operator_table(out, out, out) is multi.

operator_table(1, star,             pb_mul).
operator_table(1, slash,            pb_div).
operator_table(1, percent,          pb_mod).
operator_table(2, plus,             pb_add).
operator_table(2, minus,            pb_sub).
operator_table(3, double_l_angle,   pb_lshift).
operator_table(3, double_r_angle,   pb_rshift).
operator_table(4, amp,              pb_and).
operator_table(5, caret,            pb_xor).
operator_table(6, bar,              pb_or).
operator_table(7, double_plus,      pb_concat).

:- func max_binop_level = int.

max_binop_level = Max :-
    solutions((pred(Level::out) is multi :- operator_table(Level, _, _)),
        Levels),
    Max = foldl((func(X, M) = (if X > M then X else M)), Levels, 1).

:- pred parse_binary_expr(int::in, parse_res(past_expression)::out,
    tokens::in, tokens::out) is det.

parse_binary_expr(Level, Result, !Tokens) :-
    ( if Level > 0 then
        parse_binary_expr(Level - 1, ExprLResult, !Tokens),
        ( ExprLResult = ok(ExprL),
            BeforeOpTokens = !.Tokens,
            next_token("operator", OpResult, !Tokens),
            ( if
                OpResult = ok(token_and_string(Op, _)),
                operator_table(Level, Op, EOp)
            then
                parse_binary_expr(Level, ExprRResult, !Tokens),
                ( ExprRResult = ok(ExprR),
                    Result = ok(pe_b_op(ExprL, EOp, ExprR))
                ; ExprRResult = error(C, G, E),
                    Result = error(C, G, E)
                )
            else
                Result = ok(ExprL),
                !:Tokens = BeforeOpTokens
            )
        ; ExprLResult = error(C, G, E),
            Result = error(C, G, E)
        )
    else
        parse_unary_expr(Result, !Tokens)
    ).

:- pred parse_unary_expr(parse_res(past_expression)::out,
    tokens::in, tokens::out) is det.

parse_unary_expr(Result, !Tokens) :-
    StartTokens = !.Tokens,
    next_token("expression", TokenResult, !Tokens),
    ( TokenResult = ok(token_and_string(Token, _)),
        ( if
            ( Token = minus,
                UOp = pu_minus
            ; Token = tilda,
                UOp = pu_not
            )
        then
            parse_unary_expr(ExprResult, !Tokens),
            Result = map((func(E) = pe_u_op(UOp, E)), ExprResult)
        else
            !:Tokens = StartTokens,
            parse_expr_1(Result, !Tokens)
        )
    ; TokenResult = error(C, G, E),
        Result = error(C, G, E)
    ).

    % This precidence level covers calls and array subscriptions.
    %
:- pred parse_expr_1(parse_res(past_expression)::out,
    tokens::in, tokens::out) is det.

parse_expr_1(Result, !Tokens) :-
    parse_expr_2(Part1Result0, !Tokens),
    ( Part1Result0 = ok(Part1),
        parse_expr_part_2(Part1, Result, !Tokens)
    ; Part1Result0 = error(C, G, E),
        Result = error(C, G, E)
    ).

:- pred parse_expr_part_2(past_expression::in, parse_res(past_expression)::out,
    tokens::in, tokens::out) is det.

parse_expr_part_2(Part1, Result, !Tokens) :-
    Part1Tokens = !.Tokens,
    next_token("Call arguments or array subscript", NextResult,
        !Tokens),
    ( NextResult = ok(token_and_string(Next, _)),
        ( if
            require_switch_arms_det [Next]
            ( Next = l_paren,
                parse_call_part2(Part1, Result1, !Tokens)
            ; Next = bang,
                match_token(l_paren, ParenResult, !Tokens),
                ( ParenResult = ok(_),
                    parse_call_part2(Part1, Result0, !Tokens),
                    Result1 = map(make_bang_call, Result0)
                ; ParenResult = error(C, G, E),
                    Result1 = error(C, G, E)
                )
            ; Next = l_square,
                parse_array_subscript_part2(Part1, Result1, !Tokens)
            )
        then
            ( Result1 = ok(Expr),
                parse_expr_part_2(Expr, Result, !Tokens)
            ; Result1 = error(_, _, _),
                !:Tokens = Part1Tokens,
                Result = ok(Part1)
            )
        else
            !:Tokens = Part1Tokens,
            Result = ok(Part1)
        )
    ; NextResult = error(_, _, _),
        !:Tokens = Part1Tokens,
        Result = ok(Part1)
    ).

:- pred parse_expr_2(parse_res(past_expression)::out,
    tokens::in, tokens::out) is det.

parse_expr_2(Result, !Tokens) :-
    or([    parse_const_expr,
            within(l_paren, parse_expr, r_paren),
            within(l_square, parse_list_expr, r_square),
            parse_array_expr,
            parse_expr_symbol
        ], Result, !Tokens).

:- pred parse_const_expr(parse_res(past_expression)::out,
    tokens::in, tokens::out) is det.

parse_const_expr(Result, !Tokens) :-
    ( if parse_string(ok(String), !Tokens) then
        Result = ok(pe_const(pc_string(String)))
    else if parse_number(ok(Num), !Tokens) then
        Result = ok(pe_const(pc_number(Num)))
    else
        get_context(!.Tokens, Context),
        Result = error(Context, "", "expression")
    ).

:- pred parse_array_expr(parse_res(past_expression)::out,
    tokens::in, tokens::out) is det.

parse_array_expr(Result, !Tokens) :-
    within(l_square_colon, zero_or_more_delimited(comma, parse_expr),
        r_square_colon, Result0, !Tokens),
    Result = map((func(Exprs) = pe_array(Exprs)), Result0).

:- pred parse_string(parse_res(string)::out, tokens::in, tokens::out)
    is det.

parse_string(Result, !Tokens) :-
    match_token(string, Result0, !Tokens),
    Result = map(unescape_string_const, Result0).

:- pred parse_number(parse_res(int)::out, tokens::in, tokens::out) is det.

parse_number(Result, !Tokens) :-
    match_token(number, Result0, !Tokens),
    Result = map(det_to_int, Result0).

:- func unescape_string_const(string) = string.

unescape_string_const(S0) = from_char_list(C) :-
    between(S0, 1, length(S0) - 1, S1),
    C1 = to_char_list(S1),
    unescape_string_loop(C1) = C.

:- func unescape_string_loop(list(char)) = list(char).

unescape_string_loop([]) = [].
unescape_string_loop([C | Cs0]) = Cs :-
    ( if C = ('\\') then
        Cs = unescape_string_loop_do_escape(Cs0)
    else
        Cs = [C | unescape_string_loop(Cs0)]
    ).

:- func unescape_string_loop_do_escape(list(char)) = list(char).

unescape_string_loop_do_escape([]) =
    sorry($file, $pred, "Lexer does not support escaping the double quote").
unescape_string_loop_do_escape([C0 | Cs0]) = Cs :-
    ( if escape_char(C0, C1) then
        C = C1
    else
        % Interpret the escaped character as if it was not escaped.
        C = C0
    ),
    Cs = [C | unescape_string_loop(Cs0)].

:- pred escape_char(char::in, char::out) is semidet.

escape_char('n', '\n').
escape_char('r', '\r').
escape_char('t', '\t').
escape_char('v', '\v').
escape_char('f', '\f').
escape_char('b', '\b').
escape_char('\\', '\\').

:- pred parse_list_expr(parse_res(past_expression)::out,
    tokens::in, tokens::out) is det.

parse_list_expr(Result, !Tokens) :-
    StartTokens = !.Tokens,
    one_or_more_delimited(comma, parse_expr, HeadsResult, !Tokens),
    ( HeadsResult = ok(Heads),
        BeforeColonTokens = !.Tokens,
        match_token(colon, MatchColon, !Tokens),
        ( MatchColon = ok(_),
            parse_expr(TailResult, !Tokens),
            ( TailResult = ok(Tail),
                Result = ok(make_cons_list(Heads, Tail))
            ; TailResult = error(C, G, E),
                Result = error(C, G, E)
            )
        ; MatchColon = error(_, _, _),
            !:Tokens = BeforeColonTokens,
            Result = ok(make_cons_list(Heads, pe_const(pc_list_nil)))
        )
    ; HeadsResult = error(_, _, _),
        !:Tokens = StartTokens,
        Result = ok(pe_const(pc_list_nil))
    ).

:- pred parse_expr_symbol(parse_res(past_expression)::out,
    tokens::in, tokens::out) is det.

parse_expr_symbol(Result, !Tokens) :-
    parse_qual_ident_any(QNameResult, !Tokens),
    Result = map((func(qual_ident(Q, N)) = pe_symbol(q_name(Q, N))),
        QNameResult).

:- pred parse_call_part2(past_expression::in, parse_res(past_expression)::out,
    tokens::in, tokens::out) is det.

parse_call_part2(Callee, Result, !Tokens) :-
    zero_or_more_delimited(comma, parse_expr, ok(Args), !Tokens),
    match_token(r_paren, MatchParen, !Tokens),
    ( MatchParen = ok(_),
        Result = ok(pe_call(past_call(Callee, Args)))
    ; MatchParen = error(C, G, E),
        Result = error(C, G, E)
    ).

:- pred parse_array_subscript_part2(past_expression::in,
    parse_res(past_expression)::out, tokens::in, tokens::out) is det.

parse_array_subscript_part2(Expr, Result, !Tokens) :-
    parse_expr(SubscriptResult, !Tokens),
    match_token(r_square, MatchClose, !Tokens),
    ( if
        SubscriptResult = ok(Subscript),
        MatchClose = ok(_)
    then
        Result = ok(pe_b_op(Expr, pb_array_subscript, Subscript))
    else
        Result = combine_errors_2(SubscriptResult, MatchClose)
    ).

:- pred parse_ident_list(parse_res(list(string))::out,
    tokens::in, tokens::out) is det.

parse_ident_list(Result, !Tokens) :-
   one_or_more_delimited(comma, parse_ident, Result, !Tokens).

:- type qual_ident
    --->    qual_ident(list(string), string).

:- pred parse_qual_ident(token_type::in, parse_res(qual_ident)::out,
    tokens::in, tokens::out) is det.

parse_qual_ident(Token, Result, !Tokens) :-
    zero_or_more(parse_qualifier, ok(Qualifiers), !Tokens),
    match_token(Token, IdentResult, !Tokens),
    Result = map((func(S) = qual_ident(Qualifiers, S)), IdentResult).

:- pred parse_qual_ident_any(parse_res(qual_ident)::out,
    tokens::in, tokens::out) is det.

parse_qual_ident_any(Result, !Tokens) :-
    zero_or_more(parse_qualifier, ok(Qualifiers), !Tokens),
    parse_ident(IdentResult, !Tokens),
    Result = map((func(S) = qual_ident(Qualifiers, S)), IdentResult).

:- pred parse_qualifier(parse_res(string)::out,
    tokens::in, tokens::out) is det.

parse_qualifier(Result, !Tokens) :-
    parse_ident(IdentResult, !Tokens),
    match_token(period, DotMatch, !Tokens),
    ( if
        IdentResult = ok(Ident),
        DotMatch = ok(_)
    then
        Result = ok(Ident)
    else
        Result = combine_errors_2(IdentResult, DotMatch)
    ).

:- pred parse_ident(parse_res(string)::out, tokens::in, tokens::out) is det.

parse_ident(Result, !Tokens) :-
    or([match_token(ident_upper), match_token(ident_lower)], Result, !Tokens).

%-----------------------------------------------------------------------%

:- func make_cons_list(list(past_expression), past_expression) =
    past_expression.

make_cons_list([], Tail) = Tail.
make_cons_list([X | Xs], Tail) = List :-
    List0 = make_cons_list(Xs, Tail),
    List = pe_b_op(X, pb_list_cons, List0).

:- func maybe_list(maybe(list(X))) = list(X).

maybe_list(yes(List)) = List.
maybe_list(no) = [].

:- func make_bang_call(past_expression) = past_expression.

make_bang_call(Expr0) = Expr :-
    ( if Expr0 = pe_call(past_call(Callee, Args)) then
        Expr = pe_call(past_bang_call(Callee, Args))
    else
        unexpected($file, $pred, "Not a call")
    ).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
