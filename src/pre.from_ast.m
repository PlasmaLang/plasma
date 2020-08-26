%-----------------------------------------------------------------------%
% Plasma AST symbol resolution
% vim: ts=4 sw=4 et
%
% Copyright (C) 2015-2017, 2019-2020 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
% This module resolves symbols within the Plasma AST returning the pre-core
% representation.
%
%-----------------------------------------------------------------------%
:- module pre.from_ast.
%-----------------------------------------------------------------------%

:- interface.

:- import_module list.
:- import_module map.

:- import_module ast.
:- import_module context.
:- import_module common_types.
:- import_module pre.env.
:- import_module pre.pre_ds.
:- import_module q_name.

%-----------------------------------------------------------------------%

:- pred func_to_pre_func(env::in, q_name::in, list(ast_param)::in,
    list(ast_type_expr)::in, list(ast_block_thing)::in, context::in,
    map(func_id, pre_procedure)::in, map(func_id, pre_procedure)::out) is det.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
:- implementation.

:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module string.

:- import_module util.
:- import_module util.exception.
:- import_module util.mercury.
:- import_module varmap.

%-----------------------------------------------------------------------%

func_to_pre_func(Env, Name, Params, Returns, Body0, Context, !Pre) :-
    % Build body.
    some [!Varmap] (
        !:Varmap = varmap.init,
        env_lookup_function(Env, Name, FuncId),
        ast_to_pre_body(Env, Context, Params, ParamVarsOrWildcards,
            Body0, Body, _, !Varmap),
        Proc = pre_procedure(FuncId, !.Varmap, ParamVarsOrWildcards,
            arity(length(Returns)), Body, Context),
        map.det_insert(FuncId, Proc, !Pre)
    ).

:- pred ast_to_pre_body(env::in, context::in,
    list(ast_param)::in, list(var_or_wildcard(var))::out,
    list(ast_block_thing(context))::in, pre_statements::out,
    set(var)::out, varmap::in, varmap::out) is det.

ast_to_pre_body(Env0, Context, Params, ParamVarsOrWildcards, Body0, Body,
        UseVars, !Varmap) :-
    ParamNames = map((func(ast_param(N, _)) = N), Params),
    ( if
        map_foldl2(do_var_or_wildcard(env_add_and_initlalise_var),
            ParamNames, ParamVarsOrWildcardsPrime, Env0, EnvPrime,
            !Varmap)
    then
        ParamVarsOrWildcards = ParamVarsOrWildcardsPrime,
        Env = EnvPrime
    else
        compile_error($file, $pred, Context,
            "Two or more parameters have the same name")
    ),
    ast_to_pre(Env, Body0, Body, UseVars, !Varmap).

%-----------------------------------------------------------------------%

:- pred ast_to_pre(env::in, list(ast_block_thing)::in,
    pre_statements::out, set(var)::out, varmap::in, varmap::out) is det.

ast_to_pre(Env, Block0, Block, UseVars, !Varmap) :-
    ast_to_pre_block(Block0, Block, UseVars, _, Env, _, !Varmap).

%-----------------------------------------------------------------------%

:- pred ast_to_pre_block(list(ast_block_thing)::in,
    list(pre_statement)::out, set(var)::out,
    set(var)::out, env::in, env::out, varmap::in, varmap::out) is det.

ast_to_pre_block(Block0, Block, union_list(UseVars), union_list(DefVars), !Env,
        !Varmap) :-
    ast_to_pre_block_2(Block0, StmtsList, UseVars, DefVars, !Env, !Varmap),
    Block = condense(StmtsList).

% It seems silly to use both Env and !Varmap.  They are used differently by
% branches, with varmap tracking all variables and Env being rewound to the
% state before the branch.  Secondly Env will also capture symbols that
% aren't variables, such as modules and instances.

:- pred ast_to_pre_block_2(list(ast_block_thing)::in,
    list(list(pre_statement))::out, list(set(var))::out, list(set(var))::out,
    env::in, env::out, varmap::in, varmap::out) is det.

ast_to_pre_block_2([], [], [], [], !Env, !Varmap).
ast_to_pre_block_2([BlockThing | Block0], [Stmts0 | Stmts],
        [UseVarsHead | UseVarsTail], [DefVarsHead | DefVarsTail],
        !Env, !Varmap) :-
    ( BlockThing = astbt_statement(Stmt),
        ast_to_pre_stmt(Stmt, Stmts0, UseVarsHead, DefVarsHead,
            !Env, !Varmap),
        Block = Block0
    ; BlockThing = astbt_function(_, _),
        take_while(pred(astbt_function(_, _)::in) is semidet,
            [BlockThing | Block0], Defns, Block),
        ast_to_pre_block_defns(Defns, Stmts0, UseVarsHead, DefVarsHead,
            !Env, !Varmap)

    ),
    ast_to_pre_block_2(Block, Stmts, UseVarsTail, DefVarsTail,
        !Env, !Varmap).

:- pred ast_to_pre_block_defns(list(ast_block_thing)::in,
    list(pre_statement)::out, set(var)::out, set(var)::out,
    env::in, env::out, varmap::in, varmap::out) is det.

ast_to_pre_block_defns(Defns0, Stmts, UseVars, DefVars, !Env, !Varmap) :-
    Defns = map((func(BT) = {N, F} :-
            ( BT = astbt_function(N, F)
            ; BT = astbt_statement(_),
                unexpected($file, $pred, "Statement")
            )
        ), Defns0),

    % 1. Pre-process definitions into a letrec so that mutual recursion is
    % supported.
    map_foldl2(defn_make_letrec, Defns, Vars, !Env, !Varmap),

    % 2. Create the bodies.
    env_enter_closure(!.Env, EnvInClosure),
    map2_foldl2(defn_make_pre_body, Defns, Exprs, UseVarsList,
        EnvInClosure, _, !Varmap),
    env_leave_letrec(!Env),

    % 3. Create the expressions and statements.
    map4_corresponding2(defn_make_stmt, Defns, Vars, Exprs, UseVarsList,
        StmtsList, DefVarsList),
    Stmts = condense(StmtsList),
    UseVars = union_list(UseVarsList),
    DefVars = union_list(DefVarsList).

:- pred defn_make_letrec({nq_name, ast_function}::in, var::out,
    env::in, env::out, varmap::in, varmap::out) is det.

defn_make_letrec({Name, ast_function(Decl, _, _)}, Var, !Env, !Varmap) :-
    Context = Decl ^ afd_context,
    NameStr = nq_name_to_string(Name),
    ( if env_add_for_letrec(NameStr, VarPrime, !Env, !Varmap) then
        Var = VarPrime
    else
        compile_error($file, $pred, Context,
            format("Name already defined for nested function: %s",
                [s(NameStr)]))
    ).

:- pred defn_make_pre_body({nq_name, ast_function}::in, pre_expr::out,
    set(var)::out, env::in, env::out, varmap::in, varmap::out) is det.

defn_make_pre_body({Name, ast_function(Decl, Body0, _)}, Expr, UseVars, !Env,
        !Varmap) :-
    Decl = ast_function_decl(Params0, Returns, _, Context),
    NameStr = nq_name_to_string(Name),
    ClobberedName = clobber_lambda(NameStr, Context),
    env_lookup_lambda(!.Env, ClobberedName, FuncId),
    env_letrec_self_recursive(NameStr, FuncId, !.Env, EnvSelfRec),
    ast_to_pre_body(EnvSelfRec, Context, Params0, Params, Body0, Body,
        UseVars, !Varmap),
    % Until we properly implement letrecs we mark each variable as defined
    % immediately after its definition.  We'll need this to properly support
    % optimisation of mutually-recursive closures.
    env_letrec_defined(NameStr, !Env),
    Arity = arity(length(Returns)),
    Expr = e_lambda(pre_lambda(FuncId, Params, no, Arity, Body)).

:- pred defn_make_stmt({nq_name, ast_function}::in, var::in, pre_expr::in,
    set(var)::in, pre_statements::out, set(var)::out) is det.

defn_make_stmt({_, ast_function(Decl, _, _)},
        Var, Expr, UseVars, Stmts, DefVars) :-
    Context = Decl ^ afd_context,
    DefVars = make_singleton_set(Var),
    Stmts = [
        pre_statement(s_decl_vars([Var]),
            stmt_info(Context, set.init, set.init, stmt_always_fallsthrough)),
        pre_statement(s_assign([var(Var)], [Expr]),
            stmt_info(Context, UseVars, DefVars, stmt_always_fallsthrough))
    ].

%-----------------------------------------------------------------------%

:- pred ast_to_pre_stmt(ast_statement::in,
    pre_statements::out, set(var)::out, set(var)::out,
    env::in, env::out, varmap::in, varmap::out) is det.

ast_to_pre_stmt(ast_statement(StmtType0, Context), Stmts, UseVars, DefVars,
        !Env, !Varmap) :-
    ( StmtType0 = s_call(Call),
        ast_to_pre_stmt_call(!.Env, Context, Call, Stmts, UseVars, DefVars,
            !Varmap)
    ; StmtType0 = s_assign_statement(Patterns, MaybeExprs),
        ast_to_pre_stmt_assign(Context, Patterns, MaybeExprs, Stmts,
            UseVars, DefVars, !Env, !Varmap)
    ; StmtType0 = s_array_set_statement(_, _, _),
        util.exception.sorry($file, $pred, "Arrays")
    ; StmtType0 = s_return_statement(Exprs),
        ast_to_pre_stmt_return(!.Env, Context, Exprs, Stmts, UseVars, DefVars,
            !Varmap)
    ; StmtType0 = s_vars_statement(VarNames, MaybeExpr),
        ast_to_pre_stmt_vars(Context, VarNames, MaybeExpr, Stmts, UseVars,
            DefVars, !Env, !Varmap)
    ; StmtType0 = s_match_statement(Expr, Cases),
        ast_to_pre_stmt_match(Context, Expr, Cases, Stmts, UseVars,
            DefVars, !Env, !Varmap)
    ; StmtType0 = s_unpack(_, _),
        unexpected($file, $pred, "Moving this statement")
    ; StmtType0 = s_ite(Cond, Then, Else),
        ast_to_pre_stmt_ite(Context, Cond, Then, Else, Stmts, UseVars,
            DefVars, !Env, !Varmap)
    ).

:- pred ast_to_pre_stmt_call(env::in, context::in, ast_call_like::in,
    pre_statements::out, set(var)::out, set(var)::out,
    varmap::in, varmap::out) is det.

ast_to_pre_stmt_call(Env, Context, Call0, Stmts, UseVars, DefVars, !Varmap) :-
    ast_to_pre_call_like(Env, Call0, CallLike, UseVars, !Varmap),
    ( CallLike = pcl_call(Call)
    ; CallLike = pcl_constr(_),
        compile_error($file, $pred,
            "A construction is not a statement")
    ),
    DefVars = set.init,
    StmtType = s_call(Call),
    Stmts = [pre_statement(StmtType,
        stmt_info(Context, UseVars, DefVars, stmt_always_fallsthrough))].

:- pred ast_to_pre_stmt_assign(context::in, list(ast_pattern)::in,
    maybe(list(ast_expression))::in, pre_statements::out,
    set(var)::out, set(var)::out,
    env::in, env::out, varmap::in, varmap::out) is det.

ast_to_pre_stmt_assign(Context, Patterns0, MaybeExprs, Stmts, UseVars, DefVars,
        !Env, !Varmap) :-
    ( MaybeExprs = no,
        % Without an assignment section this only declares but doesn't
        % initalise variables.  This only makes sense, and is only legal if
        % each pattern is just a fresh variable.
        ( if
            list.map(pred(p_var(Name)::in, Name::out) is semidet,
                Patterns0, _VarNames)
        then
            util.exception.sorry($file, $pred, "Complex pattern")
        else
            compile_error($file, $pred,
                "Var declaration has complex pattern")
        )
    ; MaybeExprs = yes(Exprs0),
        % Process the expressions before adding the variables, this may
        % create confusing errors (without column numbers) but at least
        % it'll be correct.
        map2_foldl(ast_to_pre_expr(!.Env), Exprs0, Exprs, ExprsUseVarss,
            !Varmap),
        ExprsUseVars = union_list(ExprsUseVarss),

        map_foldl2(pattern_initialse_vars, Patterns0, Patterns, !Env, !Varmap),
        ( if
            map(pred(Pattern::in, VOW::out) is semidet :-
                ( Pattern = p_var(Name),
                    % We know that the variables here are initialised so we
                    % can use search_var_det to find them.
                    % XXX: This is inefficient, we had the var for these
                    % during initialsation.
                    search_var_det(!.Varmap, Name, Var),
                    VOW = var(Var)
                ; Pattern = p_symbol(Name),
                    % As above.
                    search_var_det(!.Varmap, Name, Var),
                    VOW = var(Var)
                ; Pattern = p_wildcard,
                    VOW = wildcard
                ),
                Patterns, VarOrWildcards)
        then
            filter_map(vow_is_var, VarOrWildcards, Vars),
            DefVars = list_to_set(Vars),
            UseVars = ExprsUseVars,
            StmtType = s_assign(VarOrWildcards, Exprs),
            Stmts = [pre_statement(StmtType,
                stmt_info(Context, UseVars, DefVars,
                    stmt_always_fallsthrough))]
        else if
            Patterns0 = [Pattern0],
            Exprs = [Expr]
        then
            ast_to_pre_stmt_unpack(Context, Pattern0, Expr, Stmts,
                UsedVars0, DefVars, !Env, !Varmap),
            UseVars = ExprsUseVars `union` UsedVars0
        else
            util.exception.sorry($file, $pred,
                "Can't unpack more than one pattern")
        )
    ).

:- pred ast_to_pre_stmt_return(env::in, context::in, list(ast_expression)::in,
    pre_statements::out, set(var)::out, set(var)::out,
    varmap::in, varmap::out) is det.

ast_to_pre_stmt_return(Env, Context, Exprs0, Stmts, UseVars, DefVars,
        !Varmap) :-
    map2_foldl(ast_to_pre_expr(Env), Exprs0, Exprs, ExprsUseVars, !Varmap),
    UseVars = union_list(ExprsUseVars),
    varmap.add_n_anon_vars(length(Exprs), Vars, !Varmap),
    RetVars = list_to_set(Vars),
    DefVars = RetVars,
    Stmts = [
        pre_statement(s_decl_vars(Vars),
            stmt_info(Context, set.init, set.init, stmt_always_fallsthrough)),
        pre_statement(s_assign(map(func(V) = var(V), Vars), Exprs),
            stmt_info(Context, UseVars, DefVars, stmt_always_fallsthrough)),
        pre_statement(s_return(Vars),
            stmt_info(Context, RetVars, set.init, stmt_always_returns))
    ].

:- pred ast_to_pre_stmt_vars(context::in, list(var_or_wildcard(string))::in,
    maybe(ast_expression)::in, pre_statements::out, set(var)::out,
    set(var)::out, env::in, env::out, varmap::in, varmap::out) is det.

ast_to_pre_stmt_vars(Context, VarNames, MaybeExpr, Stmts, UseVars, DefVars,
        !Env, !Varmap) :-
    ( MaybeExpr = no,
        AddToEnv = do_var_or_wildcard(env_add_uninitialised_var)
    ; MaybeExpr = yes(_),
        AddToEnv = do_var_or_wildcard(env_add_and_initlalise_var)
    ),
    ( if
        map_foldl2(AddToEnv, VarNames, VarOrWildcards, !Env, !Varmap)
    then
        filter_map(vow_is_var, VarOrWildcards, Vars),
        DeclStmt = pre_statement(s_decl_vars(Vars),
            stmt_info(Context, set.init, set.init,
                stmt_always_fallsthrough)),
        ( MaybeExpr = no,
            UseVars = init,
            DefVars = init,
            AssignStmts = []
        ; MaybeExpr = yes(Expr0),
            ast_to_pre_expr(!.Env, Expr0, Expr, UseVars, !Varmap),
            DefVars = list_to_set(Vars),
            StmtType = s_assign(VarOrWildcards, [Expr]),
            AssignStmts = [pre_statement(StmtType,
                stmt_info(Context, UseVars, DefVars,
                    stmt_always_fallsthrough))]
        ),
        Stmts = [DeclStmt | AssignStmts]
    else
        compile_error($file, $pred, Context,
            format("One or more variables %s already defined",
                [s(string(VarNames))]))
    ).

:- pred ast_to_pre_stmt_match(context::in, ast_expression::in,
    list(ast_match_case)::in, pre_statements::out, set(var)::out, set(var)::out,
    env::in, env::out, varmap::in, varmap::out) is det.

ast_to_pre_stmt_match(Context, Expr0, Cases0, Stmts, UseVars, DefVars,
        !Env, !Varmap) :-
    ast_to_pre_expr(!.Env, Expr0, Expr, UseVarsExpr, !Varmap),
    varmap.add_anon_var(Var, !Varmap),
    StmtsAssign = [
        pre_statement(s_decl_vars([Var]),
            stmt_info(Context, set.init, set.init,
                stmt_always_fallsthrough)),
        pre_statement(s_assign([var(Var)], [Expr]),
            stmt_info(Context, UseVarsExpr, make_singleton_set(Var),
                stmt_always_fallsthrough))
    ],

    map3_foldl(ast_to_pre_case(!.Env), Cases0, Cases,
        UseVarsCases, DefVars0, !Varmap),

    UseVars = union_list(UseVarsCases) `union` make_singleton_set(Var),
    DefVars = union_list(DefVars0) `intersect`
        env_uninitialised_vars(!.Env),
    env_mark_initialised(DefVars, !Env),
    % The reachability information will be updated later in
    % pre.branches
    StmtMatch = pre_statement(s_match(Var, Cases),
        stmt_info(Context, UseVars, DefVars, stmt_may_return)),

    Stmts = StmtsAssign ++ [StmtMatch].

:- pred ast_to_pre_stmt_unpack(context::in, ast_pattern::in,
    pre_expr::in, pre_statements::out, set(var)::out,
    set(var)::out, env::in, env::out, varmap::in, varmap::out) is det.

ast_to_pre_stmt_unpack(Context, Pattern0, Expr, Stmts, UsedVars, DefVars,
        !Env, !Varmap) :-
    % Transform the pattern then rename all the variables in the pattern to
    % new fresh variables.
    ast_to_pre_pattern(Pattern0, Pattern1, PatVarsSet, !Env, !Varmap),
    pat_rename(PatVarsSet, Pattern1, Pattern, map.init, Renaming, !Varmap),

    PatternVarPairs = to_assoc_list(Renaming),

    % The list of variables form the original pattern.
    PatternVars = map(fst, PatternVarPairs),
    % The list of new variables, they have the same positions in their list
    % as the original set.
    PrimeVars = map(snd, PatternVarPairs),
    PrimeVarsSet = list_to_set(PrimeVars),

    % The new pattern with the renamed variables is used with an expression
    % to copy those variables out.  TODO: For now we can only handle
    % patterns that extract a single variable.
    ( PrimeVars = [],
        unexpected($file, $pred, "Zero variables bound by unpack")
    ; PrimeVars = [_ | _],
        CopyVarsOutExprs = map(func(V) = e_var(V), PrimeVars)
    ),
    MatchExpr = e_match(Expr, [pre_e_case(Pattern, CopyVarsOutExprs)]),

    % The assignment must assign variables in the same order that
    % CopyVarsOutExpr returns them as.
    DefVars = list_to_set(PatternVars),
    PatternVarsVars = map(func(V) = var(V), PatternVars),
    AssignStmt = pre_statement(s_assign(PatternVarsVars, [MatchExpr]),
            stmt_info(Context, UsedVars, DefVars, stmt_always_fallsthrough)),

    Stmts = [
        pre_statement(s_decl_vars(PatternVars),
            stmt_info(Context, set.init, set.init, stmt_always_fallsthrough)),
        AssignStmt],
    UsedVars = PatVarsSet `union` PrimeVarsSet.

:- pred ast_to_pre_stmt_ite(context::in, ast_expression::in,
    list(ast_block_thing)::in, list(ast_block_thing)::in,
    pre_statements::out, set(var)::out, set(var)::out,
    env::in, env::out, varmap::in, varmap::out) is det.

ast_to_pre_stmt_ite(Context, Cond0, Then0, Else0, Stmts, UseVars, DefVars,
        !Env, !Varmap) :-
    % ITEs are syntas sugar for a match expression using booleans.

    ast_to_pre_expr(!.Env, Cond0, Cond, UseVarsCond, !Varmap),
    varmap.add_anon_var(Var, !Varmap),
    % TODO: To avoid amberguities, we may need a way to force this
    % variable to be bool at this point in the compiler when we know that
    % it's a bool.
    StmtsAssign = [
        pre_statement(s_decl_vars([Var]),
            stmt_info(Context, set.init, set.init,
                stmt_always_fallsthrough)),
        pre_statement(s_assign([var(Var)], [Cond]),
            stmt_info(Context, UseVarsCond, make_singleton_set(Var),
                stmt_always_fallsthrough))
    ],

    ast_to_pre_block(Then0, Then, UseVarsThen, DefVarsThen, !.Env, _,
        !Varmap),
    TrueId = env_get_bool_true(!.Env),
    TrueCase = pre_case(p_constr(TrueId, []), Then),
    ast_to_pre_block(Else0, Else, UseVarsElse, DefVarsElse, !.Env, _,
        !Varmap),
    FalseId = env_get_bool_false(!.Env),
    FalseCase = pre_case(p_constr(FalseId, []), Else),

    UseVars = union(UseVarsThen, UseVarsElse) `union`
        make_singleton_set(Var),
    DefVars = union(DefVarsThen, DefVarsElse) `intersect`
        env_uninitialised_vars(!.Env),
    env_mark_initialised(DefVars, !Env),
    StmtMatch = pre_statement(s_match(Var, [TrueCase, FalseCase]),
        stmt_info(Context, UseVars, DefVars, stmt_may_return)),
    Stmts = StmtsAssign ++ [StmtMatch].

%-----------------------------------------------------------------------%

:- pred ast_to_pre_case(env::in, ast_match_case::in, pre_case::out,
    set(var)::out, set(var)::out, varmap::in, varmap::out) is det.

ast_to_pre_case(!.Env, ast_match_case(Pattern0, Stmts0),
        pre_case(Pattern, Stmts), UseVars, DefVars, !Varmap) :-
    ast_to_pre_pattern(Pattern0, Pattern, DefVarsPattern, !Env, !Varmap),
    ast_to_pre_block(Stmts0, Stmts, UseVars, DefVarsStmts, !Env, !Varmap),
    DefVars = DefVarsPattern `union` DefVarsStmts,
    _ = !.Env.

:- pred ast_to_pre_pattern(ast_pattern::in, pre_pattern::out, set(var)::out,
    env::in, env::out, varmap::in, varmap::out) is det.

ast_to_pre_pattern(p_number(Num), p_number(Num), set.init, !Env, !Varmap).
ast_to_pre_pattern(p_constr(Name, Args0), Pattern, Vars, !Env, !Varmap) :-
    ( if env_search_constructor(!.Env, q_name_single(Name), CtorId) then
        map2_foldl2(ast_to_pre_pattern, Args0, Args, ArgsVars, !Env, !Varmap),
        Vars = union_list(ArgsVars),
        Pattern = p_constr(CtorId, Args)
    else
        ( Args0 = [],
            Kind = "variable or constructor"
        ; Args0 = [_ | _],
            Kind = "constructor"
        ),
        compile_error($file, $pred,
            format("Unknown %s '%s'", [s(Kind), s(Name)]))
    ).
ast_to_pre_pattern(p_list_nil, Pattern, set.init, !Env, !Varmap) :-
    Pattern = p_constr(env_get_list_nil(!.Env), []).
ast_to_pre_pattern(p_list_cons(Head0, Tail0), Pattern, Vars,
        !Env, !Varmap) :-
    ast_to_pre_pattern(Head0, Head, HeadVars, !Env, !Varmap),
    ast_to_pre_pattern(Tail0, Tail, TailVars, !Env, !Varmap),
    Vars = HeadVars `union` TailVars,
    Pattern = p_constr(env_get_list_cons(!.Env), [Head, Tail]).
ast_to_pre_pattern(p_wildcard, p_wildcard, set.init, !Env, !Varmap).
ast_to_pre_pattern(p_var(Name), Pattern, DefVars, !Env, !Varmap) :-
    ( if env_add_and_initlalise_var(Name, Var, !Env, !Varmap) then
        Pattern = p_var(Var),
        DefVars = make_singleton_set(Var)
    else
        compile_error($file, $pred,
            format("Variable '%s' already defined", [s(Name)]))
    ).
ast_to_pre_pattern(p_symbol(Name), Pattern, DefVars, !Env, !Varmap) :-
    env_initialise_var(Name, Result, !Env, !Varmap),
    ( Result = ok(Var),
        Pattern = p_var(Var),
        DefVars = make_singleton_set(Var)
    ; Result = does_not_exist,
        ast_to_pre_pattern(p_constr(Name, []), Pattern, DefVars, !Env, !Varmap)
    ; Result = already_initialised,
        compile_error($file, $pred, "Variable already initialised")
    ; Result = inaccessible,
        unexpected($file, $pred, "Inaccessible?")
    ).

:- pred ast_to_pre_expr(env::in, ast_expression::in,
    pre_expr::out, set(var)::out, varmap::in, varmap::out) is det.

ast_to_pre_expr(Env, Expr0, Expr, Vars, !Varmap) :-
    ast_to_pre_expr_2(Env, Expr0, Expr1, Vars, !Varmap),
    ( if Expr1 = e_constant(c_ctor(ConsId)) then
        Expr = e_construction(ConsId, [])
    else
        Expr = Expr1
    ).

:- pred ast_to_pre_expr_2(env::in, ast_expression::in, pre_expr::out,
    set(var)::out, varmap::in, varmap::out) is det.

ast_to_pre_expr_2(Env, e_call_like(Call0), Expr, Vars, !Varmap) :-
    ast_to_pre_call_like(Env, Call0, CallLike, Vars, !Varmap),
    ( CallLike = pcl_call(Call),
        Expr = e_call(Call)
    ; CallLike = pcl_constr(Expr)
    ).
ast_to_pre_expr_2(Env, e_u_op(Op, SubExpr0), Expr, Vars, !Varmap) :-
    ast_to_pre_expr(Env, SubExpr0, SubExpr, Vars, !Varmap),
    ( if env_unary_operator_func(Env, Op, OpFunc) then
        Expr = e_call(pre_call(OpFunc, [SubExpr], without_bang))
    else
        unexpected($file, $pred, "Operator implementation not found")
    ).
ast_to_pre_expr_2(Env, e_b_op(ExprL0, Op, ExprR0), Expr, Vars, !Varmap) :-
    ast_to_pre_expr(Env, ExprL0, ExprL, VarsL, !Varmap),
    ast_to_pre_expr(Env, ExprR0, ExprR, VarsR, !Varmap),
    Vars = union(VarsL, VarsR),
    % NOTE: When introducing interfaces for primative types this will need
    % to change
    ( if env_operator_entry(Env, Op, OpEntry) then
        ( OpEntry = ee_func(OpFunc),
            Expr = e_call(pre_call(OpFunc, [ExprL, ExprR], without_bang))
        ; OpEntry = ee_constructor(OpCtor),
            Expr = e_construction(OpCtor, [ExprL, ExprR])
        )
    else
        unexpected($file, $pred,
            format("Operator implementation not found: %s", [s(string(Op))]))
    ).
ast_to_pre_expr_2(Env, e_match(MatchExpr0, Cases0), Expr, Vars, !Varmap) :-
    ast_to_pre_expr(Env, MatchExpr0, MatchExpr, MatchVars, !Varmap),
    map2_foldl(ast_to_pre_expr_case(Env), Cases0, Cases, CasesVars, !Varmap),
    Expr = e_match(MatchExpr, Cases),
    Vars = MatchVars `union` union_list(CasesVars).
ast_to_pre_expr_2(Env, e_if(Cond0, Then0, Else0), Expr, Vars, !Varmap) :-
    ast_to_pre_expr(Env, Cond0, Cond, CondVars, !Varmap),
    map2_foldl(ast_to_pre_expr(Env), Then0, Then, ThenVars, !Varmap),
    map2_foldl(ast_to_pre_expr(Env), Else0, Else, ElseVars, !Varmap),
    PatTrue = p_constr(env_get_bool_true(Env), []),
    PatFalse = p_constr(env_get_bool_false(Env), []),
    Expr = e_match(Cond,
        [pre_e_case(PatTrue, Then),
         pre_e_case(PatFalse, Else)]),
    Vars = CondVars `union` union_list(ThenVars) `union` union_list(ElseVars).
ast_to_pre_expr_2(Env, e_symbol(Symbol), Expr, Vars, !Varmap) :-
    env_search(Env, Symbol, Result),
    ( Result = ok(Entry),
        ( Entry = ee_var(Var),
            Expr = e_var(Var),
            Vars = make_singleton_set(Var)
        ; Entry = ee_constructor(Constr),
            Expr = e_constant(c_ctor(Constr)),
            Vars = set.init
        ; Entry = ee_func(Func),
            Expr = e_constant(c_func(Func)),
            Vars = set.init
        )
    ; Result = not_found,
        compile_error($file, $pred,
            format("Unknown symbol: %s", [s(q_name_to_string(Symbol))]))
    ;
        ( Result = not_initaliased
        % Varibles may be inaccessible because they're not initalised.
        ; Result = inaccessible
        ),
        compile_error($file, $pred,
            format("Variable not initalised: %s",
                [s(q_name_to_string(Symbol))]))
    ; Result = maybe_cyclic_retlec,
        util.exception.sorry($file, $pred,
            format("%s is possibly involved in a mutual recursion of " ++
                "closures. If they're not mutually recursive try " ++
                "re-ordering them.",
                [s(q_name_to_string(Symbol))]))
    ).
ast_to_pre_expr_2(Env, e_const(Const0), e_constant((Const)), init, !Varmap) :-
    ( Const0 = c_string(String),
        Const = c_string(String)
    ; Const0 = c_number(Number),
        Const = c_number(Number)
    ; Const0 = c_list_nil,
        Const = c_ctor(env_get_list_nil(Env))
    ).
ast_to_pre_expr_2(_, e_array(_), _, _, !Varmap) :-
    util.exception.sorry($file, $pred, "Arrays").

:- type pre_call_like
    --->    pcl_call(pre_call)
    ;       pcl_constr(pre_expr).

:- pred ast_to_pre_call_like(env::in,
    ast_call_like::in, pre_call_like::out, set(var)::out,
    varmap::in, varmap::out) is det.

ast_to_pre_call_like(Env, CallLike0, CallLike, Vars, !Varmap) :-
    ( CallLike0 = ast_call_like(CalleeExpr0, Args0),
        WithBang = without_bang
    ; CallLike0 = ast_bang_call(CalleeExpr0, Args0),
        WithBang = with_bang
    ),
    % For the callee we call the _2 version, which does not convert
    % constructors with no args into constructions.
    ast_to_pre_expr_2(Env, CalleeExpr0, CalleeExpr, CalleeVars, !Varmap),
    map2_foldl(ast_to_pre_expr(Env), Args0, Args, Varss, !Varmap),
    Vars = union_list(Varss) `union` CalleeVars,
    ( if CalleeExpr = e_constant(c_func(Callee)) then
        CallLike = pcl_call(pre_call(Callee, Args, WithBang))
    else if CalleeExpr = e_constant(c_ctor(CtorId)) then
        ( WithBang = with_bang,
            compile_error($file, $pred,
                "Construction must not have bang")
        ; WithBang = without_bang,
            CallLike = pcl_constr(e_construction(CtorId, Args))
        )
    else
        CallLike = pcl_call(pre_ho_call(CalleeExpr, Args, WithBang))
    ).

:- pred ast_to_pre_expr_case(env::in, ast_expr_match_case::in,
    pre_expr_case::out, set(var)::out, varmap::in, varmap::out) is det.

ast_to_pre_expr_case(Env0, ast_emc(Pat0, Exprs0), pre_e_case(Pat, Exprs),
        Vars, !Varmap) :-
    % Pretty sure we don't need to capture the new variable here as we do in
    % the match statements.
    ast_to_pre_pattern(Pat0, Pat, _, Env0, Env, !Varmap),
    map2_foldl(ast_to_pre_expr(Env), Exprs0, Exprs, Varss, !Varmap),
    Vars = union_list(Varss).

%-----------------------------------------------------------------------%

    % Find uninitialse variables apearing in a pattern and initialise them.
    %
:- pred pattern_initialse_vars(ast_pattern::in, ast_pattern::out,
    env::in, env::out, varmap::in, varmap::out) is det.

pattern_initialse_vars(p_constr(Sym, Args0), p_constr(Sym, Args), !Env,
        !Varmap) :-
    map_foldl2(pattern_initialse_vars, Args0, Args, !Env, !Varmap).
pattern_initialse_vars(P@p_number(_), P, !Env, !Varmap).
pattern_initialse_vars(p_wildcard, p_wildcard, !Env, !Varmap).
pattern_initialse_vars(P@p_var(_), P, !Env, !Varmap).
pattern_initialse_vars(p_symbol(Sym), P, !Env, !Varmap) :-
    env_initialise_var(Sym, Result, !Env, !Varmap),
    ( Result = ok(_),
        P = p_symbol(Sym)
    ; Result = does_not_exist,
        % Must be a constructor.
        P = p_constr(Sym, [])
    ; Result = already_initialised,
        compile_error($file, $pred,
            format("The variable '%s' is already initialised", [s(Sym)]))
    ; Result = inaccessible,
        compile_error($file, $pred,
            format("The variable '%s' is defined in an outer scope and " ++
                "cannot be initialised from within this closure",
                [s(Sym)]))
    ).
pattern_initialse_vars(p_list_nil, p_list_nil, !Env, !Varmap).
pattern_initialse_vars(p_list_cons(PatA0, PatB0), p_list_cons(PatA, PatB),
        !Env, !Varmap) :-
    pattern_initialse_vars(PatA0, PatA, !Env, !Varmap),
    pattern_initialse_vars(PatB0, PatB, !Env, !Varmap).

%-----------------------------------------------------------------------%
