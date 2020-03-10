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

%-----------------------------------------------------------------------%

:- pred func_to_pre_func(env::in, string::in, list(ast_param)::in,
    list(ast_type_expr)::in, list(ast_block_thing)::in, context::in,
    map(func_id, pre_procedure)::in, map(func_id, pre_procedure)::out) is det.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
:- implementation.

:- import_module maybe.
:- import_module require.
:- import_module set.
:- import_module string.

:- import_module q_name.
:- import_module util.
:- import_module varmap.

%-----------------------------------------------------------------------%

func_to_pre_func(Env, Name, Params, Returns, Body0, Context, !Pre) :-
    % Build body.
    some [!Varmap] (
        !:Varmap = varmap.init,
        env_lookup_function(Env, q_name(Name), FuncId),
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
    ; BlockThing = astbt_definition(_),
        list_take_while(pred(astbt_definition(_)::in) is semidet,
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
    Defns = map((func(BT) = D :-
            ( BT = astbt_definition(D)
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

:- pred defn_make_letrec(ast_definition::in, var::out, env::in, env::out,
    varmap::in, varmap::out) is det.

defn_make_letrec(ast_function(Name, _, _, _, _, Context), Var, !Env, !Varmap) :-
    ( if env_add_for_letrec(Name, VarPrime, !Env, !Varmap) then
        Var = VarPrime
    else
        util.compile_error($file, $pred, Context,
            format("Name already defined for nested function: %s",
                [s(Name)]))
    ).

:- pred defn_make_pre_body(ast_definition::in, pre_expr::out,
    set(var)::out, env::in, env::out, varmap::in, varmap::out) is det.

defn_make_pre_body(ast_function(Name, Params0, Returns, _Uses, Body0, Context),
        Expr, UseVars, !Env, !Varmap) :-
    ClobberedName = clobber_lambda(Name, Context),
    env_lookup_lambda(!.Env, ClobberedName, FuncId),
    env_letrec_self_recursive(Name, FuncId, !.Env, EnvSelfRec),
    ast_to_pre_body(EnvSelfRec, Context, Params0, Params, Body0, Body,
        UseVars, !Varmap),
    % Until we properly implement letrecs we mark each variable as defined
    % immediately after its definition.  We'll need this to properly support
    % optimisation of mutually-recursive closures.
    env_letrec_defined(Name, !Env),
    Arity = arity(length(Returns)),
    Expr = e_lambda(pre_lambda(FuncId, Params, no, Arity, Body)).

:- pred defn_make_stmt(ast_definition::in, var::in, pre_expr::in, set(var)::in,
    pre_statements::out, set(var)::out) is det.

defn_make_stmt(ast_function(_, _, _, _, _, Context),
        Var, Expr, UseVars, Stmts, DefVars) :-
    DefVars = make_singleton_set(Var),
    Stmts = [
        pre_statement(s_decl_vars([Var]),
            stmt_info(Context, set.init, set.init, stmt_always_fallsthrough)),
        pre_statement(s_assign([var(Var)], Expr),
            stmt_info(Context, UseVars, DefVars, stmt_always_fallsthrough))
    ].

%-----------------------------------------------------------------------%

:- pred ast_to_pre_stmt(ast_statement::in,
    pre_statements::out, set(var)::out, set(var)::out,
    env::in, env::out, varmap::in, varmap::out) is det.

ast_to_pre_stmt(ast_statement(StmtType0, Context), Stmts, UseVars, DefVars,
        !Env, !Varmap) :-
    (
        StmtType0 = s_call(Call0),
        ast_to_pre_call_like(!.Env, Call0, CallLike, UseVars),
        ( CallLike = pcl_call(Call)
        ; CallLike = pcl_constr(_),
            util.compile_error($file, $pred,
                "A construction is not a statement")
        ),
        DefVars = set.init,
        StmtType = s_call(Call),
        Stmts = [pre_statement(StmtType,
            stmt_info(Context, UseVars, DefVars, stmt_always_fallsthrough))]
    ;
        StmtType0 = s_assign_statement(VarNames, Expr0),
        % Process the expression before adding the variable, this may create
        % confusing errors (without column numbers) but at least it'll be
        % correct.
        ast_to_pre_expr(!.Env, Expr0, Expr, UseVars),
        map_foldl2(ast_to_pre_init_var(Context), VarNames, VarOrWildcards,
            !Env, !Varmap),
        filter_map(vow_is_var, VarOrWildcards, Vars),
        DefVars = list_to_set(Vars),
        StmtType = s_assign(VarOrWildcards, Expr),
        Stmts = [pre_statement(StmtType,
            stmt_info(Context, UseVars, DefVars, stmt_always_fallsthrough))]
    ;
        StmtType0 = s_array_set_statement(_, _, _),
        util.sorry($file, $pred, "Arrays")
    ;
        StmtType0 = s_return_statement(Exprs0),
        map2_foldl(ast_to_pre_return(Context, !.Env), Exprs0, Vars,
            StmtssAssign, !Varmap),
        StmtsAssign = condense(StmtssAssign),
        UseVars = union_list(map((func(S) = S ^ s_info ^ si_use_vars),
            StmtsAssign)),
        RetVars = list_to_set(Vars),
        DefVars = RetVars,

        StmtReturn = pre_statement(s_return(Vars),
            stmt_info(Context, RetVars, set.init, stmt_always_returns)),
        Stmts = StmtsAssign ++ [StmtReturn]
    ;
        StmtType0 = s_vars_statement(VarNames, MaybeExpr),
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
                ast_to_pre_expr(!.Env, Expr0, Expr, UseVars),
                DefVars = list_to_set(Vars),
                StmtType = s_assign(VarOrWildcards, Expr),
                AssignStmts = [pre_statement(StmtType,
                    stmt_info(Context, UseVars, DefVars,
                        stmt_always_fallsthrough))]
            ),
            Stmts = [DeclStmt | AssignStmts]
        else
            compile_error($file, $pred, Context,
                format("One or more variables %s already defined",
                    [s(string(VarNames))]))
        )
    ;
        StmtType0 = s_match_statement(Expr0, Cases0),
        ast_to_pre_expr(!.Env, Expr0, Expr, UseVarsExpr),
        varmap.add_anon_var(Var, !Varmap),
        StmtsAssign = [
            pre_statement(s_decl_vars([Var]),
                stmt_info(Context, set.init, set.init,
                    stmt_always_fallsthrough)),
            pre_statement(s_assign([var(Var)], Expr),
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

        Stmts = StmtsAssign ++ [StmtMatch]
    ;
        StmtType0 = s_ite(Cond0, Then0, Else0),
        % ITEs are syntas sugar for a match expression using booleans.

        ast_to_pre_expr(!.Env, Cond0, Cond, UseVarsCond),
        varmap.add_anon_var(Var, !Varmap),
        % TODO: To avoid amberguities, we may need a way to force this
        % variable to be bool at this point in the compiler when we know that
        % it's a bool.
        StmtsAssign = [
            pre_statement(s_decl_vars([Var]),
                stmt_info(Context, set.init, set.init,
                    stmt_always_fallsthrough)),
            pre_statement(s_assign([var(Var)], Cond),
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
        Stmts = StmtsAssign ++ [StmtMatch]
    ).

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
    ( if env_search_constructor(!.Env, q_name(Name), CtorId) then
        map2_foldl2(ast_to_pre_pattern, Args0, Args, ArgsVars, !Env, !Varmap),
        Vars = union_list(ArgsVars),
        Pattern = p_constr(CtorId, Args)
    else
        util.compile_error($file, $pred, "Unknown constructor")
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

:- pred ast_to_pre_return(context::in, env::in, ast_expression::in,
    var::out, pre_statements::out, varmap::in, varmap::out) is det.

ast_to_pre_return(Context, Env, Expr0, Var, Stmts, !Varmap) :-
    ast_to_pre_expr(Env, Expr0, Expr, UseVars),
    varmap.add_anon_var(Var, !Varmap),
    DefVars = make_singleton_set(Var),
    Stmts = [
        pre_statement(s_decl_vars([Var]),
            stmt_info(Context, set.init, set.init, stmt_always_fallsthrough)),
        pre_statement(s_assign([var(Var)], Expr),
            stmt_info(Context, UseVars, DefVars, stmt_always_fallsthrough))
    ].

:- pred ast_to_pre_expr(env::in, ast_expression::in,
    pre_expr::out, set(var)::out) is det.

ast_to_pre_expr(Env, Expr0, Expr, Vars) :-
    ast_to_pre_expr_2(Env, Expr0, Expr1, Vars),
    ( if Expr1 = e_constant(c_ctor(ConsId)) then
        Expr = e_construction(ConsId, [])
    else
        Expr = Expr1
    ).

:- pred ast_to_pre_expr_2(env::in, ast_expression::in, pre_expr::out,
    set(var)::out) is det.

ast_to_pre_expr_2(Env, e_call_like(Call0), Expr, Vars) :-
    ast_to_pre_call_like(Env, Call0, CallLike, Vars),
    ( CallLike = pcl_call(Call),
        Expr = e_call(Call)
    ; CallLike = pcl_constr(Expr)
    ).
ast_to_pre_expr_2(Env, e_u_op(Op, SubExpr0), Expr, Vars) :-
    ast_to_pre_expr(Env, SubExpr0, SubExpr, Vars),
    ( if env_unary_operator_func(Env, Op, OpFunc) then
        Expr = e_call(pre_call(OpFunc, [SubExpr], without_bang))
    else
        unexpected($file, $pred, "Operator implementation not found")
    ).
ast_to_pre_expr_2(Env, e_b_op(ExprL0, Op, ExprR0), Expr, Vars) :-
    ast_to_pre_expr(Env, ExprL0, ExprL, VarsL),
    ast_to_pre_expr(Env, ExprR0, ExprR, VarsR),
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
ast_to_pre_expr_2(Env, e_symbol(Symbol), Expr, Vars) :-
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
        % Varibles may be inaccessable because they're not initalised.
        ; Result = inaccessible
        ),
        compile_error($file, $pred,
            format("Variable not initalised: %s",
                [s(q_name_to_string(Symbol))]))
    ; Result = maybe_cyclic_retlec,
        util.sorry($file, $pred,
            format("%s is possibly involved in a mutual recursion of " ++
                "closures. If they're not mutually recursive try " ++
                "re-ordering them.",
                [s(q_name_to_string(Symbol))]))
    ).
ast_to_pre_expr_2(Env, e_const(Const0), e_constant((Const)), init) :-
    ( Const0 = c_string(String),
        Const = c_string(String)
    ; Const0 = c_number(Number),
        Const = c_number(Number)
    ; Const0 = c_list_nil,
        Const = c_ctor(env_get_list_nil(Env))
    ).
ast_to_pre_expr_2(_, e_array(_), _, _) :-
    util.sorry($file, $pred, "Arrays").

:- type pre_call_like
    --->    pcl_call(pre_call)
    ;       pcl_constr(pre_expr).

:- pred ast_to_pre_call_like(env::in,
    ast_call_like::in, pre_call_like::out, set(var)::out) is det.

ast_to_pre_call_like(Env, CallLike0, CallLike, Vars) :-
    ( CallLike0 = ast_call_like(CalleeExpr0, Args0),
        WithBang = without_bang
    ; CallLike0 = ast_bang_call(CalleeExpr0, Args0),
        WithBang = with_bang
    ),
    % For the callee we call the _2 version, which does not convert
    % constructors with no args into constructions.
    ast_to_pre_expr_2(Env, CalleeExpr0, CalleeExpr, CalleeVars),
    map2(ast_to_pre_expr(Env), Args0, Args, Varss),
    Vars = union_list(Varss) `union` CalleeVars,
    ( if CalleeExpr = e_constant(c_func(Callee)) then
        CallLike = pcl_call(pre_call(Callee, Args, WithBang))
    else if CalleeExpr = e_constant(c_ctor(CtorId)) then
        ( WithBang = with_bang,
            util.compile_error($file, $pred,
                "Construction must not have bang")
        ; WithBang = without_bang,
            CallLike = pcl_constr(e_construction(CtorId, Args))
        )
    else
        CallLike = pcl_call(pre_ho_call(CalleeExpr, Args, WithBang))
    ).

%-----------------------------------------------------------------------%

    % do_var_or_wildcard(env_initialise_var, ...), but report the error.
    %
:- pred ast_to_pre_init_var(context::in, var_or_wildcard(string)::in,
    var_or_wildcard(var)::out, env::in, env::out, varmap::in, varmap::out)
    is det.

ast_to_pre_init_var(_, wildcard, wildcard, !Env, !Varmap).
ast_to_pre_init_var(Context, var(Name), var(Var), !Env, !Varmap) :-
    env_initialise_var(Name, Result, !Env, !Varmap),
    ( Result = ok(Var)
    ; Result = does_not_exist,
        compile_error($file, $pred, Context,
            format("A variables '%s' has not been declared", [s(Name)]))
    ; Result = already_initialised,
        compile_error($file, $pred, Context,
            format("A variables '%s' is already initialised", [s(Name)]))
    ; Result = inaccessible,
        compile_error($file, $pred, Context,
            format("A variable '%s' is defined in an outer scope " ++
                "and cannot be initialised from within this closure",
                [s(Name)]))
    ).

%-----------------------------------------------------------------------%
