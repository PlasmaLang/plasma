%-----------------------------------------------------------------------%
% Plasma AST symbol resolution
% vim: ts=4 sw=4 et
%
% Copyright (C) 2015-2016 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
% This module resolves symbols within the Plasma AST returning the pre-core
% representation.
%
%-----------------------------------------------------------------------%
:- module pre.from_ast.
%-----------------------------------------------------------------------%

:- interface.

:- import_module ast.
:- import_module pre.env.
:- import_module pre.pre_ds.
:- import_module varmap.

%-----------------------------------------------------------------------%

    % Compared with the AST representation, the pre representation has
    % variables resolved, and restricts where expressions can appear
    % (they're not allowed as the switched-on variable in switches or return
    % expressions).
    %
:- pred ast_to_pre(env::in, list(ast_statement)::in,
    pre_statements::out, varmap::in, varmap::out) is det.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
:- implementation.

:- import_module list.
:- import_module maybe.
:- import_module require.
:- import_module string.

:- import_module common_types.
:- import_module q_name.
:- import_module util.

%-----------------------------------------------------------------------%

ast_to_pre(Env, Statements0, Statements, !Varmap) :-
    ast_to_pre_stmts(Statements0, Statements, _, _, Env, _, !Varmap).

:- pred ast_to_pre_stmts(list(ast_statement)::in,
    pre_statements::out, set(var)::out,
    set(var)::out, env::in, env::out, varmap::in, varmap::out) is det.

ast_to_pre_stmts(Stmts0, Stmts, union_list(UseVars), union_list(DefVars), !Env,
        !Varmap) :-
    map3_foldl2(ast_to_pre_stmt, Stmts0, StmtsList, UseVars, DefVars, !Env,
        !Varmap),
    Stmts = condense(StmtsList).

% It seems silly to use both Env and !Varmap.  However once we add
% branching structures they will be used quite differently and we will need
% both.  Secondly Env will also capture symbols that aren't variables, such
% as modules and instances.

:- pred ast_to_pre_stmt(ast_statement::in,
    pre_statements::out, set(var)::out, set(var)::out,
    env::in, env::out, varmap::in, varmap::out) is det.

ast_to_pre_stmt(Stmt0, Stmts, UseVars, DefVars, !Env, !Varmap) :-
    Stmt0 = ast_statement(StmtType0, Context),
    (
        StmtType0 = s_call(Call0),
        ast_to_pre_call(!.Env, !.Varmap, Call0, Call, UseVars),
        DefVars = set.init,
        StmtType = s_call(Call),
        Stmts = [pre_statement(StmtType,
            stmt_info(Context, UseVars, DefVars, set.init,
                stmt_always_fallsthrough))]
    ;
        % TODO: Raise an error if we rebind a variable (but not a module).
        StmtType0 = s_assign_statement(VarNames, Exprs0),
        ( if
            VarNames = [VarName],
            Exprs0 = [Expr0]
        then
            % Process the expression before adding the variable, this may
            % create confusing errors (without column numbers) but at least
            % it'll be correct.
            ast_to_pre_expr(!.Env, !.Varmap, Expr0, Expr, UseVars),
            ( if env_add_var(VarName, Var, !Env, !Varmap) then
                DefVars = make_singleton_set(Var),
                StmtType = s_assign(Var, Expr)
            else
                compile_error($file, $pred, Context,
                    format("Variable '%s' already defined", [s(VarName)]))
            )
        else
            util.sorry($file, $pred, "Multi-value expressions")
        ),
        Stmts = [pre_statement(StmtType,
            stmt_info(Context, UseVars, DefVars, set.init,
                stmt_always_fallsthrough))]
    ;
        StmtType0 = s_array_set_statement(_, _, _),
        util.sorry($file, $pred, "Arrays")
    ;
        StmtType0 = s_return_statement(Exprs0),
        ( if Exprs0 = [Expr0] then
            ast_to_pre_expr(!.Env, !.Varmap, Expr0, Expr, UseVars)
        else
            util.sorry($file, $pred, "Multi-value expressions")
        ),
        varmap.add_anon_var(Var, !Varmap),
        DefVars = make_singleton_set(Var),
        StmtAssign = pre_statement(s_assign(Var, Expr),
            stmt_info(Context, UseVars, DefVars, set.init,
                stmt_always_fallsthrough)),
        StmtReturn = pre_statement(s_return(Var),
            stmt_info(Context, make_singleton_set(Var), set.init, set.init,
                stmt_always_returns)),
        Stmts = [StmtAssign, StmtReturn]
    ;
        StmtType0 = s_match_statement(Expr0, Cases0),
        ast_to_pre_expr(!.Env, !.Varmap, Expr0, Expr, UseVarsExpr),
        varmap.add_anon_var(Var, !Varmap),
        StmtAssign = pre_statement(s_assign(Var, Expr),
            stmt_info(Context, UseVarsExpr, make_singleton_set(Var),
                set.init, stmt_always_fallsthrough)),

        map3_foldl(ast_to_pre_case(!.Env), Cases0, Cases,
            UseVarsCases, DefVars0, !Varmap),

        UseVars = union_list(UseVarsCases) `union` make_singleton_set(Var),
        DefVars = union_list(DefVars0),
        % The reachability information will be updated later in
        % pre.branches
        StmtMatch = pre_statement(s_match(Var, Cases),
            stmt_info(Context, UseVars, DefVars, set.init, stmt_may_return)),

        Stmts = [StmtAssign, StmtMatch]
    ;
        StmtType0 = s_ite(Cond0, Then0, Else0),
        % ITEs are syntas sugar for a match expression using booleans.

        ast_to_pre_expr(!.Env, !.Varmap, Cond0, Cond, UseVarsCond),
        varmap.add_anon_var(Var, !Varmap),
        % TODO: To avoid amberguities, we may need a way to force this
        % variable to be bool at this point in the compiler when we know that
        % it's a bool.
        StmtAssign = pre_statement(s_assign(Var, Cond),
            stmt_info(Context, UseVarsCond, make_singleton_set(Var),
                set.init, stmt_always_fallsthrough)),

        ast_to_pre_stmts(Then0, Then, UseVarsThen, DefVarsThen, !.Env, _,
            !Varmap),
        TrueId = env_get_bool_true(!.Env),
        TrueCase = pre_case(p_constr(TrueId), Then),
        ast_to_pre_stmts(Else0, Else, UseVarsElse, DefVarsElse, !.Env, _,
            !Varmap),
        FalseId = env_get_bool_false(!.Env),
        FalseCase = pre_case(p_constr(FalseId), Else),

        UseVars = union(UseVarsThen, UseVarsElse) `union`
            make_singleton_set(Var),
        DefVars = union(DefVarsThen, DefVarsElse),
        StmtMatch = pre_statement(s_match(Var, [TrueCase, FalseCase]),
            stmt_info(Context, UseVars, DefVars, set.init, stmt_may_return)),
        Stmts = [StmtAssign, StmtMatch]
    ).

:- pred ast_to_pre_case(env::in, ast_match_case::in, pre_case::out,
    set(var)::out, set(var)::out, varmap::in, varmap::out) is det.

ast_to_pre_case(!.Env, ast_match_case(Pattern0, Stmts0),
        pre_case(Pattern, Stmts), UseVars, DefVars, !Varmap) :-
    ast_to_pre_pattern(Pattern0, Pattern, DefVarsPattern, !Env, !Varmap),
    ast_to_pre_stmts(Stmts0, Stmts, UseVars, DefVarsStmts, !Env, !Varmap),
    DefVars = DefVarsPattern `union` DefVarsStmts,
    _ = !.Env.

:- pred ast_to_pre_pattern(ast_pattern::in, pre_pattern::out, set(var)::out,
    env::in, env::out, varmap::in, varmap::out) is det.

ast_to_pre_pattern(p_number(Num), p_number(Num), set.init, !Env, !Varmap).
ast_to_pre_pattern(p_constr(Name, Args), Pattern, set.init, !Env, !Varmap) :-
    ( if env_search_constructor(!.Env, q_name(Name), CtorId) then
        ( Args = [_ | _],
            util.sorry($file, $pred, "Pattern match with arguments")
        ; Args = []
        ),
        Pattern = p_constr(CtorId)
    else
        util.compile_error($file, $pred, "Unknown constructor")
    ).

ast_to_pre_pattern(p_var(Name), Pattern, DefVars, !Env, !Varmap) :-
    ( if first_char(Name, '_', _) then
        Pattern = p_wildcard,
        DefVars = set.init
    else
        ( if env_add_var(Name, Var, !Env, !Varmap) then
            Pattern = p_var(Var),
            DefVars = make_singleton_set(Var)
        else
            compile_error($file, $pred,
                format("Variable '%s' already defined", [s(Name)]))
        )
    ).

:- pred ast_to_pre_expr(env::in, varmap::in, ast_expression::in,
    pre_expr::out, set(var)::out) is det.

ast_to_pre_expr(Env, Varmap, e_call(Call0), e_call(Call), Vars) :-
    ast_to_pre_call(Env, Varmap, Call0, Call, Vars).
ast_to_pre_expr(Env, Varmap, e_u_op(Op, SubExpr0), Expr, Vars) :-
    ast_to_pre_expr(Env, Varmap, SubExpr0, SubExpr, Vars),
    ( if env_unary_operator_func(Env, Op, OpFunc) then
        Expr = e_call(pre_call(OpFunc, [SubExpr], without_bang))
    else
        unexpected($file, $pred, "Operator implementation not found")
    ).
ast_to_pre_expr(Env, Varmap,
        e_b_op(ExprL0, Op, ExprR0), Expr, Vars) :-
    ast_to_pre_expr(Env, Varmap, ExprL0, ExprL, VarsL),
    ast_to_pre_expr(Env, Varmap, ExprR0, ExprR, VarsR),
    Vars = union(VarsL, VarsR),
    % NOTE: When introducing interfaces for primative types this will need
    % to change.
    ( if env_operator_func(Env, Op, OpFunc) then
        Expr = e_call(pre_call(OpFunc, [ExprL, ExprR], without_bang))
    else
        unexpected($file, $pred, "Operator implementation not found")
    ).
ast_to_pre_expr(Env, Varmap, e_symbol(Symbol), Expr, Vars) :-
    ( if
        env_search(Env, Symbol, Entry)
    then
        ( Entry = ee_var(Var),
            Expr = e_var(Var),
            Vars = make_singleton_set(Var)
        ; Entry = ee_constructor(Constr),
            Expr = e_construction(Constr),
            Vars = set.init
        ; Entry = ee_func(Func),
            Expr = e_constant(c_func(Func)),
            Vars = set.init
        )
    else if
        q_name_parts(Symbol, [], VarName),
        search_var(Varmap, VarName, Var)
    then
        Expr = e_var(Var),
        Vars = make_singleton_set(Var)
    else
        compile_error($file, $pred,
            format("Unknown symbol: %s", [s(q_name_to_string(Symbol))]))
    ).
ast_to_pre_expr(_, _, e_const(Const0), e_constant((Const)), init) :-
    ( Const0 = c_string(String),
        Const = c_string(String)
    ; Const0 = c_number(Number),
        Const = c_number(Number)
    ; Const0 = c_list_nil,
        util.sorry($file, $pred, "Lists")
    ).
ast_to_pre_expr(_, _, e_array(_), _, _) :-
    util.sorry($file, $pred, "Arrays").

:- pred ast_to_pre_call(env::in, varmap::in,
    ast_call::in, pre_call::out, set(var)::out) is det.

ast_to_pre_call(Env, Varmap, Call0, Call, Vars) :-
    ( Call0 = ast_call(CalleeExpr0, Args0)
    ; Call0 = ast_bang_call(CalleeExpr0, Args0)
    ),
    ast_to_pre_expr(Env, Varmap, CalleeExpr0, CalleeExpr, CalleeVars),
    ( if CalleeExpr = e_constant(c_func(Callee)) then
        map2(ast_to_pre_expr(Env, Varmap), Args0, Args, Varss),
        Vars = union_list(Varss),
        ( Call0 = ast_call(_, _),
            Call = pre_call(Callee, Args, without_bang)
        ; Call0 = ast_bang_call(_, _),
            Call = pre_call(Callee, Args, with_bang)
        )
    else
        _ = CalleeVars, % we would need this here.
        util.sorry($file, $pred, "Higher order call: " ++ string(CalleeExpr0))
    ).

