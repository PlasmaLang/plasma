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
:- pred ast_to_pre(list(ast_statement)::in,
    pre_statements::out, set(var)::out,
    set(var)::out, env::in, env::out, varmap::in, varmap::out) is det.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
:- implementation.

:- import_module list.
:- import_module maybe.
:- import_module require.
:- import_module string.

:- import_module common_types.
:- import_module q_name.

%-----------------------------------------------------------------------%

ast_to_pre(Stmts0, Stmts, union_list(UseVars), union_list(DefVars), !Env,
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
        ast_to_pre_call(!.Env, Call0, Call, UseVars),
        DefVars = set.init,
        StmtType = s_call(Call),
        Stmts = [pre_statement(StmtType,
            stmt_info(Context, UseVars, DefVars, set.init))]
    ;
        % TODO: Raise an error if we rebind a variable (but not a module).
        StmtType0 = s_assign_statement(VarNames, _, Exprs0),
        ( if
            VarNames = [VarName],
            Exprs0 = [Expr0]
        then
            ast_to_pre_expr(!.Env, Expr0, Expr, UseVars),
            env_add_var(VarName, Var, !Env, !Varmap),
            DefVars = make_singleton_set(Var),
            StmtType = s_assign(Var, Expr)
        else
            sorry($file, $pred, "Multi-value expressions not yet supported")
        ),
        Stmts = [pre_statement(StmtType,
            stmt_info(Context, UseVars, DefVars, set.init))]
    ;
        StmtType0 = s_array_set_statement(_, _, _),
        sorry($file, $pred, "Arrays")
    ;
        StmtType0 = s_return_statement(Exprs0),
        ( if Exprs0 = [Expr0] then
            ast_to_pre_expr(!.Env, Expr0, Expr, UseVars)
        else
            sorry($file, $pred, "Multi-value expressions")
        ),
        varmap.add_anon_var(Var, !Varmap),
        DefVars = make_singleton_set(Var),
        StmtAssign = pre_statement(s_assign(Var, Expr),
            stmt_info(Context, UseVars, DefVars, set.init)),
        StmtReturn = pre_statement(s_return(Var),
            stmt_info(Context, make_singleton_set(Var), set.init, set.init)),
        Stmts = [StmtAssign, StmtReturn]
    ;
        StmtType0 = s_match_statement(Expr0, Cases0),
        ast_to_pre_expr(!.Env, Expr0, Expr, UseVarsExpr),
        varmap.add_anon_var(Var, !Varmap),
        StmtAssign = pre_statement(s_assign(Var, Expr),
            stmt_info(Context, UseVarsExpr, make_singleton_set(Var),
                set.init)),

        map3_foldl(ast_to_pre_case(!.Env), Cases0, Cases,
            UseVarsCases, DefVars0, !Varmap),

        UseVars = union_list(UseVarsCases) `union` make_singleton_set(Var),
        DefVars = union_list(DefVars0),
        StmtMatch = pre_statement(s_match(Var, Cases),
            stmt_info(Context, UseVars, DefVars, set.init)),

        Stmts = [StmtAssign, StmtMatch]
    ).

:- pred ast_to_pre_case(env::in, ast_match_case::in, pre_case::out,
    set(var)::out, set(var)::out, varmap::in, varmap::out) is det.

ast_to_pre_case(!.Env, ast_match_case(Pattern, Stmts0),
        pre_case(pre_pattern, Stmts), UseVars, DefVars, !Varmap) :-
    pattern_create_free_vars(Pattern, !Env, !Varmap),
    ast_to_pre(Stmts0, Stmts, UseVars, DefVars, !Env, !Varmap),
    _ = !.Env.

:- pred pattern_create_free_vars(ast_pattern::in, env::in, env::out,
    varmap::in, varmap::out) is det.

pattern_create_free_vars(p_number(_), !Env, !Varmap).
pattern_create_free_vars(p_ident(Name), !Env, !Varmap) :-
    sorry($file, $pred, "TODO: Fix pattern representation"),
    env_add_var(Name, _, !Env, !Varmap).

:- pred ast_to_pre_expr(env::in, ast_expression::in,
    pre_expr::out, set(var)::out) is det.

ast_to_pre_expr(Env, e_call(Call0), e_call(Call), Vars) :-
    ast_to_pre_call(Env, Call0, Call, Vars).
ast_to_pre_expr(Env, e_u_op(Op, SubExpr0), Expr, Vars) :-
    ast_to_pre_expr(Env, SubExpr0, SubExpr, Vars),
    ( if env_unary_operator_func(Env, Op, OpFunc) then
        Expr = e_call(pre_call(OpFunc, [SubExpr], without_bang))
    else
        unexpected($file, $pred, "Operator implementation not found")
    ).
ast_to_pre_expr(Env,
        e_b_op(ExprL0, Op, ExprR0), Expr, Vars) :-
    ast_to_pre_expr(Env, ExprL0, ExprL, VarsL),
    ast_to_pre_expr(Env, ExprR0, ExprR, VarsR),
    Vars = union(VarsL, VarsR),
    % NOTE: When introducing interfaces for primative types this will need
    % to change.
    ( if env_operator_func(Env, Op, OpFunc) then
        Expr = e_call(pre_call(OpFunc, [ExprL, ExprR], without_bang))
    else
        unexpected($file, $pred, "Operator implementation not found")
    ).
ast_to_pre_expr(_, e_var(_), _, _) :-
    unexpected($file, $pred, "var").
ast_to_pre_expr(_, e_func(_), _, _) :-
    unexpected($file, $pred, "func").
ast_to_pre_expr(Env, e_symbol(Symbol), Expr, Vars) :-
    ( if
        env_search(Env, Symbol, Entry)
    then
        ( Entry = ee_var(Var),
            Expr = e_var(Var),
            Vars = make_singleton_set(Var)
        ; Entry = ee_func(Func),
            Expr = e_func(Func),
            Vars = set.init
        )
    else
        unexpected($file, $pred,
            format("Unknown symbol: %s", [s(q_name_to_string(Symbol))]))
    ).
ast_to_pre_expr(_, e_const(Const0), e_const(Const), init) :-
    ( Const0 = c_string(String),
        Const = c_string(String)
    ; Const0 = c_number(Number),
        Const = c_number(Number)
    ; Const0 = c_list_nil,
        sorry($file, $pred, "list")
    ).
ast_to_pre_expr(_, e_array(_), _, _) :-
    sorry($file, $pred, "Arrays").

:- pred ast_to_pre_call(env::in,
    ast_call::in, pre_call::out, set(var)::out) is det.

ast_to_pre_call(Env, Call0, Call, Vars) :-
    ( Call0 = ast_call(CalleeExpr0, Args0)
    ; Call0 = ast_bang_call(CalleeExpr0, Args0)
    ),
    ast_to_pre_expr(Env, CalleeExpr0, CalleeExpr, CalleeVars),
    ( if CalleeExpr = e_func(Callee) then
        map2(ast_to_pre_expr(Env), Args0, Args, Varss),
        Vars = union_list(Varss),
        ( Call0 = ast_call(_, _),
            Call = pre_call(Callee, Args, without_bang)
        ; Call0 = ast_bang_call(_, _),
            Call = pre_call(Callee, Args, with_bang)
        )
    else
        _ = CalleeVars, % we would need this here.
        sorry($file, $pred, "Higher order call: " ++ string(CalleeExpr0))
    ).

