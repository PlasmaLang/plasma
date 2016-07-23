%-----------------------------------------------------------------------%
% Plasma AST symbol resolution
% vim: ts=4 sw=4 et
%
% Copyright (C) 2015 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
% This module resolves symbols within the Plasma AST.
%
%-----------------------------------------------------------------------%
:- module ast.resolve.
%-----------------------------------------------------------------------%

:- interface.

:- import_module ast.env.
:- import_module varmap.

%-----------------------------------------------------------------------%

:- pred resolve_symbols_stmts(list(past_statement)::in,
    list(past_statement(stmt_info_varsets))::out, env::in, env::out,
    varmap::in, varmap::out) is det.

:- pred resolve_symbols_stmt(past_statement::in,
    past_statement(stmt_info_varsets)::out,
    env::in, env::out, varmap::in, varmap::out) is det.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
:- implementation.

:- import_module require.

%-----------------------------------------------------------------------%

resolve_symbols_stmts(!Statements, !Env, !Varmap) :-
    map_foldl2(resolve_symbols_stmt, !Statements, !Env, !Varmap).

% It seems silly to use both Env and !Varmap.  However once we add
% branching structures they will be used quite differently and we will need
% both.  Secondly Env will also capture symbols that aren't variables, such
% as modules and instances.

% XXX: Set DefSets in a seperate pass.

resolve_symbols_stmt(!Stmt, !Env, !Varmap) :-
    !.Stmt = past_statement(StmtType0, Context),
    (
        StmtType0 = ps_call(Call0),
        resolve_symbols_call(!.Env, Call0, Call, UseVars),
        DefVars = set.init,
        StmtType = ps_call(Call)
    ;
        % TODO: Raise an error if we rebind a variable (but not a module).
        StmtType0 = ps_asign_statement(VarNames, _, Exprs0),
        map2(resolve_symbols_expr(!.Env), Exprs0, Exprs, UseVarss),
        UseVars = union_list(UseVarss),
        map_foldl2(env_add_var, VarNames, Vars, !Env, !Varmap),
        DefVars = set(Vars),
        StmtType = ps_asign_statement(VarNames, yes(Vars), Exprs)
    ;
        StmtType0 = ps_array_set_statement(ArrayVar, Subscript0, RHS0),
        resolve_symbols_expr(!.Env, Subscript0, Subscript, UseVarsA),
        resolve_symbols_expr(!.Env, RHS0, RHS, UseVarsB),
        UseVars = UseVarsA `union` UseVarsB,
        DefVars = sorry($file, $pred, "How do you compute defvars here?"),
        StmtType = ps_array_set_statement(ArrayVar, Subscript, RHS)
    ;
        StmtType0 = ps_return_statement(Exprs0),
        map2(resolve_symbols_expr(!.Env), Exprs0, Exprs, UseVarss),
        UseVars = union_list(UseVarss),
        DefVars = set.init,
        StmtType = ps_return_statement(Exprs)
    ;
        StmtType0 = ps_match_statement(Expr0, Cases0),
        resolve_symbols_expr(!.Env, Expr0, Expr, UseVarsExpr),
        map_foldl(resolve_symbols_case(!.Env), Cases0, Cases, !Varmap),

        % XXX working here.
        UseVarsCases = union_list(map(
            (func(C) = union_list(
                map((func(S) = S ^ past_stmt_info ^ siv_use_vars),
                    C ^ pc_stmts))
            ), Cases)),
        UseVars = UseVarsCases `union` UseVarsExpr,
        % I think we need to set the defvars to the union of the branches'
        % defvars, otherwise we can't properly detect and report errors
        % later.
        DefVars = set.init,
        StmtType = ps_match_statement(Expr, Cases)
    ),
    !:Stmt = past_statement(StmtType,
        stmt_info_varsets(Context, UseVars, DefVars, set.init)).

:- pred resolve_symbols_case(env::in,
    past_match_case::in, past_match_case(stmt_info_varsets)::out,
    varmap::in, varmap::out) is det.

resolve_symbols_case(!.Env, past_match_case(Pattern, Stmts0),
        past_match_case(Pattern, Stmts), !Varmap) :-
    pattern_create_free_vars(Pattern, !Env, !Varmap),
    resolve_symbols_stmts(Stmts0, Stmts, !Env, !Varmap),
    _ = !.Env.

:- pred pattern_create_free_vars(past_pattern::in, env::in, env::out,
    varmap::in, varmap::out) is det.

pattern_create_free_vars(pp_number(_), !Env, !Varmap).
pattern_create_free_vars(pp_ident(Name), !Env, !Varmap) :-
    env_add_var(Name, _, !Env, !Varmap).

:- pred resolve_symbols_expr(env::in, past_expression::in,
    past_expression::out, set(var)::out) is det.

resolve_symbols_expr(Env, pe_call(Call0), pe_call(Call), Vars) :-
    resolve_symbols_call(Env, Call0, Call, Vars).
resolve_symbols_expr(Env, pe_u_op(Op, SubExpr0), Expr, Vars) :-
    resolve_symbols_expr(Env, SubExpr0, SubExpr, Vars),
    ( if env_unary_operator_func(Env, Op, OpFunc) then
        Expr = pe_call(past_call(pe_func(OpFunc), [SubExpr]))
    else
        unexpected($file, $pred, "Operator implementation not found")
    ).
resolve_symbols_expr(Env,
        pe_b_op(ExprL0, Op, ExprR0), Expr, Vars) :-
    resolve_symbols_expr(Env, ExprL0, ExprL, VarsL),
    resolve_symbols_expr(Env, ExprR0, ExprR, VarsR),
    Vars = union(VarsL, VarsR),
    % NOTE: When introducing interfaces for primative types this will need
    % to change.
    ( if env_operator_func(Env, Op, OpFunc) then
        Expr = pe_call(past_call(pe_func(OpFunc), [ExprL, ExprR]))
    else
        unexpected($file, $pred, "Operator implementation not found")
    ).
resolve_symbols_expr(_, pe_var(_), _, _) :-
    unexpected($file, $pred, "var").
resolve_symbols_expr(_, pe_func(_), _, _) :-
    unexpected($file, $pred, "func").
resolve_symbols_expr(Env, pe_symbol(Symbol), Expr, Vars) :-
    ( if
        env_search(Env, Symbol, Entry)
    then
        ( Entry = ee_var(Var),
            Expr = pe_var(Var),
            Vars = make_singleton_set(Var)
        ; Entry = ee_func(Func),
            Expr = pe_func(Func),
            Vars = init
        )
    else
        unexpected($file, $pred,
            format("Unknown symbol: %s", [s(q_name_to_string(Symbol))]))
    ).
resolve_symbols_expr(_, pe_const(C), pe_const(C), init).
resolve_symbols_expr(Env, pe_array(SubExprs0), pe_array(SubExprs),
        Vars) :-
    map2(resolve_symbols_expr(Env), SubExprs0, SubExprs, Varss),
    Vars = union_list(Varss).

:- pred resolve_symbols_call(env::in,
    past_call::in, past_call::out, set(var)::out) is det.

resolve_symbols_call(Env, Call0, Call, Vars) :-
    ( Call0 = past_call(Callee0, Args0)
    ; Call0 = past_bang_call(Callee0, Args0)
    ),
    resolve_symbols_expr(Env, Callee0, Callee, CalleeVars),
    map2(resolve_symbols_expr(Env), Args0, Args, Varss),
    Vars = union_list(Varss) `union` CalleeVars,
    ( Call0 = past_call(_, _),
        Call = past_call(Callee, Args)
    ; Call0 = past_bang_call(_, _),
        Call = past_bang_call(Callee, Args)
    ).

