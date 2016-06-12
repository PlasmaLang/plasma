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

:- pred resolve_symbols_stmt(past_statement::in, past_statement::out,
    env::in, env::out, varmap::in, varmap::out) is det.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
:- implementation.

:- import_module set.
:- import_module require.

%-----------------------------------------------------------------------%

% It seems silly to use both Env and !Varmap.  However once we add
% branching structures they will be used quite differently and we will need
% both.  Secondly Env will also capture symbols that aren't variables, such
% as modules and instances.

resolve_symbols_stmt(ps_bang_call(Call0, Context),
        ps_bang_call(Call, Context), !Env, !Varmap) :-
    resolve_symbols_call(!.Env, Call0, Call, _CallVars).
resolve_symbols_stmt(ps_bang_asign_call(Vars, Call0, Context),
        ps_bang_asign_call(Vars, Call, Context), !Env, !Varmap) :-
    resolve_symbols_call(!.Env, Call0, Call, _CallVars),

    % Add the assigned variables to the environment and varmap.
    map_foldl2(env_add_var, Vars, _, !Env, !Varmap).
resolve_symbols_stmt(ps_asign_statement(VarNames, _, Exprs0, Context),
        ps_asign_statement(VarNames, yes(Vars), Exprs, Context),
        !Env, !Varmap) :-
    map2(resolve_symbols_expr(!.Env), Exprs0, Exprs, _Varss),
    map_foldl2(env_add_var, VarNames, Vars, !Env, !Varmap).
resolve_symbols_stmt(ps_return_statement(Exprs0, Context),
        ps_return_statement(Exprs, Context), !Env, !Varmap) :-
    map2(resolve_symbols_expr(!.Env), Exprs0, Exprs, _Varss).
resolve_symbols_stmt(
        ps_array_set_statement(ArrayVar, Subscript0, RHS0, Context),
        ps_array_set_statement(ArrayVar, Subscript, RHS, Context),
        !Env, !Varmap) :-
    resolve_symbols_expr(!.Env, Subscript0, Subscript, _),
    resolve_symbols_expr(!.Env, RHS0, RHS, _).

:- pred build_varmap_var(string::in, varmap::in, varmap::out) is det.

build_varmap_var(String, !Varmap) :-
    add_or_get_var(String, _, !Varmap).

:- pred resolve_symbols_expr(env::in, past_expression::in,
    past_expression::out, set(var)::out) is det.

resolve_symbols_expr(Env, pe_call(Call0), pe_call(Call), Vars) :-
    resolve_symbols_call(Env, Call0, Call, Vars).
resolve_symbols_expr(Env, pe_u_op(Op, SubExpr0), pe_u_op(Op, SubExpr),
        Vars) :-
    resolve_symbols_expr(Env, SubExpr0, SubExpr, Vars).
resolve_symbols_expr(Env,
        pe_b_op(ExprA0, Op, ExprB0), pe_b_op(ExprA, Op, ExprB), Vars) :-
    resolve_symbols_expr(Env, ExprA0, ExprA, VarsA),
    resolve_symbols_expr(Env, ExprB0, ExprB, VarsB),
    Vars = union(VarsA, VarsB).
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
        unexpected($file, $pred, "Unknown symbol")
    ).
resolve_symbols_expr(_, pe_const(C), pe_const(C), init).
resolve_symbols_expr(Env, pe_array(SubExprs0), pe_array(SubExprs),
        Vars) :-
    map2(resolve_symbols_expr(Env), SubExprs0, SubExprs, Varss),
    Vars = union_list(Varss).

:- pred resolve_symbols_call(env::in,
    past_call::in, past_call::out, set(var)::out) is det.

resolve_symbols_call(Env,
        past_call(Callee, Args0), past_call(Callee, Args), Vars) :-
    map2(resolve_symbols_expr(Env), Args0, Args, Varss),
    Vars = union_list(Varss).

