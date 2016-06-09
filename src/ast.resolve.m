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
:- import_module core.

%-----------------------------------------------------------------------%

:- pred resolve_symbols_stmt(core::in, past_statement::in, past_statement::out,
    env::in, env::out, varmap::in, varmap::out) is det.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
:- implementation.

:- import_module set.
:- import_module require.

%-----------------------------------------------------------------------%

% It seems silly to use both !Env and !Varmap.  However once we add
% branching structures they will be used quite differently and we will need
% both.  Secondly Env will also capture symbols that arn't variables, such
% as modules and instances.

resolve_symbols_stmt(Core, ps_bang_call(Call0, Context),
        ps_bang_call(Call, Context), !Env, !Varmap) :-
    resolve_symbols_call(Core, !.Env, Call0, Call, _CallVars).
resolve_symbols_stmt(Core, ps_bang_asign_call(Vars, Call0, Context),
        ps_bang_asign_call(Vars, Call, Context), !Env, !Varmap) :-
    resolve_symbols_call(Core, !.Env, Call0, Call, _CallVars),

    % Add the assigned variables to the environment and varmap.
    foldl(build_varmap_var, Vars, !Varmap),
    foldl(env_add_var, Vars, !Env).
resolve_symbols_stmt(Core, ps_asign_statement(Vars, Exprs0, Context),
        ps_asign_statement(Vars, Exprs, Context), !Env, !Varmap) :-
    map2(resolve_symbols_expr(Core, !.Env), Exprs0, Exprs, _Varss),
    foldl(build_varmap_var, Vars, !Varmap),
    foldl(env_add_var, Vars, !Env).
resolve_symbols_stmt(Core, ps_return_statement(Exprs0, Context),
        ps_return_statement(Exprs, Context), !Env, !Varmap) :-
    map2(resolve_symbols_expr(Core, !.Env), Exprs0, Exprs, _Varss).
resolve_symbols_stmt(Core,
        ps_array_set_statement(ArrayVar, Subscript0, RHS0, Context),
        ps_array_set_statement(ArrayVar, Subscript, RHS, Context),
        !Env, !Varmap) :-
    resolve_symbols_expr(Core, !.Env, Subscript0, Subscript, _),
    resolve_symbols_expr(Core, !.Env, RHS0, RHS, _).

:- pred build_varmap_var(string::in, varmap::in, varmap::out) is det.

build_varmap_var(String, !Varmap) :-
    add_or_get_var(String, _, !Varmap).

:- pred resolve_symbols_expr(core::in, env::in, past_expression::in,
    past_expression::out, set(string)::out) is det.

resolve_symbols_expr(Core, Env, pe_call(Call0), pe_call(Call), Vars) :-
    resolve_symbols_call(Core, Env, Call0, Call, Vars).
resolve_symbols_expr(Core, Env, pe_u_op(Op, SubExpr0), pe_u_op(Op, SubExpr),
        Vars) :-
    resolve_symbols_expr(Core, Env, SubExpr0, SubExpr, Vars).
resolve_symbols_expr(Core, Env,
        pe_b_op(ExprA0, Op, ExprB0), pe_b_op(ExprA, Op, ExprB), Vars) :-
    resolve_symbols_expr(Core, Env, ExprA0, ExprA, VarsA),
    resolve_symbols_expr(Core, Env, ExprB0, ExprB, VarsB),
    Vars = union(VarsA, VarsB).
resolve_symbols_expr(_, _, pe_var(_), _, _) :-
    unexpected($file, $pred, "var").
resolve_symbols_expr(_, _, pe_func(_), _, _) :-
    unexpected($file, $pred, "func").
resolve_symbols_expr(Core, Env, pe_symbol(Symbol), Expr, Vars) :-
    % TODO: How do we know this is a local variable?
    ( if
        symbol_parts(Symbol, [], String),
        env_has_var(Env, String)
    then
        Expr = pe_var(String),
        Vars = make_singleton_set(String)
    else if
        core_search_function(Core, Symbol, Funcs),
        singleton_set(Func, Funcs)
    then
        Expr = pe_func(Func),
        Vars = init
    else
        unexpected($file, $pred, "Unknown symbol")
    ).
resolve_symbols_expr(_, _, pe_const(C), pe_const(C), init).
resolve_symbols_expr(Core, Env, pe_array(SubExprs0), pe_array(SubExprs),
        Vars) :-
    map2(resolve_symbols_expr(Core, Env), SubExprs0, SubExprs, Varss),
    Vars = union_list(Varss).

:- pred resolve_symbols_call(core::in, env::in,
    past_call::in, past_call::out, set(string)::out) is det.

resolve_symbols_call(Core, Env,
        past_call(Callee, Args0), past_call(Callee, Args), Vars) :-
    map2(resolve_symbols_expr(Core, Env), Args0, Args, Varss),
    Vars = union_list(Varss).

