%-----------------------------------------------------------------------%
% Plasma AST symbol resolution
% vim: ts=4 sw=4 et
%
% Copyright (C) Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
% This module computes nonlocals within the pre-core representation.
%
%-----------------------------------------------------------------------%
:- module pre.closures.
%-----------------------------------------------------------------------%

:- interface.

:- import_module pre.pre_ds.

%-----------------------------------------------------------------------%

:- pred compute_closures(pre_function::in, pre_function::out) is det.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
:- implementation.

:- import_module maybe.
:- import_module list.
:- import_module require.
:- import_module set.

:- import_module varmap.

%-----------------------------------------------------------------------%

compute_closures(!Func) :-
    Stmts0 = !.Func ^ f_body,
    filter_map(vow_is_var, !.Func ^ f_param_vars, Params),
    map_foldl(compute_closures_stmt, Stmts0, Stmts, list_to_set(Params), _),
    !Func ^ f_body := Stmts.

:- pred compute_closures_stmt(pre_statement::in, pre_statement::out,
    set(var)::in, set(var)::out) is det.

compute_closures_stmt(!Stmt, !DeclVars) :-
    !.Stmt = pre_statement(Type, Info),
    ( Type = s_call(Call0),
        compute_closures_call(!.DeclVars, Call0, Call),
        !:Stmt = pre_statement(s_call(Call), Info)
    ; Type = s_decl_vars(Vars),
        !:DeclVars = !.DeclVars `union` list_to_set(Vars)
    ; Type = s_assign(Vars, Exprs0),
        map(compute_closures_expr(!.DeclVars), Exprs0, Exprs),
        !:Stmt = pre_statement(s_assign(Vars, Exprs), Info)
    ; Type = s_return(_)
    ; Type = s_match(Var, Cases0),
        map(compute_closures_case(!.DeclVars), Cases0, Cases),
        !:Stmt = pre_statement(s_match(Var, Cases), Info)
    ).

:- pred compute_closures_case(set(var)::in, pre_case::in, pre_case::out) is det.

compute_closures_case(DeclVars, pre_case(Pat, Stmts0), pre_case(Pat, Stmts)) :-
    map_foldl(compute_closures_stmt, Stmts0, Stmts,
        DeclVars `union` pattern_all_vars(Pat), _).

:- pred compute_closures_call(set(var)::in, pre_call::in, pre_call::out) is det.

compute_closures_call(DeclVars,
        pre_call(Func, Args0, Bang), pre_call(Func, Args, Bang)) :-
    map(compute_closures_expr(DeclVars), Args0, Args).
compute_closures_call(DeclVars,
        pre_ho_call(Callee0, Args0, Bang), pre_ho_call(Callee, Args, Bang)) :-
    compute_closures_expr(DeclVars, Callee0, Callee),
    map(compute_closures_expr(DeclVars), Args0, Args).

:- pred compute_closures_expr(set(var)::in, pre_expr::in, pre_expr::out) is det.

compute_closures_expr(DeclVars, e_call(Call0), e_call(Call)) :-
    compute_closures_call(DeclVars, Call0, Call).
compute_closures_expr(DeclVars, e_match(Expr0, Cases0), e_match(Expr, Cases)) :-
    compute_closures_expr(DeclVars, Expr0, Expr),
    map(compute_closures_e_case(DeclVars), Cases0, Cases).
compute_closures_expr(_, e_var(V), e_var(V)).
compute_closures_expr(DeclVars, e_construction(Ctors, Args0),
        e_construction(Ctors, Args)) :-
    map(compute_closures_expr(DeclVars), Args0, Args).
compute_closures_expr(DeclVars, e_lambda(Lambda0), e_lambda(Lambda)) :-
    compute_closures_lambda(DeclVars, Lambda0, Lambda).
compute_closures_expr(_, e_constant(C), e_constant(C)).

:- pred compute_closures_e_case(set(var)::in,
    pre_expr_case::in, pre_expr_case::out) is det.

compute_closures_e_case(DeclVars,
        pre_e_case(Pat, Exprs0), pre_e_case(Pat, Exprs)) :-
    map(compute_closures_expr(DeclVars `union` pattern_all_vars(Pat)),
        Exprs0, Exprs).

:- pred compute_closures_lambda(set(var)::in,
    pre_lambda::in, pre_lambda::out) is det.

compute_closures_lambda(DeclVars, !Lambda) :-
    MaybeCaptured = !.Lambda ^ pl_captured,
    ( MaybeCaptured = no
    ; MaybeCaptured = yes(_),
        unexpected($file, $pred, "Expect MaybeCaptured = no")
    ),
    map_foldl(compute_closures_stmt, !.Lambda ^ pl_body, Body,
        DeclVars `union` list_to_set(ParamVars), _),
    % We have to capture this information from within the lambda, if we got
    % it from outside it could be confused with other expressions within the
    % same statement.
    DefVars = union_list(map(func(S) = S ^ s_info ^ si_def_vars, Body)),
    UseVars = union_list(map(func(S) = S ^ s_info ^ si_use_vars, Body)),
    filter_map(vow_is_var, !.Lambda ^ pl_params, ParamVars),
    Captured = (UseVars `intersect` DeclVars) `difference` DefVars
        `difference` list_to_set(ParamVars),
    !Lambda ^ pl_captured := yes(Captured),
    !Lambda ^ pl_body := Body.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
