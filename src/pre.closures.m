%-----------------------------------------------------------------------%
% Plasma AST symbol resolution
% vim: ts=4 sw=4 et
%
% Copyright (C) 2015-2016, 2019 Plasma Team
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

:- pred compute_closures(pre_procedure::in, pre_procedure::out) is det.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
:- implementation.

:- import_module maybe.
:- import_module require.
:- import_module set.
:- import_module unit.

:- import_module pre.util.
:- import_module varmap.

%-----------------------------------------------------------------------%

compute_closures(!Proc) :-
    Stmts0 = !.Proc ^ p_body,
    filter_map(vow_is_var, !.Proc ^ p_param_vars, Params),
    map_foldl(compute_closures_stmt, Stmts0, Stmts, set(Params), _),
    !Proc ^ p_body := Stmts.

:- pred compute_closures_stmt(pre_statement::in, pre_statement::out,
    set(var)::in, set(var)::out) is det.

compute_closures_stmt(!Stmt, !DeclVars) :-
    !.Stmt = pre_statement(Type, Info),
    ( Type = s_call(Call0),
        compute_closures_call(!.DeclVars, Call0, Call),
        !:Stmt = pre_statement(s_call(Call), Info)
    ; Type = s_decl_vars(Vars),
        !:DeclVars = !.DeclVars `union` set(Vars)
    ; Type = s_assign(Vars, Expr0),
        compute_closures_expr(!.DeclVars, Expr0, Expr),
        !:Stmt = pre_statement(s_assign(Vars, Expr), Info)
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
compute_closures_expr(_, e_var(V), e_var(V)).
compute_closures_expr(DeclVars, e_construction(Ctor, Args0),
        e_construction(Ctor, Args)) :-
    map(compute_closures_expr(DeclVars), Args0, Args).
compute_closures_expr(DeclVars, e_lambda(Lambda0), e_lambda(Lambda)) :-
    compute_closures_lambda(DeclVars, Lambda0, Lambda).
compute_closures_expr(_, e_constant(C), e_constant(C)).

:- pred compute_closures_lambda(set(var)::in,
    pre_lambda::in, pre_lambda::out) is det.

compute_closures_lambda(DeclVars, !Lambda) :-
    MaybeCaptured = !.Lambda ^ pl_captured,
    ( MaybeCaptured = no
    ; MaybeCaptured = yes(_),
        unexpected($file, $pred, "Expect MaybeCaptured = no")
    ),
    map_foldl(compute_closures_stmt, !.Lambda ^ pl_body, Body,
        DeclVars `union` set(ParamVars), _),
    % We have to capture this information from within the lambda, if we got
    % it from outside it could be confused with other expressions within the
    % same statement.
    DefVars = union_list(map(func(S) = S ^ s_info ^ si_def_vars, Body)),
    UseVars = union_list(map(func(S) = S ^ s_info ^ si_use_vars, Body)),
    filter_map(vow_is_var, !.Lambda ^ pl_params, ParamVars),
    Captured = (UseVars `intersect` DeclVars) `difference` DefVars
        `difference` set(ParamVars),
    !Lambda ^ pl_captured := yes(Captured),
    !Lambda ^ pl_body := Body.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
