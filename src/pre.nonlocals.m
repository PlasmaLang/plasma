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
:- module pre.nonlocals.
%-----------------------------------------------------------------------%

:- interface.

:- import_module pre.pre_ds.

%-----------------------------------------------------------------------%

:- pred compute_nonlocals(pre_procedure::in, pre_procedure::out) is det.

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

compute_nonlocals(!Proc) :-
    ParamVars0 = !.Proc ^ p_param_vars,
    filter_map(vow_is_var, ParamVars0, ParamVars),
    Stmts0 = !.Proc ^ p_body,
    compute_nonlocals_stmts(set(ParamVars), Stmts0, Stmts1),
    compute_nonlocals_stmts_rev(set.init, _, Stmts1, Stmts),
    !Proc ^ p_body := Stmts.

%-----------------------------------------------------------------------%

    % Process the statements in forward-order.  Later statements update
    % their non-locals depending on the variables defined so far.
    %
:- pred compute_nonlocals_stmts(set(var)::in,
    pre_statements::in, pre_statements::out) is det.

compute_nonlocals_stmts(_, [], []).
compute_nonlocals_stmts(DefVars0, [Stmt0 | Stmts0], [Stmt | Stmts]) :-
    compute_nonlocals_stmt(DefVars0, Stmt0, Stmt1),

    stmt_info(_, StmtUseVars, StmtDefVars, _, _) = Stmt1 ^ s_info,
    StmtNonLocals = DefVars0 `intersect` StmtUseVars,
    DefVars = DefVars0 `union` StmtDefVars,
    Stmt = Stmt1 ^ s_info ^ si_non_locals := StmtNonLocals,

    compute_nonlocals_stmts(DefVars, Stmts0, Stmts).

:- pred compute_nonlocals_stmt(set(var)::in,
    pre_statement::in, pre_statement::out) is det.

compute_nonlocals_stmt(DefVars, !Stmt) :-
    !.Stmt = pre_statement(StmtType0, StmtInfo),
    (
        ( StmtType0 = s_call(_)
        ; StmtType0 = s_decl_vars(_)
        ; StmtType0 = s_assign(_, _)
        ; StmtType0 = s_return(_)
        ),
        StmtType = StmtType0
    ; StmtType0 = s_match(Expr, Cases0),
        map(compute_nonlocals_case(DefVars), Cases0, Cases),
        StmtType = s_match(Expr, Cases)
    ),
    !:Stmt = pre_statement(StmtType, StmtInfo),
    update_lambdas_this_stmt(compute_nonlocals_lambda(DefVars), !Stmt,
        unit, _).

:- pred compute_nonlocals_case(set(var)::in, pre_case::in, pre_case::out)
    is det.

compute_nonlocals_case(DefVars0, pre_case(Pat, Stmts0), pre_case(Pat, Stmts)) :-
    DefVarsPat = pattern_all_vars(Pat),
    DefVars = DefVarsPat `union` DefVars0,
    compute_nonlocals_stmts(DefVars, Stmts0, Stmts).

:- pred compute_nonlocals_lambda(set(var)::in, pre_lambda::in,
    pre_lambda::out, unit::in, unit::out) is det.

compute_nonlocals_lambda(DefVars,
        pre_lambda(FuncId, Params0, MaybeCaptured, Arity, Body0),
        pre_lambda(FuncId, Params0, MaybeCaptured, Arity, Body),
        !Unit) :-
    filter_map(vow_is_var, Params0, Params),
    DefVarsInner = DefVars `union` set(Params),
    compute_nonlocals_stmts(DefVarsInner, Body0, Body).

%-----------------------------------------------------------------------%

    % Process the statements in reverse-order.  Earlier statements update
    % their non-locals depending on variables used later (and defined in
    % that statement).
    %
:- pred compute_nonlocals_stmts_rev(set(var)::in, set(var)::out,
    pre_statements::in, pre_statements::out) is det.

compute_nonlocals_stmts_rev(UseVars, UseVars, [], []).
compute_nonlocals_stmts_rev(UseVars0, UseVars,
        [Stmt0 | Stmts0], [Stmt | Stmts]) :-
    compute_nonlocals_stmts_rev(UseVars0, UseVars1, Stmts0, Stmts),

    stmt_info(_, StmtUseVars, StmtDefVars, StmtNonLocals0, _) = Stmt0 ^ s_info,

    StmtNonLocals = StmtNonLocals0 `union`
        (StmtDefVars `intersect` UseVars1),
    Stmt1 = Stmt0 ^ s_info ^ si_non_locals := StmtNonLocals,

    compute_nonlocals_stmt_rev(UseVars1, Stmt1, Stmt),

    UseVars = UseVars1 `union` StmtUseVars.

:- pred compute_nonlocals_stmt_rev(set(var)::in,
    pre_statement::in, pre_statement::out) is det.

compute_nonlocals_stmt_rev(UseVars, !Stmt) :-
    update_lambdas_this_stmt(compute_nonlocals_lambda_rev, !Stmt, unit, _),
    !.Stmt = pre_statement(StmtType0, StmtInfo),
    (
        ( StmtType0 = s_call(_)
        ; StmtType0 = s_decl_vars(_)
        ; StmtType0 = s_assign(_, _)
        ; StmtType0 = s_return(_)
        ),
        StmtType = StmtType0
    ; StmtType0 = s_match(Expr, Cases0),
        map(compute_nonlocals_case_rev(UseVars), Cases0, Cases),
        StmtType = s_match(Expr, Cases)
    ),
    !:Stmt = pre_statement(StmtType, StmtInfo).

:- pred compute_nonlocals_case_rev(set(var)::in, pre_case::in, pre_case::out)
    is det.

compute_nonlocals_case_rev(UseVars,
        pre_case(Pat, Stmts0), pre_case(Pat, Stmts)) :-
    compute_nonlocals_stmts_rev(UseVars, _, Stmts0, Stmts).

:- pred compute_nonlocals_lambda_rev(pre_lambda::in, pre_lambda::out,
    unit::in, unit::out) is det.

compute_nonlocals_lambda_rev(!Lambda, !Unit) :-
    MaybeCaptured = !.Lambda ^ pl_captured,
    ( MaybeCaptured = no
    ; MaybeCaptured = yes(_),
        unexpected($file, $pred, "Expect MaybeCaptured = no")
    ),
    compute_nonlocals_stmts_rev(set.init, UseVars, !.Lambda ^ pl_body, Body),
    % We have to capture this information from within the lambda, if we got
    % it from outside it could be confused with other expressions within the
    % same statement.
    NonLocals = union_list(map(func(S) = S ^ s_info ^ si_non_locals, Body)),
    DefVars = union_list(map(func(S) = S ^ s_info ^ si_def_vars, Body)),
    filter_map(vow_is_var, !.Lambda ^ pl_params, ParamVars),
    Captured = (UseVars `intersect` NonLocals) `difference` DefVars
        `difference` set(ParamVars),
    !Lambda ^ pl_captured := yes(Captured),
    !Lambda ^ pl_body := Body.

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
    map_foldl(compute_closures_stmt, !.Lambda ^ pl_body, Body, DeclVars, _),
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
