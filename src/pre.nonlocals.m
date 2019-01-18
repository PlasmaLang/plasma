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

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
:- implementation.

:- import_module set.

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
        ; StmtType0 = s_assign(_, _)
        ; StmtType0 = s_return(_)
        ),
        StmtType = StmtType0
    ; StmtType0 = s_match(Expr, Cases0),
        map(compute_nonlocals_case(DefVars), Cases0, Cases),
        StmtType = s_match(Expr, Cases)
    ),
    !:Stmt = pre_statement(StmtType, StmtInfo),
    update_lambdas_this_stmt(compute_nonlocals_lambda(DefVars), !Stmt).

:- pred compute_nonlocals_case(set(var)::in, pre_case::in, pre_case::out)
    is det.

compute_nonlocals_case(DefVars0, pre_case(Pat, Stmts0), pre_case(Pat, Stmts)) :-
    DefVarsPat = pattern_all_vars(Pat),
    DefVars = DefVarsPat `union` DefVars0,
    compute_nonlocals_stmts(DefVars, Stmts0, Stmts).

:- pred compute_nonlocals_lambda(set(var)::in, pre_expr::in(e_lambda),
    pre_expr::out(e_lambda)) is det.

compute_nonlocals_lambda(DefVars, e_lambda(FuncId, Params0, Arity, Body0),
        e_lambda(FuncId, Params0, Arity, Body)) :-
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
    update_lambdas_this_stmt(compute_nonlocals_lambda_rev, !Stmt),
    !.Stmt = pre_statement(StmtType0, StmtInfo),
    (
        ( StmtType0 = s_call(_)
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

:- pred compute_nonlocals_lambda_rev(pre_expr::in(e_lambda),
    pre_expr::out(e_lambda)) is det.

compute_nonlocals_lambda_rev(e_lambda(FuncId, Params, Arity, Body0),
        e_lambda(FuncId, Params, Arity, Body)) :-
    compute_nonlocals_stmts_rev(set.init, _, Body0, Body).

%-----------------------------------------------------------------------%
