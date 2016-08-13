%-----------------------------------------------------------------------%
% Plasma AST symbol resolution
% vim: ts=4 sw=4 et
%
% Copyright (C) 2015-2016 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
% This module computes nonlocals within the Pre-core representation.
%
%-----------------------------------------------------------------------%
:- module pre.nonlocals.
%-----------------------------------------------------------------------%

:- interface.

:- import_module set.

:- import_module pre.pre_ds.
:- import_module varmap.

%-----------------------------------------------------------------------%

:- pred compute_nonlocals_stmts(set(var)::in,
    pre_statements::in, pre_statements::out) is det.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
:- implementation.

%-----------------------------------------------------------------------%

compute_nonlocals_stmts(_, [], []).
compute_nonlocals_stmts(DefVars0, [Stmt0 | Stmts0], [Stmt | Stmts]) :-
    compute_nonlocals_stmt(DefVars0, Stmt0, Stmt1),

    stmt_info(_, StmtDefVars, StmtUseVars, _) = Stmt1 ^ s_info,
    StmtNonLocals = DefVars0 `intersect` StmtUseVars,
    DefVars = DefVars0 `union` StmtDefVars,
    Stmt = Stmt1 ^ s_info ^ si_non_locals := StmtNonLocals,

    compute_nonlocals_stmts(DefVars, Stmts0, Stmts).

:- pred compute_nonlocals_stmt(set(var)::in,
    pre_statement::in, pre_statement::out) is det.

compute_nonlocals_stmt(DefVars0, !Stmt) :-
    !.Stmt = pre_statement(StmtType0, StmtInfo),
    (
        ( StmtType0 = s_call(_)
        ; StmtType0 = s_assign(_, _)
        ; StmtType0 = s_return(_)
        ),
        StmtType = StmtType0
    ; StmtType0 = s_match(Expr, Cases0),
        map(compute_nonlocals_case(DefVars0), Cases0, Cases),
        StmtType = s_match(Expr, Cases)
    ),
    !:Stmt = pre_statement(StmtType, StmtInfo).

:- pred compute_nonlocals_case(set(var)::in, pre_case::in, pre_case::out)
    is det.

compute_nonlocals_case(DefVars, pre_case(Pat, Stmts0), pre_case(Pat, Stmts)) :-
    compute_nonlocals_stmts(DefVars, Stmts0, Stmts).

