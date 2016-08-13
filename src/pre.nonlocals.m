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

:- import_module varmap.

% XXX Temporary
:- import_module ast.

%-----------------------------------------------------------------------%

:- pred compute_nonlocals_stmts(set(var)::in,
    list(ast_statement(stmt_info_varsets))::in,
    list(ast_statement(stmt_info_varsets))::out) is det.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
:- implementation.

%-----------------------------------------------------------------------%

compute_nonlocals_stmts(_, [], []).
compute_nonlocals_stmts(DefVars0, [Stmt0 | Stmts0], [Stmt | Stmts]) :-
    compute_nonlocals_stmt(DefVars0, Stmt0, Stmt1),

    StmtInfo0 = Stmt1 ^ ast_stmt_info,
    StmtInfo0 = stmt_info_varsets(Context, StmtDefVars, StmtUseVars,
        _),

    StmtNonLocals = DefVars0 `intersect` StmtUseVars,
    DefVars = DefVars0 `union` StmtDefVars,

    StmtInfo = stmt_info_varsets(Context, StmtDefVars, StmtUseVars,
        StmtNonLocals),
    Stmt = Stmt1 ^ ast_stmt_info := StmtInfo,

    compute_nonlocals_stmts(DefVars, Stmts0, Stmts).

:- pred compute_nonlocals_stmt(set(var)::in,
    ast_statement(stmt_info_varsets)::in,
    ast_statement(stmt_info_varsets)::out) is det.

compute_nonlocals_stmt(DefVars0, !Stmt) :-
    !.Stmt = ast_statement(StmtType0, StmtInfo),
    (
        ( StmtType0 = s_call(_)
        ; StmtType0 = s_assign_statement(_, _, _)
        ; StmtType0 = s_array_set_statement(_, _, _)
        ; StmtType0 = s_return_statement(_)
        ),
        StmtType = StmtType0
    ; StmtType0 = s_match_statement(Expr, Cases0),
        map(compute_nonlocals_case(DefVars0), Cases0, Cases),
        StmtType = s_match_statement(Expr, Cases)
    ),
    !:Stmt = ast_statement(StmtType, StmtInfo).

:- pred compute_nonlocals_case(set(var)::in,
    ast_match_case(stmt_info_varsets)::in,
    ast_match_case(stmt_info_varsets)::out) is det.

compute_nonlocals_case(DefVars,
        ast_match_case(Pat, Stmts0), ast_match_case(Pat, Stmts)) :-
    compute_nonlocals_stmts(DefVars, Stmts0, Stmts).

