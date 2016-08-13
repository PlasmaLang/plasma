%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module pre.to_core.
%
% Copyright (C) 2015-2016 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
% Plasma parse tree to core representation conversion
%
%-----------------------------------------------------------------------%

:- interface.

:- import_module core.
:- import_module core.code.
:- import_module pre.pre_ds.

:- import_module varmap.

%-----------------------------------------------------------------------%

:- pred pre_to_core(pre_statements::in, expr::out,
    varmap::in, varmap::out) is det.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module maybe.
:- import_module require.

:- import_module ast.
:- import_module common_types. % for const

%-----------------------------------------------------------------------%

pre_to_core([], _, !Varmap) :-
    unexpected($file, $pred, "No code to generate").
pre_to_core([Stmt | Stmts], Expr, !Varmap) :-
    ( Stmts = [],
        pre_to_core_stmt(Stmt, no, Expr, !Varmap)
    ; Stmts = [_ | _],
        pre_to_core(Stmts, StmtsExpr, !Varmap),
        pre_to_core_stmt(Stmt, yes(StmtsExpr), Expr, !Varmap)
    ).

    % pre_to_core_stmt(Statement, MaybeContinuation, Expr, !Varmap).
    %
    % Build Expr from Statement, MaybeContinuation is the code to execute after
    % Statement, if NULL then return after executing Statement.
    %
:- pred pre_to_core_stmt(pre_statement::in, maybe(expr)::in, expr::out,
    varmap::in, varmap::out) is det.

pre_to_core_stmt(Stmt, MaybeContinue, Expr, !Varmap) :-
    Stmt = pre_statement(StmtType, Info),
    Context = Info ^ si_context,
    ( StmtType = s_call(Call),
        pre_to_core_call(Context, Call, CallExpr, !Varmap),
        ( MaybeContinue = yes(Continue),
            Expr = expr(e_let([], CallExpr, Continue), code_info_init(Context))
        ; MaybeContinue = no,
            Expr = CallExpr
        )
    ; StmtType = s_assign(Var, PreExpr),
        pre_to_core_expr(Context, PreExpr, LetExpr, !Varmap),
        ( MaybeContinue = yes(Continue),
            Expr = expr(e_let([Var], LetExpr, Continue),
                code_info_init(Context))
        ; MaybeContinue = no,
            Expr = LetExpr
        )
    ; StmtType = s_return(Var),
        Expr = expr(e_var(Var), code_info_init(Context)),
        expect(unify(MaybeContinue, no), $pred, "Code after return")
    ; StmtType = s_match(_, _),
        sorry($file, $pred, "match")
    ).

:- pred pre_to_core_expr(context::in, pre_expr::in, expr::out,
    varmap::in, varmap::out) is det.

pre_to_core_expr(Context, e_call(Call), Expr, !Varmap) :-
    pre_to_core_call(Context, Call, Expr, !Varmap).
pre_to_core_expr(Context, e_var(Var),
        expr(e_var(Var), code_info_init(Context)), !Varmap).
pre_to_core_expr(Context, e_const(Const), expr(e_const(Const),
        code_info_init(Context)), !Varmap).

:- pred pre_to_core_call(context::in, pre_call::in, expr::out,
    varmap::in, varmap::out) is det.

pre_to_core_call(Context, Call, Expr, !Varmap) :-
    CodeInfo0 = code_info_init(Context),
    Call = pre_call(Callee, Args0, WithBang),
    ( WithBang = without_bang,
        CodeInfo = CodeInfo0
    ; WithBang = with_bang,
        code_info_set_using_marker(has_using_marker, CodeInfo0, CodeInfo)
    ),
    map_foldl(pre_to_core_expr(Context), Args0, ArgExprs, !Varmap),
    make_arg_vars(length(Args0), Args, !Varmap),
    Expr = expr(e_let(Args,
            expr(e_tuple(ArgExprs), code_info_init(Context)),
            expr(e_call(Callee, Args), CodeInfo)),
        code_info_init(Context)).

:- pred make_arg_vars(int::in, list(var)::out, varmap::in, varmap::out)
    is det.

make_arg_vars(Num, Vars, !Varmap) :-
    ( if Num = 0 then
        Vars = []
    else
        make_arg_vars(Num - 1, Vars0, !Varmap),
        add_anon_var(Var, !Varmap),
        Vars = [Var | Vars0]
    ).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
