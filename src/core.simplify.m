%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module core.simplify.
%
% Copyright (C) 2018-2019 Plasma Team
% Distributed under the terms of the MIT see ../LICENSE.code
%
% Plasma simplifcation step
%
% This compiler stage does a simplification pass.
%
%-----------------------------------------------------------------------%
:- interface.

:- import_module compile_error.
:- import_module result.

:- pred simplify(errors(compile_error)::out, core::in, core::out) is det.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
:- implementation.

:- import_module core.util.

%-----------------------------------------------------------------------%

simplify(Errors, !Core) :-
    process_noerror_funcs(simplify_func, Errors, !Core).

:- pred simplify_func(core::in, func_id::in, function::in,
    result(function, compile_error)::out) is det.

simplify_func(_Core, _FuncId, !.Func, ok(!:Func)) :-
    ( if
        func_get_body(!.Func, Varmap, Params, Expr0),
        func_get_vartypes(!.Func, VarTypes)
    then
        simplify_expr(Expr0, Expr),
        func_set_body(Varmap, Params, Expr, VarTypes, !Func)
    else
        unexpected($file, $pred, "Body missing")
    ).

:- pred simplify_expr(expr::in, expr::out) is det.

simplify_expr(!Expr) :-
    ExprType = !.Expr ^ e_type,
    ( ExprType = e_tuple(Exprs0),
        map(simplify_expr, Exprs0, Exprs),
        ( if Exprs = [Expr] then
            !:Expr = Expr
        else
            !Expr ^ e_type := e_tuple(Exprs)
        )
    ; ExprType = e_let(Vars, LetExpr0, InExpr0),
        simplify_expr(LetExpr0, LetExpr),
        simplify_expr(InExpr0, InExpr),
        ( if is_empty_tuple_expr(LetExpr) then
            expect(unify(Vars, []), $file, $pred, "Bad empty let"),
            !:Expr = InExpr
        else if
            is_empty_tuple_expr(InExpr),
            Vars = []
        then
            !:Expr = LetExpr
        else if
            Vars = [Var],
            InExpr = expr(e_var(Var), _)
            % This is untested, so I've commented out, it should be disjoint
            % with the above test.  And should be made more granular in the
            % future so that only part of the expression is replaced.
            %    InExpr = expr(e_tuple(Exprs), _),
            %    map((pred(E::in, V::in) is semidet :-
            %            E = expr(e_var(V), _)
            %        ), Exprs, Vars)
        then
            !:Expr = LetExpr
        else
            !Expr ^ e_type := e_let(Vars, LetExpr, InExpr)
        )
    ; ExprType = e_call(_, _, _)
    ; ExprType = e_var(_)
    ; ExprType = e_constant(_)
    ; ExprType = e_construction(_, _)
    ; ExprType = e_closure(_, _)
    ; ExprType = e_match(Vars, Cases0),
        map(simplify_case, Cases0, Cases),
        !Expr ^ e_type := e_match(Vars, Cases)
    ).

:- pred simplify_case(expr_case::in, expr_case::out) is det.

simplify_case(e_case(Pat, !.Expr), e_case(Pat, !:Expr)) :-
    simplify_expr(!Expr).

%-----------------------------------------------------------------------%

:- pred is_empty_tuple_expr(expr::in) is semidet.

is_empty_tuple_expr(Expr) :-
    Expr = expr(e_tuple([]), _).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
