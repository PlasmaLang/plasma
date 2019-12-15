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
        func_get_body(!.Func, Varmap, Params, Captured, Expr0),
        func_get_vartypes(!.Func, VarTypes)
    then
        simplify_expr(Expr0, Expr),
        func_set_body(Varmap, Params, Captured, Expr, VarTypes, !Func)
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
    ; ExprType = e_lets(Lets0, InExpr0),
        simplify_lets(Lets0, [], Lets),
        simplify_expr(InExpr0, InExpr),
        ( if
            is_empty_tuple_expr(InExpr),
            Lets = [e_let([], LetExpr)]
        then
            !:Expr = LetExpr
        else
            !Expr ^ e_type := e_lets(Lets, InExpr)
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

% TODO:
%  * Remove single-use variables
%  * Remove aliased variables, including through tuples.
:- pred simplify_lets(list(expr_let)::in, list(expr_let)::in,
    list(expr_let)::out) is det.

simplify_lets([], !Lets) :-
    reverse(!Lets).
simplify_lets([L | Ls0], !RevLets) :-
    L = e_let(Vars, Expr0),
    simplify_expr(Expr0, Expr),
    ( if is_empty_tuple_expr(Expr) then
        expect(unify(Vars, []), $file, $pred, "Bad empty let"),
        % Discard L
        Ls = Ls0
    else if Expr = expr(e_tuple(Exprs), _) then
        Lets = map_corresponding(func(V, E) = e_let([V], E), Vars, Exprs),
        Ls = Lets ++ Ls0
    else if
        Expr = expr(e_lets(InnerLets, InnerExpr), _),
        % Disable this optimisation until we can re-nest otherwise the code
        % generator can't handle the flatterned expressions.
        semidet_false
    then
        % Flattern inner lets.
        Ls = InnerLets ++ [e_let(Vars, InnerExpr)] ++ Ls0
    else
        Ls = Ls0,
        !:RevLets = [e_let(Vars, Expr) | !.RevLets]
    ),
    simplify_lets(Ls, !RevLets).

:- pred simplify_case(expr_case::in, expr_case::out) is det.

simplify_case(e_case(Pat, !.Expr), e_case(Pat, !:Expr)) :-
    simplify_expr(!Expr).

%-----------------------------------------------------------------------%

:- pred is_empty_tuple_expr(expr::in) is semidet.

is_empty_tuple_expr(Expr) :-
    Expr = expr(e_tuple([]), _).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
