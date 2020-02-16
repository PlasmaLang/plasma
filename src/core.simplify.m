%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module core.simplify.
%
% Copyright (C) 2018-2020 Plasma Team
% Distributed under the terms of the MIT see ../LICENSE.code
%
% Plasma simplifcation step
%
% This compiler stage does a simplification pass.
%
%-----------------------------------------------------------------------%
:- interface.

:- import_module io.

:- import_module util.log.

:- import_module compile_error.
:- import_module util.result.

:- pred simplify(log_config::in, errors(compile_error)::out,
    core::in, core::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
:- implementation.

:- import_module cord.
:- import_module require.

:- import_module core.util.

%-----------------------------------------------------------------------%

simplify(Verbose, Errors, !Core, !IO) :-
    process_noerror_funcs(Verbose, simplify_func, Errors, !Core, !IO).

:- pred simplify_func(core::in, func_id::in, function::in,
    result_partial(function, compile_error)::out) is det.

simplify_func(_Core, _FuncId, !.Func, ok(!:Func, init)) :-
    ( if
        func_get_body(!.Func, Varmap, Params, Captured, Expr0)
    then
        simplify_expr(map.init, Expr0, Expr),
        func_set_body(Varmap, Params, Captured, Expr, !Func)
    else
        unexpected($file, $pred, "Body missing")
    ).

:- pred simplify_expr(map(var, var)::in, expr::in, expr::out) is det.

simplify_expr(Renaming, !Expr) :-
    ExprType = !.Expr ^ e_type,
    ( ExprType = e_tuple(Exprs0),
        map(simplify_expr(Renaming), Exprs0, Exprs),
        ( if Exprs = [Expr] then
            !:Expr = Expr
        else
            !Expr ^ e_type := e_tuple(Exprs)
        )
    ; ExprType = e_lets(Lets0, InExpr0),
        simplify_lets(Lets0, [], Lets, Renaming, RenamingIn),
        rename_expr(RenamingIn, InExpr0, InExpr1),
        simplify_expr(init, InExpr1, InExpr),
        ( if
            is_empty_tuple_expr(InExpr),
            Lets = [e_let([], LetExpr)]
        then
            !:Expr = LetExpr,
            InInfo = InExpr ^ e_info,
            maybe_fixup_moved_info(InInfo, !Expr)
        else
            !Expr ^ e_type := e_lets(Lets, InExpr)
        )
    ; ExprType = e_call(_, _, _)
    ; ExprType = e_var(_)
    ; ExprType = e_constant(_)
    ; ExprType = e_construction(_, _)
    ; ExprType = e_closure(_, _)
    ; ExprType = e_match(Vars, Cases0),
        map(simplify_case(Renaming), Cases0, Cases),
        !Expr ^ e_type := e_match(Vars, Cases)
    ).

:- pred maybe_fixup_moved_info(code_info::in, expr::in, expr::out) is det.

maybe_fixup_moved_info(InInfo, !Expr) :-
    ( if code_info_origin(InInfo) = o_user_return(Context) then
        % If this expression was created when preparing a return
        % statement fixup the code info to point to the return
        % statement.
        code_info_set_origin(o_user_return(Context),
            !.Expr ^ e_info, Info),
        !Expr ^ e_info := Info
    else
        true
    ).

% TODO:
%  * Remove single-use variables
:- pred simplify_lets(list(expr_let)::in, list(expr_let)::in,
    list(expr_let)::out, map(var, var)::in, map(var, var)::out) is det.

simplify_lets([], !Lets, !Renamings) :-
    reverse(!Lets).
simplify_lets([L | Ls0], !RevLets, !Renamings) :-
    L = e_let(Vars, Expr0),
    simplify_expr(!.Renamings, Expr0, Expr1),
    rename_expr(!.Renamings, Expr1, Expr),
    ( if is_empty_tuple_expr(Expr) then
        expect(unify(Vars, []), $file, $pred, "Bad empty let"),
        % Discard L
        Ls = Ls0
    else if Expr = expr(e_tuple(Exprs), _) then
        Lets = map_corresponding(func(V, E) = e_let([V], E), Vars, Exprs),
        Ls = Lets ++ Ls0
    else if Expr = expr(e_lets(InnerLets, InnerExpr), _) then
        % Flattern inner lets.
        Ls = InnerLets ++ [e_let(Vars, InnerExpr)] ++ Ls0
    else if
        Vars = [VarDup],
        Expr = expr(e_var(VarOrig), _)
    then
        % We can drop this variable assignment by renaming the new variable
        % in the following expressions.
        map.det_insert(VarDup, VarOrig, !Renamings),
        Ls = Ls0
    else
        Ls = Ls0,
        !:RevLets = [e_let(Vars, Expr) | !.RevLets]
    ),
    simplify_lets(Ls, !RevLets, !Renamings).

:- pred simplify_case(map(var, var)::in, expr_case::in, expr_case::out) is det.

simplify_case(Renaming, e_case(Pat, !.Expr), e_case(Pat, !:Expr)) :-
    simplify_expr(Renaming, !Expr).

%-----------------------------------------------------------------------%

:- pred is_empty_tuple_expr(expr::in) is semidet.

is_empty_tuple_expr(Expr) :-
    Expr = expr(e_tuple([]), _).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
