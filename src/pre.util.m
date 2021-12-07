%-----------------------------------------------------------------------%
% Plasma AST symbol resolution
% vim: ts=4 sw=4 et
%
% Copyright (C) 2019-2021 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
% This module computes nonlocals within the pre-core representation.
%
%-----------------------------------------------------------------------%
:- module pre.util.
%-----------------------------------------------------------------------%
:- interface.

:- import_module list.

:- import_module pre.pre_ds.

%-----------------------------------------------------------------------%

:- pred update_lambdas_this_stmt(pred(pre_lambda, pre_lambda, T, T),
    pre_statement, pre_statement, T, T).
:- mode update_lambdas_this_stmt(pred(in, out, in, out) is det,
    in, out, in, out) is det.

:- pred update_lambdas_this_stmt_2(pred(pre_lambda, pre_lambda, T, T, U, U),
    pre_statement, pre_statement, T, T, U, U).
:- mode update_lambdas_this_stmt_2(pred(in, out, in, out, in, out) is det,
    in, out, in, out, in, out) is det.

    % This returns only the lambdas found directly.  it won't recurse into
    % lambdas and return any inside them.
    %
:- func get_all_lambdas_stmts(pre_statements) = list(pre_lambda).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
:- implementation.

%-----------------------------------------------------------------------%

update_lambdas_this_stmt(Update, pre_statement(Type0, Info),
        pre_statement(Type, Info), !Acc) :-
    ( Type0 = s_call(Call0),
        update_lambdas_call(Update, Call0, Call, !Acc),
        Type = s_call(Call)
    ; Type0 = s_decl_vars(_),
        Type = Type0
    ; Type0 = s_assign(Var, Exprs0),
        map_foldl(update_lambdas_expr(Update), Exprs0, Exprs, !Acc),
        Type = s_assign(Var, Exprs)
    ; Type0 = s_return(_),
        Type = Type0
    ; Type0 =  s_match(_, _),
        % We expect our caller to recurse into nested statements.
        Type = Type0
    ).

update_lambdas_this_stmt_2(Update, pre_statement(Type0, Info),
        pre_statement(Type, Info), !Acc1, !Acc2) :-
    ( Type0 = s_call(Call0),
        update_lambdas_call_2(Update, Call0, Call, !Acc1, !Acc2),
        Type = s_call(Call)
    ; Type0 = s_decl_vars(_),
        Type = Type0
    ; Type0 = s_assign(Var, Exprs0),
        map_foldl2(update_lambdas_expr_2(Update), Exprs0, Exprs, !Acc1, !Acc2),
        Type = s_assign(Var, Exprs)
    ; Type0 = s_return(_),
        Type = Type0
    ; Type0 =  s_match(_, _),
        % We expect our caller to recurse into nested statements.
        Type = Type0
    ).

:- pred update_lambdas_call(pred(pre_lambda, pre_lambda, T, T),
    pre_call, pre_call, T, T).
:- mode update_lambdas_call(pred(in, out, in, out) is det,
    in, out, in, out) is det.

update_lambdas_call(Update, pre_call(Func, Args0, Bang),
        pre_call(Func, Args, Bang), !Acc) :-
    map_foldl(update_lambdas_expr(Update), Args0, Args, !Acc).
update_lambdas_call(Update, pre_ho_call(Ho0, Args0, Bang),
        pre_ho_call(Ho, Args, Bang), !Acc) :-
    update_lambdas_expr(Update, Ho0, Ho, !Acc),
    map_foldl(update_lambdas_expr(Update), Args0, Args, !Acc).

:- pred update_lambdas_call_2(pred(pre_lambda, pre_lambda, T, T, U, U),
    pre_call, pre_call, T, T, U, U).
:- mode update_lambdas_call_2(pred(in, out, in, out, in, out) is det,
    in, out, in, out, in, out) is det.

update_lambdas_call_2(Update, pre_call(Func, Args0, Bang),
        pre_call(Func, Args, Bang), !Acc1, !Acc2) :-
    map_foldl2(update_lambdas_expr_2(Update), Args0, Args, !Acc1, !Acc2).
update_lambdas_call_2(Update, pre_ho_call(Ho0, Args0, Bang),
        pre_ho_call(Ho, Args, Bang), !Acc1, !Acc2) :-
    update_lambdas_expr_2(Update, Ho0, Ho, !Acc1, !Acc2),
    map_foldl2(update_lambdas_expr_2(Update), Args0, Args, !Acc1, !Acc2).

:- pred update_lambdas_expr(pred(pre_lambda, pre_lambda, T, T),
    pre_expr, pre_expr, T, T).
:- mode update_lambdas_expr(pred(in, out, in, out) is det,
    in, out, in, out) is det.

update_lambdas_expr(Update, e_call(Call0), e_call(Call), !Acc) :-
    update_lambdas_call(Update, Call0, Call, !Acc).
update_lambdas_expr(Update, e_match(Expr0, Cases0), e_match(Expr, Cases),
        !Acc) :-
    update_lambdas_expr(Update, Expr0, Expr, !Acc),
    map_foldl(update_lambdas_case(Update), Cases0, Cases, !Acc).
update_lambdas_expr(_, e_var(Var), e_var(Var), !Acc).
update_lambdas_expr(Update, e_construction(Ctors, Args0),
        e_construction(Ctors, Args), !Acc) :-
    map_foldl(update_lambdas_expr(Update), Args0, Args, !Acc).
update_lambdas_expr(Update, e_lambda(Lambda0), e_lambda(Lambda), !Acc) :-
    Update(Lambda0, Lambda, !Acc).
update_lambdas_expr(_, e_constant(Const), e_constant(Const), !Acc).

:- pred update_lambdas_expr_2(pred(pre_lambda, pre_lambda, T, T, U, U),
    pre_expr, pre_expr, T, T, U, U).
:- mode update_lambdas_expr_2(pred(in, out, in, out, in, out) is det,
    in, out, in, out, in, out) is det.

update_lambdas_expr_2(Update, e_call(Call0), e_call(Call), !Acc1, !Acc2) :-
    update_lambdas_call_2(Update, Call0, Call, !Acc1, !Acc2).
update_lambdas_expr_2(Update, e_match(Expr0, Cases0), e_match(Expr, Cases),
        !Acc1, !Acc2) :-
    update_lambdas_expr_2(Update, Expr0, Expr, !Acc1, !Acc2),
    map_foldl2(update_lambdas_case_2(Update), Cases0, Cases, !Acc1, !Acc2).
update_lambdas_expr_2(_, e_var(Var), e_var(Var), !Acc1, !Acc2).
update_lambdas_expr_2(Update, e_construction(Ctors, Args0),
        e_construction(Ctors, Args), !Acc1, !Acc2) :-
    map_foldl2(update_lambdas_expr_2(Update), Args0, Args, !Acc1, !Acc2).
update_lambdas_expr_2(Update, e_lambda(Lambda0), e_lambda(Lambda),
        !Acc1, !Acc2) :-
    Update(Lambda0, Lambda, !Acc1, !Acc2).
update_lambdas_expr_2(_, e_constant(Const), e_constant(Const), !Acc1, !Acc2).

:- pred update_lambdas_case(pred(pre_lambda, pre_lambda, T, T),
    pre_expr_case, pre_expr_case, T, T).
:- mode update_lambdas_case(pred(in, out, in, out) is det,
    in, out, in, out) is det.

update_lambdas_case(Update, pre_e_case(Pat, Expr0), pre_e_case(Pat, Expr),
        !Acc) :-
    map_foldl(update_lambdas_expr(Update), Expr0, Expr, !Acc).

:- pred update_lambdas_case_2(pred(pre_lambda, pre_lambda, T, T, U, U),
    pre_expr_case, pre_expr_case, T, T, U, U).
:- mode update_lambdas_case_2(pred(in, out, in, out, in, out) is det,
    in, out, in, out, in, out) is det.

update_lambdas_case_2(Update, pre_e_case(Pat, Expr0), pre_e_case(Pat, Expr),
        !Acc1, !Acc2) :-
    map_foldl2(update_lambdas_expr_2(Update), Expr0, Expr, !Acc1, !Acc2).

%-----------------------------------------------------------------------%

get_all_lambdas_stmts(Stmts) = condense(map(get_all_lambdas_stmt, Stmts)).

:- func get_all_lambdas_stmt(pre_statement) = list(pre_lambda).

get_all_lambdas_stmt(pre_statement(Type, _)) = Lambdas :-
    ( Type = s_call(Call),
        Lambdas = get_all_lambdas_call(Call)
    ; Type = s_decl_vars(_),
        Lambdas = []
    ; Type = s_assign(_, Exprs),
        Lambdas = condense(map(get_all_lambdas_expr, Exprs))
    ; Type = s_return(_),
        Lambdas = []
    ; Type = s_match(_, Cases),
        Lambdas = condense(map(get_all_lambdas_case, Cases))
    ).

:- func get_all_lambdas_call(pre_call) = list(pre_lambda).

get_all_lambdas_call(Call) = condense(map(get_all_lambdas_expr, Args)) :-
    ( Call = pre_call(_, Args, _)
    ; Call = pre_ho_call(_, Args, _)
    ).

:- func get_all_lambdas_expr(pre_expr) = list(pre_lambda).

get_all_lambdas_expr(Expr) = Lambdas :-
    ( Expr = e_call(Call),
        Lambdas = get_all_lambdas_call(Call)
    ; Expr = e_match(MatchExpr, Cases),
        Lambdas = get_all_lambdas_expr(MatchExpr) ++ condense(map(
            func(pre_e_case(_, Es)) =
                condense(map(get_all_lambdas_expr, Es)),
            Cases))
    ;
        ( Expr = e_var(_)
        ; Expr = e_constant(_)
        ),
        Lambdas = []
    ; Expr = e_construction(_, Args),
        Lambdas = condense(map(get_all_lambdas_expr, Args))
    ; Expr = e_lambda(Lambda),
        Lambdas = [Lambda]
    ).

:- func get_all_lambdas_case(pre_case) = list(pre_lambda).

get_all_lambdas_case(pre_case(_, Stmts)) = get_all_lambdas_stmts(Stmts).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
