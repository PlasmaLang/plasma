%-----------------------------------------------------------------------%
% Plasma AST symbol resolution
% vim: ts=4 sw=4 et
%
% Copyright (C) 2019 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
% This module computes nonlocals within the pre-core representation.
%
%-----------------------------------------------------------------------%
:- module pre.util.
%-----------------------------------------------------------------------%
:- interface.

:- import_module pre.pre_ds.

%-----------------------------------------------------------------------%

:- pred update_lambdas_this_stmt(pred(pre_expr, pre_expr),
    pre_statement, pre_statement).
:- mode update_lambdas_this_stmt(pred(in(e_lambda), out(e_lambda)) is det,
    in, out) is det.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
:- implementation.

%-----------------------------------------------------------------------%

update_lambdas_this_stmt(Update, pre_statement(Type0, Info),
        pre_statement(Type, Info)) :-
    ( Type0 = s_call(Call0),
        update_lambdas_call(Update, Call0, Call),
        Type = s_call(Call)
    ; Type0 = s_assign(Var, Expr0),
        update_lambdas_expr(Update, Expr0, Expr),
        Type = s_assign(Var, Expr)
    ; Type0 = s_return(_),
        Type = Type0
    ; Type0 =  s_match(_, _),
        % We expect our caller to recurse into nested statements.
        Type = Type0
    ).

:- pred update_lambdas_call(pred(pre_expr, pre_expr), pre_call, pre_call).
:- mode update_lambdas_call(pred(in(e_lambda), out(e_lambda)) is det,
    in, out) is det.

update_lambdas_call(Update, pre_call(Func, Args0, Bang),
        pre_call(Func, Args, Bang)) :-
    map(update_lambdas_expr(Update), Args0, Args).
update_lambdas_call(Update, pre_ho_call(Ho0, Args0, Bang),
        pre_ho_call(Ho, Args, Bang)) :-
    update_lambdas_expr(Update, Ho0, Ho),
    map(update_lambdas_expr(Update), Args0, Args).

:- pred update_lambdas_expr(pred(pre_expr, pre_expr), pre_expr, pre_expr).
:- mode update_lambdas_expr(pred(in(e_lambda), out(e_lambda)) is det,
    in, out) is det.

update_lambdas_expr(Update, e_call(Call0), e_call(Call)) :-
    update_lambdas_call(Update, Call0, Call).
update_lambdas_expr(_, e_var(Var), e_var(Var)).
update_lambdas_expr(Update, e_construction(Ctor, Args0),
        e_construction(Ctor, Args)) :-
    map(update_lambdas_expr(Update), Args0, Args).
update_lambdas_expr(Update, e_lambda(Func, Params, Arity, Body0), Lambda) :-
    Update(e_lambda(Func, Params, Arity, Body0), Lambda).
update_lambdas_expr(_, e_constant(Const), e_constant(Const)).

