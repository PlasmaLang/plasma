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

:- pred update_lambdas_this_stmt(pred(pre_lambda, pre_lambda, T, T),
    pre_statement, pre_statement, T, T).
:- mode update_lambdas_this_stmt(pred(in, out, in, out) is det,
    in, out, in, out) is det.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
:- implementation.

%-----------------------------------------------------------------------%

update_lambdas_this_stmt(Update, pre_statement(Type0, Info),
        pre_statement(Type, Info), !Acc) :-
    ( Type0 = s_call(Call0),
        update_lambdas_call(Update, Call0, Call, !Acc),
        Type = s_call(Call)
    ; Type0 = s_assign(Var, Expr0),
        update_lambdas_expr(Update, Expr0, Expr, !Acc),
        Type = s_assign(Var, Expr)
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

:- pred update_lambdas_expr(pred(pre_lambda, pre_lambda, T, T),
    pre_expr, pre_expr, T, T).
:- mode update_lambdas_expr(pred(in, out, in, out) is det,
    in, out, in, out) is det.

update_lambdas_expr(Update, e_call(Call0), e_call(Call), !Acc) :-
    update_lambdas_call(Update, Call0, Call, !Acc).
update_lambdas_expr(_, e_var(Var), e_var(Var), !Acc).
update_lambdas_expr(Update, e_construction(Ctor, Args0),
        e_construction(Ctor, Args), !Acc) :-
    map_foldl(update_lambdas_expr(Update), Args0, Args, !Acc).
update_lambdas_expr(Update,
        e_lambda(Func0, Params0, Arity0, Body0),
        e_lambda(Func,  Params,  Arity,  Body),
        !Acc) :-
    Update(pre_lambda(Func0, Params0, Arity0, Body0),
           pre_lambda(Func,  Params,  Arity,  Body), !Acc).
update_lambdas_expr(_, e_constant(Const), e_constant(Const), !Acc).

