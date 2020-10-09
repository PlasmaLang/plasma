%-----------------------------------------------------------------------%
% Plasma AST symbol resolution
% vim: ts=4 sw=4 et
%
% Copyright (C) 2017-2020 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
%-----------------------------------------------------------------------%
:- module pre.bang.
%-----------------------------------------------------------------------%

:- interface.

:- import_module compile_error.
:- import_module core.
:- import_module pre.pre_ds.
:- import_module util.
:- import_module util.result.

%-----------------------------------------------------------------------%

:- func check_bangs(core, pre_function) = errors(compile_error).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

:- import_module cord.
:- import_module int.
:- import_module list.

:- import_module context.
:- import_module common_types.

%-----------------------------------------------------------------------%

% Most of the resource checking is done in a stage after type checking,
% (core.res_chk.m) so that type information is available for higher-order
% values.  The only check done here is whether there are multiple bangs in a
% single statement.

check_bangs(_Core, Func) =
        cord_list_to_cord(map(check_bangs_stmt, Stmts)) :-
    Stmts = Func ^ f_body.

:- func check_bangs_stmt(pre_statement) = errors(compile_error).

check_bangs_stmt(Stmt) = !:Errors :-
    !:Errors = init,
    StmtType = Stmt ^ s_type,
    Context = Stmt ^ s_info ^ si_context,
    ( StmtType = s_call(Call),
        check_bangs_call(Context, Call, ExprsWithBang, StmtErrors),
        add_errors(StmtErrors, !Errors)
    ; StmtType = s_decl_vars(_),
        ExprsWithBang = 0
    ; StmtType = s_assign(_, Exprs),
        map2(check_bangs_expr(Context), Exprs, ExprsWithBangs, StmtErrors),
        ExprsWithBang = sum(ExprsWithBangs),
        add_errors(cord_list_to_cord(StmtErrors), !Errors)
    ; StmtType = s_return(_),
        ExprsWithBang = 0
    ; StmtType = s_match(_, Cases),
        CasesErrors = map(check_bangs_case, Cases),
        ExprsWithBang = 0,
        add_errors(cord_list_to_cord(CasesErrors), !Errors)
    ),
    ( if ExprsWithBang > 1 then
        add_error(Context, ce_too_many_bangs_in_statement, !Errors)
    else
        true
    ).

    % This code has been removed because it cannot check higher order code
    % from this pass.  And later pases do not carry statement information.
    % It should be re-implemented in the future.
    % ( if
    %     all [U] ( member(U, UsedSet) =>
    %         (
    %             count_value(Used, U, 1),
    %             \+ member(U, ObservedSet),
    %             URes = core_get_resource(Info ^ cri_core, U),
    %             all [P] ( member(P, UsedSet `union` ObservedSet) =>
    %                 \+ (U \= P, resource_is_decendant(Info ^ cri_core, URes, P))
    %             )
    %         )
    %     ),
    %     all [O] ( member(O, ObservedSet) =>
    %         (
    %             \+ member(O, UsedSet),
    %             ORes = core_get_resource(Info ^ cri_core, O),
    %             all [PP] ( member(PP, UsedSet) =>
    %                 \+ resource_is_decendant(Info ^ cri_core, ORes, PP)
    %             )
    %         )
    %     )
    % then
    %     true
    % else
    %     add_error(Context, ce_resource_reused_in_stmt, !Errors)
    % ).

:- func check_bangs_case(pre_case) = errors(compile_error).

check_bangs_case(pre_case(_, Stmts)) =
    cord_list_to_cord(map(check_bangs_stmt, Stmts)).

:- pred check_bangs_expr(context::in, pre_expr::in,
    int::out, errors(compile_error)::out) is det.

check_bangs_expr(Context, e_call(Call), ExprsWithBang, Errors) :-
    check_bangs_call(Context, Call, ExprsWithBang, Errors).
check_bangs_expr(Context, e_match(Expr, Cases), Bangs, Errors) :-
    check_bangs_expr(Context, Expr, BangsInExpr, ExprErrors),
    map2(check_bangs_expr_case(Context), Cases, BangsInCases, CasesErrors),
    Bangs = sum(BangsInCases) + BangsInExpr,
    Errors = ExprErrors ++ cord_list_to_cord(CasesErrors).
check_bangs_expr(_, e_var(_), 0, init).
check_bangs_expr(Context, e_construction(_, Exprs), Bangs, Errors) :-
    map2(check_bangs_expr(Context), Exprs, BangsInExprs, Errors0),
    Bangs = sum(BangsInExprs),
    Errors = cord_list_to_cord(Errors0).
check_bangs_expr(_, e_lambda(Lambda), 0, Errors) :-
    Body = Lambda ^ pl_body,
    Errors = cord_list_to_cord(map(check_bangs_stmt, Body)).
check_bangs_expr(_, e_constant(_), 0, init).

:- pred check_bangs_call(context::in, pre_call::in, int::out,
    errors(compile_error)::out) is det.

check_bangs_call(Context, Call, ExprsWithBang, !:Errors) :-
    !:Errors = init,
    ( Call = pre_call(_, Args, WithBang)
    ; Call = pre_ho_call(_, Args, WithBang)
    ),
    map2(check_bangs_expr(Context), Args, BangsInArgs0, ArgsErrors),
    BangsInArgs = sum(BangsInArgs0),
    add_errors(cord_list_to_cord(ArgsErrors), !Errors),
    ( WithBang = with_bang,
        ExprsWithBang = BangsInArgs + 1
    ; WithBang = without_bang,
        ExprsWithBang = BangsInArgs
    ).

:- pred check_bangs_expr_case(context::in, pre_expr_case::in, int::out,
    errors(compile_error)::out) is det.

check_bangs_expr_case(Context, pre_e_case(_, Expr), Bangs, Errors) :-
    map2(check_bangs_expr(Context), Expr, Bangss, Errorss),
    Bangs = sum(Bangss),
    Errors = cord_list_to_cord(Errorss).

%-----------------------------------------------------------------------%

:- func sum(list(int)) = int.

sum(Xs) = foldl(func(A, B) = A + B, Xs, 0).

