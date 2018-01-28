%-----------------------------------------------------------------------%
% Plasma AST symbol resolution
% vim: ts=4 sw=4 et
%
% Copyright (C) 2017-2018 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
%-----------------------------------------------------------------------%
:- module pre.resource.
%-----------------------------------------------------------------------%

:- interface.

:- import_module compile_error.
:- import_module core.
:- import_module pre.pre_ds.
:- import_module result.

%-----------------------------------------------------------------------%

:- func check_resources(core, pre_procedure) = errors(compile_error).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

:- import_module bag.
:- import_module cord.
:- import_module int.

:- import_module common_types.
:- import_module core.function.
:- import_module core.resource.
:- import_module util.

%-----------------------------------------------------------------------%

% TODO: Most of the resource checking will have to move to a stage after
% type checking, particularly for higher order calls & resources bound to
% values.  Other resource checking will have to stay here such as errors for
% use of multiple resources in the one statement.

check_resources(Core, Proc) =
        cord_list_to_cord(map(check_res_stmt(Info), Stmts)) :-
    FuncId = Proc ^ p_func_id,
    Stmts = Proc ^ p_body,
    core_get_function_det(Core, FuncId, Func),
    func_get_resource_signature(Func, Using, Observing),
    Info = check_res_info(Core, Using, Observing).

:- type check_res_info
    --->    check_res_info(
                cri_core        :: core,
                cri_using       :: set(resource_id),
                cri_observing   :: set(resource_id)
            ).

% TODO: We will need to add information to compund statements to advise
% whether they use or observe some resources so that later optimisation
% know if they can be reordered.

:- func check_res_stmt(check_res_info, pre_statement) =
    errors(compile_error).

check_res_stmt(Info, Stmt) = !:Errors :-
    !:Errors = init,
    StmtType = Stmt ^ s_type,
    Context = Stmt ^ s_info ^ si_context,
    ( StmtType = s_call(Call),
        check_res_call(Info, Context, Call, ExprsWithBang, StmtErrors),
        add_errors(StmtErrors, !Errors)
    ; StmtType = s_assign(_, Expr),
        check_res_expr(Info, Context, Expr, ExprsWithBang, StmtErrors),
        add_errors(StmtErrors, !Errors)
    ; StmtType = s_return(_),
        ExprsWithBang = 0
    ; StmtType = s_match(_, Cases),
        CasesErrors = map(check_res_case(Info), Cases),
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

:- func check_res_case(check_res_info, pre_case) =
    errors(compile_error).

check_res_case(Info, pre_case(_, Stmts)) =
    cord_list_to_cord(map(check_res_stmt(Info), Stmts)).

:- pred check_res_expr(check_res_info::in, context::in, pre_expr::in,
    int::out, errors(compile_error)::out) is det.

check_res_expr(Info, Context, e_call(Call), ExprsWithBang, Errors) :-
    check_res_call(Info, Context, Call, ExprsWithBang, Errors).
check_res_expr(_, _, e_var(_), 0, init).
check_res_expr(_, _, e_construction(_, _), 0, init).
check_res_expr(_, _, e_constant(_), 0, init).

:- pred check_res_call(check_res_info::in, context::in, pre_call::in,
    int::out, errors(compile_error)::out) is det.

check_res_call(Info, Context, Call, ExprsWithBang, !:Errors) :-
    !:Errors = init,
    Core = Info ^ cri_core,

    ( Call = pre_call(_, Args, WithBang)
    ; Call = pre_ho_call(_, Args, WithBang)
    ),
    map2(check_res_expr(Info, Context), Args, BangsInArgs0, ArgsErrors),
    BangsInArgs = foldl(func(A, B) = A + B, BangsInArgs0, 0),
    add_errors(cord_list_to_cord(ArgsErrors), !Errors),
    ( WithBang = with_bang,
        ExprsWithBang = BangsInArgs + 1
    ; WithBang = without_bang,
        ExprsWithBang = BangsInArgs
    ),

    ( Call = pre_call(CalleeId, _, _),
        core_get_function_det(Core, CalleeId, Callee),
        func_get_resource_signature(Callee, CalleeUsing, CalleeObserving)
    ; Call = pre_ho_call(_, _, _),
        % XXX
        CalleeUsing = init,
        CalleeObserving = init
    ),

    FuncUsing = Info ^ cri_using,
    FuncObserving = Info ^ cri_observing,
    ( if
        all_resources_in_parent(Core, CalleeUsing, FuncUsing),
        all_resources_in_parent(Core, CalleeObserving,
            FuncUsing `union` FuncObserving)
    then
        true
    else
        add_error(Context, ce_resource_unavailable, !Errors)
    ),
    ( if
        non_empty(CalleeUsing `union` CalleeObserving) =>
        WithBang = with_bang
    then
        true
    else
        add_error(Context, ce_no_bang, !Errors)
    ).

:- pred all_resources_in_parent(core::in, set(resource_id)::in,
    set(resource_id)::in) is semidet.

all_resources_in_parent(Core, CalleeRes, FuncRes) :-
    all [C] ( member(C, CalleeRes) => (
        non_empty(FuncRes),
        ( member(C, FuncRes)
        ;
            CR = core_get_resource(Core, C),
            some [F] ( member(F, FuncRes) =>
                resource_is_decendant(Core, CR, F)
            )
        ))
    ).

