%-----------------------------------------------------------------------%
% Plasma AST symbol resolution
% vim: ts=4 sw=4 et
%
% Copyright (C) 2017 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
% This module fixes variable usage in branching code.  It:
%   * fixes var-def sets
%   * Determines some reachability information (WRT return statements).
%   * checks that used variables are always well defined (eg
%     along all execution paths)
%   * names-appart branch-local variables (from other
%     branches).
%
%-----------------------------------------------------------------------%
:- module pre.resources.
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

:- import_module cord.

:- import_module common_types.
:- import_module core.function.
:- import_module core.resource.

%-----------------------------------------------------------------------%

check_resources(Core, Proc) =
        cord_list_to_cord(map(check_res_stmt(Info), Stmts)) :-
    Proc = pre_procedure(FuncId, _Varmap, _Vars, Stmts),
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

check_res_stmt(Info, Stmt) = Errors :-
    StmtType = Stmt ^ s_type,
    Context = Stmt ^ s_info ^ si_context,
    ( StmtType = s_call(Call),
        Errors = check_res_call(Info, Context, Call)
    ; StmtType = s_assign(_, Expr),
        Errors = check_res_expr(Info, Context, Expr)
    ; StmtType = s_return(_),
        Errors = init
    ; StmtType = s_match(_, Cases),
        CasesErrors = map(check_res_case(Info), Cases),
        Errors = cord_list_to_cord(CasesErrors)
    ).

:- func check_res_case(check_res_info, pre_case) =
    errors(compile_error).

check_res_case(Info, pre_case(_, Stmts)) =
    cord_list_to_cord(map(check_res_stmt(Info), Stmts)).

:- func check_res_expr(check_res_info, context, pre_expr) =
    errors(compile_error).

check_res_expr(Info, Context, e_call(Call)) =
    check_res_call(Info, Context, Call).
check_res_expr(_, _, e_var(_)) = init.
check_res_expr(_, _, e_construction(_, _)) = init.
check_res_expr(_, _, e_constant(_)) = init.

:- func check_res_call(check_res_info, context, pre_call) =
    errors(compile_error).

check_res_call(Info, Context, pre_call(CalleeId, _, WithBang)) = !:Errors :-
    Core = Info ^ cri_core,
    core_get_function_det(Core, CalleeId, Callee),
    func_get_resource_signature(Callee, CalleeUsing, CalleeObserving),
    FuncUsing = Info ^ cri_using,
    FuncObserving = Info ^ cri_observing,
    !:Errors = init,
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

