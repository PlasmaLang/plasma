%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module core.res_chk.
%
% Copyright (C) 2018 Plasma Team
% Distributed under the terms of the MIT see ../LICENSE.code
%
% Plasma resource checking - post typechecking
%
% We do some resource checking before type checking, but need to repeat it
% here for higher-order code.
%
%-----------------------------------------------------------------------%
:- interface.

:- import_module compile_error.
:- import_module result.

:- pred res_check(errors(compile_error)::out, core::in, core::out) is det.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
:- implementation.

:- import_module cord.

:- import_module core.util.

%-----------------------------------------------------------------------%

res_check(Errors, !Core) :-
    check_noerror_funcs(res_check_func, Errors, !Core).

:- func res_check_func(core, func_id, function) = errors(compile_error).

res_check_func(Core, _FuncId, Func) = Errors :-
    func_get_resource_signature(Func, Using, Observing),
    ( if
        func_get_body(Func, _Varmap, _Params, ExprP),
        func_get_vartypes(Func, VarTypesP)
    then
        Expr = ExprP,
        VarTypes = VarTypesP
    else
        unexpected($file, $pred, "Couldn't lookup function body or types")
    ),
    Info = check_res_info(Core, Using, Observing, VarTypes),
    Errors = res_check_expr(Info, Expr).

:- type check_res_info
    --->    check_res_info(
                cri_core        :: core,
                cri_using       :: set(resource_id),
                cri_observing   :: set(resource_id),
                cri_vartypes    :: map(var, type_)
            ).

:- func res_check_expr(check_res_info, expr) = errors(compile_error).

res_check_expr(Info, expr(ExprType, CodeInfo)) = Errors :-
    ( ExprType = e_tuple(Exprs),
        Errors = cord_list_to_cord(map(res_check_expr(Info), Exprs))
    ; ExprType = e_let(_, LetExpr, InExpr),
        Errors = res_check_expr(Info, LetExpr) ++
            res_check_expr(Info, InExpr)
    ;
        ( ExprType = e_var(_)
        ; ExprType = e_constant(_)
        ; ExprType = e_construction(_, _)
        ),
        Errors = cord.init
    ; ExprType = e_match(_, Cases),
        Errors = cord_list_to_cord(map(
            (func(e_case(_, E)) = res_check_expr(Info, E)),
            Cases))
    ; ExprType = e_call(Callee, InputArgs, Resources),
        % TODO: Information in function results needs to be handled in the
        % type checker.  And Args mustn't be handled there.

        some [!Errors] (
            !:Errors = init,
            ( Resources = unknown_resources,
                unexpected($file, $pred, "Missing resource usage information")
            ; Resources = resources(Using, Observing),
                add_errors(res_check_call(Info, CodeInfo, Using, Observing),
                    !Errors)
            ),

            ( Callee = c_plain(FuncId),
                core_get_function_det(Info ^ cri_core, FuncId, Func),
                func_get_type_signature(Func, InputParamTypes, _, _)
            ; Callee = c_ho(Var),
                lookup(Info ^ cri_vartypes, Var, Type),
                ( if Type = func_type(InputParamTypesP, _, _, _) then
                    InputParamTypes = InputParamTypesP
                else
                    unexpected($file, $pred, "Not a funciton type")
                )
            ),
            add_errors(cord_list_to_cord(
                map_corresponding(check_resource_in_arg(Info, CodeInfo), InputParamTypes,
                    InputArgs)),
                !Errors),

            Errors = !.Errors
        )
    ).

:- func res_check_call(check_res_info, code_info,
    set(resource_id), set(resource_id)) = errors(compile_error).

res_check_call(Info, CodeInfo, CalleeUsing, CalleeObserving) = !:Errors :-
    !:Errors = init,
    FuncUsing = Info ^ cri_using,
    FuncObserving = Info ^ cri_observing,
    Core = Info ^ cri_core,
    Bang = code_info_bang_marker(CodeInfo),
    Context = code_info_get_context(CodeInfo),
    ( if
        all_resources_in_parent(Core, CalleeUsing, FuncUsing),
        all_resources_in_parent(Core, CalleeObserving,
            FuncUsing `union` FuncObserving)
    then
        true
    else
        add_error(Context, ce_resource_unavailable_call, !Errors)
    ),
    ( if empty(CalleeUsing `union` CalleeObserving) then
        ( Bang = has_bang_marker,
            add_error(Context, ce_unnecessary_bang, !Errors)
        ; Bang = no_bang_marker
        )
    else
        ( Bang = has_bang_marker
        ; Bang = no_bang_marker,
            add_error(Context, ce_no_bang, !Errors)
        )
    ).

:- func check_resource_in_arg(check_res_info, code_info, type_, var) =
    errors(compile_error).

check_resource_in_arg(Info, CodeInfo, ParamType, Arg) = !:Errors :-
    !:Errors = init,
    (
        ( ParamType = builtin_type(_)
        ; ParamType = type_variable(_)
        ; ParamType = type_ref(_, _)
        )
    ; ParamType = func_type(_, _, ParamUses, ParamObserves),
        lookup(Info ^ cri_vartypes, Arg, ArgType),
        Core = Info ^ cri_core,
        ( if ArgType = func_type(_, _, ArgUses, ArgObserves) then
            ( if
                all_resources_in_parent(Core, ArgUses, ParamUses),
                all_resources_in_parent(Core, ArgObserves,
                    ParamUses `union` ParamObserves)
            then
                true
            else
                Context = code_info_get_context(CodeInfo),
                add_error(Context, ce_resource_unavailable_arg, !Errors)
            )
        else
            unexpected($file, $pred, "Function type expected")
        )
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

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
