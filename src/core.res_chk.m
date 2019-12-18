%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module core.res_chk.
%
% Copyright (C) 2018-2019 Plasma Team
% Distributed under the terms of the MIT see ../LICENSE.code
%
% Plasma resource checking - post typechecking
%
% We do some resource checking before type checking, but need to repeat it
% here for higher-order code.  Resource checking needs to interact with the
% typechecker to pass resource usage attributes on higher order functions
% around through a function definition.  The type checker and this module
% have to coordinate carefully.  Here's how it works.
%
% Resource are provided by some pieces of code (callers) and required by
% others (callees).  These are:
%
% Required by:
%  + Callees at call sites of their environment,
%  + Function constructors of the new value,
%  + Returns from calls returning functions (out arguments),
%  + Parameters of function definitions
%
% Provided by:
%  + Environment at a call site,
%  + Arguments in function calls,
%  + Return parameters of function definitions
%
% Resource use attributes are "passed" around by constructions,
% deconstructions, assignments and so on.
%
% The type checker introduces resource use attributes into the type system
% only at those places resources are required, at other places it ignores
% them.  Then the solver propagates that information around.  Therefore
% within the type system resource annotations mean "this is the set of
% resources that this function may need", and not "might have access to".
% Then this module checks the locations where resources are provided,
% ensuring that all the required resources (on the type annotation) are
% provided by the environment or anther type.
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
        func_get_body(Func, _Varmap, _Params, _Captured, ExprP),
        func_get_vartypes(Func, VarTypesP)
    then
        Expr = ExprP,
        VarTypes = VarTypesP
    else
        unexpected($file, $pred, "Couldn't lookup function body or types")
    ),
    Info = check_res_info(Core, Using, Observing, VarTypes),
    ExprErrors = res_check_expr(Info, Expr),

    func_get_type_signature(Func, _, OutputTypes, _),
    ExprTypes = code_info_get_types(Expr ^ e_info),
    Context = func_get_context(Func),
    OutputErrors = foldl((func(MbE, Es) = maybe_cord(MbE) ++ Es),
            map_corresponding(check_output_res(Core, Context),
                OutputTypes, ExprTypes),
            init),

    Errors = ExprErrors ++ OutputErrors.

:- func check_output_res(core, context, type_, type_) =
    maybe(error(compile_error)).

check_output_res(Core, Context, TypeRequire, TypeProvide) = MaybeError :-
    ( if
        TypeRequire = func_type(_, _, UsesRequire, ObservesRequire),
        TypeProvide = func_type(_, _, UsesProvide, ObservesProvide)
    then
        ( if
            all_resources_in_parent(Core, UsesRequire, UsesProvide),
            all_resources_in_parent(Core, ObservesRequire,
                ObservesProvide `union` UsesProvide)
        then
            MaybeError = no
        else
            MaybeError = yes(error(Context, ce_resource_unavailable_output))
        )
    else
        % Don't do any stricter tests, the type checker will have done
        % them.
        MaybeError = no
    ).

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
    ; ExprType = e_lets(Lets, InExpr),
        Errors = cord_list_to_cord(list.map(
                func(e_let(_, E)) = res_check_expr(Info, E), Lets)) ++
            res_check_expr(Info, InExpr)
    ;
        ( ExprType = e_var(_)
        ; ExprType = e_construction(_, _)
        ; ExprType = e_constant(_)
        ; ExprType = e_closure(_, _)
        ),
        Errors = cord.init
    ; ExprType = e_match(_, Cases),
        Errors = cord_list_to_cord(map(
            (func(e_case(_, E)) = res_check_expr(Info, E)),
            Cases))
    ; ExprType = e_call(Callee, Args, Resources),
        % Check that the call has all the correct resources available for
        % this callee.
        ( Resources = unknown_resources,
            unexpected($file, $pred, "Missing resource usage information")
        ; Resources = resources(Using, Observing),
            CallErrors = res_check_call(Info, CodeInfo, Using, Observing)
        ),

        ( Callee = c_plain(FuncId),
            core_get_function_det(Info ^ cri_core, FuncId, Func),
            func_get_type_signature(Func, InputParams, _, _)
        ; Callee = c_ho(HOVar),
            HOType = lookup(Info ^ cri_vartypes, HOVar),
            ( if HOType = func_type(InputParamsP, _, _, _) then
                InputParams = InputParamsP
            else
                unexpected($file, $pred, "Call to non-function")
            )
        ),
        Context = code_info_get_context(CodeInfo),
        ArgsErrors = cord_list_to_cord(map_corresponding(
            res_check_call_arg(Info, Context), InputParams, Args)),

        Errors = CallErrors ++ ArgsErrors
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

:- func res_check_call_arg(check_res_info, context, type_, var) =
    errors(compile_error).

res_check_call_arg(Info, Context, Param, ArgVar) =
        res_check_call_arg_types(Info ^ cri_core, Context, Param, Arg) :-
    Arg = lookup(Info ^ cri_vartypes, ArgVar).

:- func res_check_call_arg_types(core, context, type_, type_) =
    errors(compile_error).

res_check_call_arg_types(_, _, builtin_type(_), _) = init.
res_check_call_arg_types(Core, Context, func_type(_ParamInputs, _ParamOutputs,
        ParamUses, ParamObserves), Arg) = Errors :-
    ( if Arg = func_type(ArgInputs, ArgOutputs, ArgUses, ArgObserves) then
        ( if
            all_resources_in_parent(Core, ArgUses, ParamUses),
            all_resources_in_parent(Core, ArgObserves,
                ParamUses `union` ParamObserves)
        then
            FuncErrors = init
        else
            FuncErrors = error(Context, ce_resource_unavailable_arg)
        ),

        % TODO: Need to figure this out later.
        ( if
            ( member(Type, ArgInputs)
            ; member(Type, ArgOutputs)
            ) =>
            is_or_has_function_type(Type)
        then
            util.sorry($file, $pred, "Nested function types")
        else
            true
        ),

        Errors = FuncErrors
    else
        unexpected($file, $pred, "Types don't match")
    ).
res_check_call_arg_types(_, _, type_variable(_), _) = init.
res_check_call_arg_types(Core, Context, type_ref(_, Params), Arg) = Errors :-
    ( if Arg = type_ref(_, Args) then
        Errors = cord_list_to_cord(map_corresponding(
            res_check_call_arg_types(Core, Context), Params, Args))
    else
        unexpected($file, $pred, "Types don't match")
    ).

:- pred is_or_has_function_type(type_::in) is semidet.

is_or_has_function_type(func_type(_, _, _, _)).
is_or_has_function_type(type_ref(_, Args)) :-
    member(Arg, Args) =>
    is_or_has_function_type(Arg).

%-----------------------------------------------------------------------%

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
