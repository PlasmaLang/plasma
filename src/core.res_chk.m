%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module core.res_chk.
%
% Copyright (C) 2018-2020 Plasma Team
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

:- import_module io.

:- import_module compile_error.
:- import_module util.log.
:- import_module util.result.

:- pred res_check(log_config::in, errors(compile_error)::out,
    core::in, core::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
:- implementation.

:- import_module cord.
:- import_module require.

:- import_module context.
:- import_module core.util.
:- import_module util.mercury.

%-----------------------------------------------------------------------%

res_check(Verbose, Errors, !Core, !IO) :-
    process_noerror_funcs(Verbose, res_check_func, Errors, !Core, !IO).

:- pred res_check_func(core::in, func_id::in, function::in,
    result_partial(function, compile_error)::out) is det.

res_check_func(Core, _FuncId, Func0, Result) :-
    func_get_resource_signature(Func0, Using, Observing),
    ( if
        func_get_body(Func0, Varmap, Params, Captured, Expr0),
        func_get_vartypes(Func0, VarTypes)
    then
        Info = check_res_info(Core, Using, Observing, VarTypes),
        res_check_expr(Info, Expr0, Expr, ExprResult),
        func_set_body(Varmap, Params, Captured, Expr, VarTypes, Func0, Func)
    else
        unexpected($file, $pred, "Couldn't lookup function body or types")
    ),

    func_get_type_signature(Func, _, OutputTypes, _),
    ExprTypes = code_info_types(Expr ^ e_info),
    Context = func_get_context(Func),
    OutputErrors = foldl((func(MbE, Es) = maybe_cord(MbE) ++ Es),
            map_corresponding(check_output_res(Core, Context),
                OutputTypes, ExprTypes),
            init),

    ( ExprResult = ok(_),
        ( if not has_fatal_errors(OutputErrors) then
            Result = ok(Func, OutputErrors)
        else
            Result = errors(OutputErrors)
        )
    ; ExprResult = errors(ExprErrors),
        Result = errors(ExprErrors ++ OutputErrors)
    ).

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

:- pred res_check_expr(check_res_info::in, expr::in, expr::out,
    result(bang_marker, compile_error)::out) is det.

res_check_expr(Info, Expr0, Expr, Result) :-
    Expr0 = expr(ExprType0, CodeInfo0),
    (
        ( ExprType0 = e_var(_)
        ; ExprType0 = e_construction(_, _)
        ; ExprType0 = e_constant(_)
        ; ExprType0 = e_closure(_, _)
        ),
        Result = ok(no_bang_marker),
        Expr = Expr0
    ;
        ( ExprType0 = e_tuple(Exprs0),
            map2(res_check_expr(Info), Exprs0, Exprs, Results0),
            ExprType = e_tuple(Exprs)
        ; ExprType0 = e_lets(Lets0, InExpr0),
            map2(res_check_let(Info), Lets0, Lets, LetsResults),
            res_check_expr(Info, InExpr0, InExpr, InResult),
            Results0 = [InResult | LetsResults],
            ExprType = e_lets(Lets, InExpr)
        ; ExprType0 = e_match(Var, Cases0),
            map2(res_check_case(Info), Cases0, Cases, Results0),
            ExprType = e_match(Var, Cases)
        ),

        % On these nodes we must propagate the bang information from inner
        % expressions to outer ones.
        Results = result_list_to_result(Results0),
        ( Results = ok(InnerBangs),
            ( if any_true(unify(has_bang_marker), InnerBangs) then
                code_info_set_bang_marker(has_bang_marker, CodeInfo0,
                    CodeInfo),
                Result = ok(has_bang_marker)
            else
                CodeInfo = CodeInfo0,
                Result = ok(no_bang_marker)
            )
        ; Results = errors(InnerErrors),
            Result = errors(InnerErrors),
            CodeInfo = CodeInfo0
        ),
        Expr = expr(ExprType, CodeInfo)
    ; ExprType0 = e_call(Callee, Args, Resources),
        Expr = Expr0,
        % Check that the call has all the correct resources available for
        % this callee.
        ( Resources = unknown_resources,
            unexpected($file, $pred, "Missing resource usage information")
        ; Resources = resources(Using, Observing),
            CallResult = res_check_call(Info, CodeInfo0, Using, Observing)
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
        Context = code_info_context(CodeInfo0),
        ArgsErrors = cord_list_to_cord(map_corresponding(
            res_check_call_arg(Info, Context), InputParams, Args)),
        ( if is_empty(ArgsErrors) then
            Result = CallResult
        else
            ( CallResult = ok(_),
                Result = errors(ArgsErrors)
            ; CallResult = errors(Errors),
                Result = errors(Errors ++ ArgsErrors)
            )
        )
    ).

:- pred res_check_let(check_res_info::in, expr_let::in, expr_let::out,
    result(bang_marker, compile_error)::out) is det.

res_check_let(Info, e_let(Var, Expr0), e_let(Var, Expr), Result) :-
    res_check_expr(Info, Expr0, Expr, Result).

:- pred res_check_case(check_res_info::in, expr_case::in, expr_case::out,
    result(bang_marker, compile_error)::out) is det.

res_check_case(Info, e_case(Pat, Expr0), e_case(Pat, Expr), Result) :-
    res_check_expr(Info, Expr0, Expr, Result).

:- func res_check_call(check_res_info, code_info,
    set(resource_id), set(resource_id)) = result(bang_marker, compile_error).

res_check_call(Info, CodeInfo, CalleeUsing, CalleeObserving) = Result :-
    some [!Errors] (
        !:Errors = init,
        FuncUsing = Info ^ cri_using,
        FuncObserving = Info ^ cri_observing,
        Core = Info ^ cri_core,
        Bang = code_info_bang_marker(CodeInfo),
        Context = code_info_context(CodeInfo),
        ( if
            all_resources_in_parent(Core, CalleeUsing, FuncUsing),
            all_resources_in_parent(Core, CalleeObserving,
                FuncUsing `union` FuncObserving)
        then
            true
        else
            add_error(Context, ce_resource_unavailable_call, !Errors)
        ),
        ( if is_empty(CalleeUsing `union` CalleeObserving) then
            ( Bang = has_bang_marker,
                add_error(Context, ce_unnecessary_bang, !Errors)
            ; Bang = no_bang_marker
            )
        else
            ( Bang = has_bang_marker
            ; Bang = no_bang_marker,
                add_error(Context, ce_no_bang, !Errors)
            )
        ),
        ( if is_empty(!.Errors) then
            Result = ok(Bang)
        else
            Result = errors(!.Errors)
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
            util.exception.sorry($file, $pred, Context, "Nested function types")
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
        is_non_empty(FuncRes),
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
