%-----------------------------------------------------------------------%
% Plasma typechecking
% vim: ts=4 sw=4 et
%
% Copyright (C) 2016-2017 Plasma Team
% Distributed under the terms of the MIT see ../LICENSE.code
%
% This module typechecks plasma core using a solver over Prolog-like terms.
% Solver variables and constraints are created as follows.
%
% Consider an expression which performs a list cons:
%
% cons(elem, list)
%
% cons is declared as func(t, List(t)) -> List(t)
%
% + Because we use an ANF-like representation associating a type with each
%   variable is almost sufficient, we also associate types with calls.  Each
%   type is represented by a solver variable.  In this example these are:
%   elem, list and the call to cons.  Each of these can have constraints
%   thate describe any type information we already know:
%   elem       = int
%   list       = T0
%   call(cons) = func(T1, list(T1)) -> list(T1) % based on declaration
%   T1         = int % from function application
%   list(T1)   = T0
%
%   We assume that cons's type is fixed and will not be inferred by this
%   invocation of the solver.  Other cases are handled seperately.
%
%   The new type variable, and therefore solver variable, T1, is introduced.
%   T0 is also introduced to stand in for the type of the list.
%
% + The solver can combine these rules, unifing them and finding the unique
%   solution.  Type variables that appear in the signature of the function are
%   allowed to be part of the solution, others are not as that would mean it
%   is ambigiously typed.
%
% Other type variables and constraints are.
%
% + The parameters and return values of the current function.  Including
%   treatment of any type variables.
%
% Propagation is probably the only step required to find the correct types.
% However labeling (search) can also occur.  Type variables in the signature
% must be handled specially, they must not be labeled during search and may
% require extra rules (WIP).
%
%-----------------------------------------------------------------------%
:- module core.typecheck.
%-----------------------------------------------------------------------%

:- interface.

:- import_module compile_error.
:- import_module result.

:- pred typecheck(errors(compile_error)::out, core::in, core::out) is det.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
:- implementation.

:- import_module counter.
:- import_module cord.
:- import_module io.
:- import_module map.
:- import_module string.

:- import_module core.util.
:- import_module pretty_utils.
:- import_module util.

:- include_module core.typecheck.solve.
:- import_module core.typecheck.solve.

%-----------------------------------------------------------------------%

typecheck(Errors, !Core) :-
    % TODO: Add support for inference, which must be bottom up by SCC.
    process_noerror_funcs(typecheck_func, Errors, !Core).

:- pred typecheck_func(core::in, func_id::in,
    function::in, result(function, compile_error)::out) is det.

typecheck_func(Core, FuncId, Func0, Result) :-
    compute_arity_func(Core, Func0, ArityResult),
    ( ArityResult = ok(Func1),
        % Now do the real typechecking.
        build_cp_func(Core, FuncId, init, Constraints),
        ( if func_get_varmap(Func1, VarmapPrime) then
            Varmap = VarmapPrime
        else
            unexpected($file, $pred, "Couldn't retrive varmap")
        ),
        solve(solver_var_pretty(Varmap), Constraints, Mapping),
        update_types_func(Core, Mapping, Func1, Result)
    ; ArityResult = errors(ArityErrors),
        Result = errors(ArityErrors)
    ).

%-----------------------------------------------------------------------%

:- pred compute_arity_func(core::in, function::in,
    result(function, compile_error)::out) is det.

compute_arity_func(Core, Func0, Result) :-
    func_get_type_signature(Func0, _, _, DeclaredArity),
    ( if func_get_body(Func0, Varmap, Args, Expr0) then
        compute_arity_expr(Core, Expr0, Expr, ArityResult),
        ( ArityResult = ok(Arity),
            ( if Arity = DeclaredArity then
                func_set_body(Varmap, Args, Expr, Func0, Func),
                Result = ok(Func)
            else
                Result = return_error(func_get_context(Func0),
                    ce_arity_mismatch_func(DeclaredArity, Arity))
            )
        ; ArityResult = errors(Errors),
            Result = errors(Errors)
        )
    else
        unexpected($file, $pred, "Imported function")
    ).

:- pred compute_arity_expr(core::in, expr::in, expr::out,
    result(arity, compile_error)::out) is det.

compute_arity_expr(Core, expr(ExprType0, CodeInfo0), expr(ExprType, CodeInfo),
        Result) :-
    ( ExprType0 = e_tuple(Exprs0),
        compute_arity_expr_tuple(Core, Exprs0, Exprs, CodeInfo0, CodeInfo,
            Result),
        ExprType = e_tuple(Exprs)
    ; ExprType0 = e_let(Vars, ExprLet0, ExprIn0),
        compute_arity_expr_let(Core, Vars, Result, ExprLet0, ExprLet,
            ExprIn0, ExprIn, CodeInfo0, CodeInfo),
        ExprType = e_let(Vars, ExprLet, ExprIn)
    ; ExprType0 = e_call(Callee, Args),
        ExprType = e_call(Callee, Args),
        compute_arity_expr_call(Core, Callee, Args, CodeInfo0, CodeInfo,
            Result)
    ; ExprType0 = e_match(Var, Cases0),
        compute_arity_expr_match(Core, Cases0, Cases, CodeInfo0, CodeInfo,
            Result),
        ExprType = e_match(Var, Cases)
    ;
        ( ExprType0 = e_var(_)
        ; ExprType0 = e_constant(_)
        ; ExprType0 = e_construction(_, _)
        ),
        Arity = arity(1),
        code_info_set_arity(Arity, CodeInfo0, CodeInfo),
        ExprType = ExprType0,
        Result = ok(Arity)
    ).

:- pred compute_arity_expr_tuple(core::in, list(expr)::in, list(expr)::out,
    code_info::in, code_info::out, result(arity, compile_error)::out) is det.

compute_arity_expr_tuple(Core, !Exprs, !CodeInfo, Result) :-
    Context = code_info_get_context(!.CodeInfo),
    map2(compute_arity_expr(Core), !Exprs, ListResults),
    TupleResult = result_list_to_result(ListResults),
    Arity = arity(length(!.Exprs)),
    code_info_set_arity(Arity, !CodeInfo),
    ( TupleResult = ok(TupleAritys),
        ( if all [TArity] member(TArity, TupleAritys) =>
            TArity = arity(1)
        then
            Result = ok(Arity)
        else
            Result = return_error(Context, ce_arity_mismatch_tuple)
        )
    ; TupleResult = errors(Errors),
        Result = errors(Errors)
    ).

:- pred compute_arity_expr_let(core::in, list(T)::in,
    result(arity, compile_error)::out, expr::in, expr::out,
    expr::in, expr::out, code_info::in, code_info::out) is det.

compute_arity_expr_let(Core, Vars, Result, !ExprLet,
        !ExprIn, !CodeInfo) :-
    compute_arity_expr(Core, !ExprLet, LetRes),
    ( LetRes = ok(LetArity),
        Arity = arity(length(Vars)),
        ( if Arity = LetArity then
            compute_arity_expr(Core, !ExprIn, InRes),
            ( InRes = ok(ExprArity),
                code_info_set_arity(ExprArity, !CodeInfo),
                Result = ok(ExprArity)
            ; InRes = errors(Errors),
                Result = errors(Errors)
            )
        else
            Result = return_error(code_info_get_context(!.CodeInfo),
                ce_arity_mismatch_expr(LetArity, Arity))
        )
    ; LetRes = errors(Errors),
        Result = errors(Errors)
    ).

:- pred compute_arity_expr_call(core::in, func_id::in, list(T)::in,
    code_info::in, code_info::out, result(arity, compile_error)::out) is det.

compute_arity_expr_call(Core, FuncId, Args, !CodeInfo, Result) :-
    core_get_function_det(Core, FuncId, CalleeFn),
    func_get_type_signature(CalleeFn, Inputs, _, Arity),
    length(Inputs, InputsLen),
    length(Args, ArgsLen),
    ( if InputsLen = ArgsLen then
        InputErrors = init
    else
        InputErrors = error(code_info_get_context(!.CodeInfo),
            ce_parameter_number(length(Inputs), length(Args)))
    ),
    code_info_set_arity(Arity, !CodeInfo),
    ( if is_empty(InputErrors) then
        Result = ok(Arity)
    else
        Result = errors(InputErrors)
    ).

:- pred compute_arity_expr_match(core::in, list(expr_case)::in,
    list(expr_case)::out, code_info::in, code_info::out,
    result(arity, compile_error)::out) is det.

compute_arity_expr_match(Core, !Cases, !CodeInfo, Result) :-
    Context = code_info_get_context(!.CodeInfo),
    map2(compute_arity_case(Core), !Cases, CaseResults),
    Result0 = result_list_to_result(CaseResults),
    ( Result0 = ok(CaseArities),
        (
            CaseArities = [],
            Result = return_error(Context, ce_match_has_no_cases)
        ;
            CaseArities = [Arity | _],
            ( if all_same(CaseArities) then
                code_info_set_arity(Arity, !CodeInfo),
                Result = ok(Arity)
            else
                Result = return_error(Context,
                    ce_arity_mismatch_match(CaseArities))
            )
        )
    ; Result0 = errors(Errors),
        Result = errors(Errors)
    ).

:- pred compute_arity_case(core::in, expr_case::in, expr_case::out,
    result(arity, compile_error)::out) is det.

compute_arity_case(Core, e_case(Pat, Expr0), e_case(Pat, Expr), Result) :-
    compute_arity_expr(Core, Expr0, Expr, Result).

%-----------------------------------------------------------------------%

    % Solver variable.
:- type solver_var
            % The type of an expression.
    --->    sv_var(
                svv_var             :: var
            )

            % The type of an output value.
    ;       sv_output(
                svo_result_num      :: int
            ).

:- func solver_var_pretty(varmap, solver_var) = cord(string).

solver_var_pretty(Varmap, sv_var(Var)) =
    singleton(format("Sv_%s", [s(get_var_name(Varmap, Var))])).
solver_var_pretty(_, sv_output(Num)) =
    singleton(format("Output_%i", [i(Num)])).

:- pred build_cp_func(core::in, func_id::in, problem(solver_var)::in,
    problem(solver_var)::out) is det.

build_cp_func(Core, FuncId, !Problem) :-
    trace [io(!IO), compile_time(flag("typecheck_solve"))] (
        % TODO: Fix this once we can typecheck SCCs as it might not make
        % sense anymore.
        core_lookup_function_name(Core, FuncId, FuncName),
        format("Building typechecking problem for %s\n",
            [s(q_name_to_string(FuncName))], !IO)
    ),

    core_get_function_det(Core, FuncId, Func),
    func_get_type_signature(Func, InputTypes, OutputTypes, _),
    ( if func_get_body(Func, _, Inputs, Expr) then
        some [!TypeVars, !TypeVarSource] (
            !:TypeVars = init_type_vars,
            Context = func_get_context(Func),

            start_type_var_mapping(!TypeVars),
            % Determine which type variables are free (universally
            % quantified).
            foldl2(set_free_type_vars, OutputTypes, [], ParamFreeVarLits0,
                !TypeVars),
            foldl2(set_free_type_vars, InputTypes,
                ParamFreeVarLits0, ParamFreeVarLits, !TypeVars),
            post_constraint(make_conjunction_from_lits(ParamFreeVarLits),
                !Problem),

            map_foldl3(build_cp_output(Context), OutputTypes, OutputConstrs,
                0, _, !Problem, !TypeVars),
            map_corresponding_foldl2(build_cp_inputs(Context), InputTypes,
                Inputs, InputConstrs, !Problem, !TypeVars),
            post_constraint(make_conjunction(OutputConstrs ++ InputConstrs),
                !Problem),
            end_type_var_mapping(!TypeVars),

            build_cp_expr(Core, Expr, TypesOrVars, !Problem, !TypeVars),
            list.map_foldl(unify_with_output(Context), TypesOrVars,
                Constraints, 0, _),

            _ = !.TypeVars, % TODO: Is this needed?

            post_constraint(make_conjunction(Constraints), !Problem)
        )
    else
        unexpected($module, $pred, "Imported pred")
    ).

:- pred set_free_type_vars(type_::in,
    list(constraint_literal(V))::in, list(constraint_literal(V))::out,
    type_var_map(type_var)::in, type_var_map(type_var)::out) is det.

set_free_type_vars(builtin_type(_), !Lits, !TypeVarMap).
set_free_type_vars(type_variable(TypeVar), Lits, [Lit | Lits], !TypeVarMap) :-
    maybe_add_free_type_var(TypeVar, Lit, !TypeVarMap).
set_free_type_vars(type_ref(_, Args), !Lits, !TypeVarMap) :-
    foldl2(set_free_type_vars, Args, !Lits, !TypeVarMap).
set_free_type_vars(func_type(Args, Returns), !Lits, !TypeVarMap) :-
    foldl2(set_free_type_vars, Args, !Lits, !TypeVarMap),
    foldl2(set_free_type_vars, Returns, !Lits, !TypeVarMap).

:- pred build_cp_output(context::in, type_::in,
    constraint(solver_var)::out, int::in, int::out,
    P::in, P::out, type_var_map(string)::in, type_var_map(string)::out) is det
    <= var_source(P).

build_cp_output(Context, Out, Constraint, !ResNum, !Problem, !TypeVars) :-
    build_cp_type(Context, Out, v_named(sv_output(!.ResNum)), Constraint,
        !Problem, !TypeVars),
    !:ResNum = !.ResNum + 1.

:- pred build_cp_inputs(context::in, type_::in, varmap.var::in,
    constraint(solver_var)::out,
    P::in, P::out, type_var_map(string)::in, type_var_map(string)::out) is det
    <= var_source(P).

build_cp_inputs(Context, Type, Var, Constraint, !Problem, !TypeVars) :-
    build_cp_type(Context, Type, v_named(sv_var(Var)), Constraint,
        !Problem, !TypeVars).

:- pred unify_with_output(context::in, type_or_var::in,
    constraint(solver_var)::out, int::in, int::out) is det.

unify_with_output(Context, TypeOrVar, Constraint, !ResNum) :-
    OutputVar = v_named(sv_output(!.ResNum)),
    !:ResNum = !.ResNum + 1,
    ( TypeOrVar = type_(Type),
        Constraint = build_cp_simple_type(Context, Type, OutputVar)
    ; TypeOrVar = var(Var),
        Constraint = make_constraint(cl_var_var(Var, OutputVar, Context))
    ).

    % An expressions type is either known directly (and has no holes), or is
    % the given variable's type.
    %
:- type type_or_var
    --->    type_(simple_type)
    ;       var(var(solver_var)).

:- type simple_type
    --->    builtin_type(builtin_type)
    ;       type_ref(type_id).

:- pred build_cp_expr(core::in, expr::in, list(type_or_var)::out,
    problem(solver_var)::in, problem(solver_var)::out,
    type_vars::in, type_vars::out) is det.

build_cp_expr(Core, expr(ExprType, CodeInfo), TypesOrVars, !Problem,
        !TypeVars) :-
    Context = code_info_get_context(CodeInfo),
    ( ExprType = e_tuple(Exprs),
        map_foldl2(build_cp_expr(Core), Exprs, ExprsTypesOrVars, !Problem,
            !TypeVars),
        TypesOrVars = map(one_item, ExprsTypesOrVars)
    ; ExprType = e_let(LetVars, ExprLet, ExprIn),
        build_cp_expr_let(Core, LetVars, ExprLet, ExprIn, Context,
            TypesOrVars, !Problem, !TypeVars)
    ; ExprType = e_call(Callee, Args),
        build_cp_expr_call(Core, Callee, Args, Context,
            TypesOrVars, !Problem, !TypeVars)
    ; ExprType = e_match(Var, Cases),
        map_foldl2(build_cp_case(Core, Var), Cases, CasesTypesOrVars,
            !Problem, !TypeVars),
        unify_types_or_vars_list(Context, CasesTypesOrVars, TypesOrVars,
            Constraint),
        post_constraint(Constraint, !Problem)
    ; ExprType = e_var(Var),
        TypesOrVars = [var(v_named(sv_var(Var)))]
    ; ExprType = e_constant(Constant),
        TypesOrVars = build_cp_expr_constant(Constant)
    ; ExprType = e_construction(CtorId, Args),
        build_cp_expr_construction(Core, CtorId, Args, Context, TypesOrVars,
            !Problem, !TypeVars)
    ).

:- pred build_cp_expr_let(core::in,
    list(var)::in, expr::in, expr::in, context::in,
    list(type_or_var)::out, problem(solver_var)::in, problem(solver_var)::out,
    type_vars::in, type_vars::out) is det.

build_cp_expr_let(Core, LetVars, ExprLet, ExprIn, Context,
        TypesOrVars, !Problem, !TypeVars) :-
    build_cp_expr(Core, ExprLet, LetTypesOrVars, !Problem,
        !TypeVars),
    map_corresponding(
        (pred(Var::in, TypeOrVar::in, Con::out) is det :-
            SVar = v_named(sv_var(Var)),
            ( TypeOrVar = var(EVar),
                Con = make_constraint(cl_var_var(SVar, EVar, Context))
            ; TypeOrVar = type_(Type),
                Con = build_cp_simple_type(Context, Type, SVar)
            )
        ), LetVars, LetTypesOrVars, Cons),
    build_cp_expr(Core, ExprIn, TypesOrVars, !Problem, !TypeVars),
    post_constraint(make_conjunction(Cons), !Problem).

:- pred build_cp_expr_call(core::in,
    func_id::in, list(var)::in, context::in,
    list(type_or_var)::out, problem(solver_var)::in, problem(solver_var)::out,
    type_vars::in, type_vars::out) is det.

build_cp_expr_call(Core, Callee, Args, Context,
        TypesOrVars, !Problem, !TypeVars) :-
    core_get_function_det(Core, Callee, Function),
    func_get_type_signature(Function, ParameterTypes, ResultTypes, _),
    start_type_var_mapping(!TypeVars),
    map_corresponding_foldl2(unify_param(Context), ParameterTypes, Args,
        ParamsLiterals, !Problem, !TypeVars),
    post_constraint(make_conjunction(ParamsLiterals), !Problem),
    % XXX: need a new type of solver var for ResultSVars, maybe need
    % expression numbers again?
    map_foldl2(unify_or_return_result(Context), ResultTypes,
        TypesOrVars, !Problem, !TypeVars),
    end_type_var_mapping(!TypeVars).

:- pred build_cp_case(core::in, var::in, expr_case::in, list(type_or_var)::out,
    problem(solver_var)::in, problem(solver_var)::out,
    type_vars::in, type_vars::out) is det.

build_cp_case(Core, Var, e_case(Pattern, Expr), TypesOrVars, !Problem,
        !TypeVarSource) :-
    Context = code_info_get_context(Expr ^ e_info),
    build_cp_pattern(Core, Context, Pattern, Var, Constraint,
        !Problem, !TypeVarSource),
    post_constraint(Constraint, !Problem),
    build_cp_expr(Core, Expr, TypesOrVars, !Problem, !TypeVarSource).

:- pred build_cp_pattern(core::in, context::in, expr_pattern::in, var::in,
    constraint(solver_var)::out, P::in, P::out,
    type_vars::in, type_vars::out) is det <= var_source(P).

build_cp_pattern(_, _, p_num(_), Var, Constraint, !Problem, !TypeVarSource) :-
    Constraint = make_constraint(cl_var_builtin(v_named(sv_var(Var)), int)).
build_cp_pattern(_, Context, p_variable(VarA), Var, Constraint,
        !Problem, !TypeVarSource) :-
    Constraint = make_constraint(
        cl_var_var(v_named(sv_var(VarA)), v_named(sv_var(Var)), Context)).
build_cp_pattern(_, _, p_wildcard, _, make_constraint(cl_true),
    !Problem, !TypeVarSource).
build_cp_pattern(Core, Context, p_ctor(CtorId, Args), Var, Constraint,
        !Problem, !TypeVarSource) :-
    SVar = v_named(sv_var(Var)),
    core_get_constructor_types(Core, CtorId, length(Args), Types),

    map_foldl2(build_cp_ctor_type(Core, CtorId, SVar, Args, Context),
        to_sorted_list(Types), Disjuncts, !Problem, !TypeVarSource),
    Constraint = make_disjunction(Disjuncts).

:- func build_cp_expr_constant(const_type) = list(type_or_var).

build_cp_expr_constant(c_string(_)) =
    [type_(builtin_type(string))].
build_cp_expr_constant(c_number(_)) =
    [type_(builtin_type(int))].
build_cp_expr_constant(c_func(_)) =
    util.sorry($file, $pred, "Higher order value").
build_cp_expr_constant(c_ctor(_)) =
    util.sorry($file, $pred, "Constructor").

:- pred build_cp_expr_construction(core::in,
    ctor_id::in, list(var)::in, context::in, list(type_or_var)::out,
    problem(solver_var)::in, problem(solver_var)::out,
    type_vars::in, type_vars::out) is det.

build_cp_expr_construction(Core, CtorId, Args, Context, TypesOrVars,
        !Problem, !TypeVars) :-
    new_variable("Constructor expression", SVar, !Problem),
    TypesOrVars = [var(SVar)],

    core_get_constructor_types(Core, CtorId, length(Args), Types),
    map_foldl2(build_cp_ctor_type(Core, CtorId, SVar, Args, Context),
        set.to_sorted_list(Types), Constraints, !Problem, !TypeVars),
    post_constraint(make_disjunction(Constraints), !Problem).

%-----------------------------------------------------------------------%

:- pred build_cp_ctor_type(core::in, ctor_id::in, var(solver_var)::in,
    list(var)::in, context::in, type_id::in, constraint(solver_var)::out,
    P::in, P::out, type_vars::in, type_vars::out) is det <= var_source(P).

build_cp_ctor_type(Core, CtorId, SVar, Args, Context, TypeId, Constraint,
        !Problem, !TypeVars) :-
    core_get_constructor_det(Core, TypeId, CtorId, Ctor),

    start_type_var_mapping(!TypeVars),

    TypeVarNames = Ctor ^ c_params,
    map_foldl(make_type_var, TypeVarNames, TypeVars, !TypeVars),

    map_corresponding_foldl2(build_cp_ctor_type_arg(Context), Args,
        Ctor ^ c_fields, ArgConstraints, !Problem, !TypeVars),
    % TODO: record how type variables are mapped and filled in the type
    % constraint below.
    end_type_var_mapping(!TypeVars),

    ResultConstraint = make_constraint(cl_var_usertype(SVar, TypeId,
        TypeVars, Context)),
    Constraint =
        make_conjunction([ResultConstraint | ArgConstraints]).

:- pred build_cp_ctor_type_arg(context::in, var::in, type_field::in,
    constraint(solver_var)::out, P::in, P::out,
    type_var_map(type_var)::in, type_var_map(type_var)::out)
    is det <= var_source(P).

build_cp_ctor_type_arg(Context, Arg, Field, Constraint,
        !Problem, !TypeVarMap) :-
    Type = Field ^ tf_type,
    ArgVar = v_named(sv_var(Arg)),
    ( Type = builtin_type(Builtin),
        Constraint = make_constraint(cl_var_builtin(ArgVar, Builtin))
    ; Type = type_ref(TypeId, Args),
        new_variables("Ctor arg", length(Args), ArgsVars, !Problem),
        % TODO: Handle type variables nested within deeper type expressions.
        map_corresponding_foldl2(build_cp_type(Context), Args, ArgsVars,
            ArgConstraints, !Problem, !TypeVarMap),
        HeadConstraint = make_constraint(cl_var_usertype(ArgVar, TypeId,
            ArgsVars, Context)),
        Constraint = make_conjunction([HeadConstraint | ArgConstraints])
    ; Type = func_type(_, _),
        util.sorry($file, $pred, "Function type")
    ; Type = type_variable(TypeVarStr),
        TypeVar = lookup_type_var(!.TypeVarMap, TypeVarStr),
        Constraint = make_constraint(cl_var_var(ArgVar, TypeVar, Context))
    ).

%-----------------------------------------------------------------------%

:- pred unify_types_or_vars_list(context::in, list(list(type_or_var))::in,
    list(type_or_var)::out, constraint(solver_var)::out) is det.

unify_types_or_vars_list(_, [], _, _) :-
    unexpected($file, $pred, "No cases").
unify_types_or_vars_list(Context, [ToVsHead | ToVsTail], ToVs,
        make_conjunction(Constraints)) :-
    unify_types_or_vars_list(Context, ToVsHead, ToVsTail, ToVs, Constraints).

:- pred unify_types_or_vars_list(context::in, list(type_or_var)::in,
    list(list(type_or_var))::in, list(type_or_var)::out,
    list(constraint(solver_var))::out) is det.

unify_types_or_vars_list(_, ToVs, [], ToVs, []).
unify_types_or_vars_list(Context, ToVsA, [ToVsB | ToVsTail], ToVs,
        CHeads ++ CTail) :-
    map2_corresponding(unify_type_or_var(Context), ToVsA, ToVsB,
        ToVs0, CHeads),
    unify_types_or_vars_list(Context, ToVs0, ToVsTail, ToVs, CTail).

:- pred unify_type_or_var(context::in, type_or_var::in, type_or_var::in,
    type_or_var::out, constraint(solver_var)::out) is det.

unify_type_or_var(Context, type_(TypeA), ToVB, ToV, Constraint) :-
    ( ToVB = type_(TypeB),
        ( if TypeA = TypeB then
            ToV = type_(TypeA)
        else
            compile_error($file, $pred, "Compilation error, cannot unify types")
        ),
        Constraint = make_constraint(cl_true)
    ;
        ToVB = var(Var),
        % It's important to return the var, rather than the type, so that
        % all the types end up getting unified with one-another by the
        % solver.
        ToV = var(Var),
        Constraint = build_cp_simple_type(Context, TypeA, Var)
    ).
unify_type_or_var(Context, var(VarA), ToVB, ToV, Constraint) :-
    ( ToVB = type_(Type),
        unify_type_or_var(Context, type_(Type), var(VarA), ToV, Constraint)
    ; ToVB = var(VarB),
        ToV = var(VarA),
        ( if VarA = VarB then
            Constraint = make_constraint(cl_true)
        else
            Constraint = make_constraint(cl_var_var(VarA, VarB, Context))
        )
    ).

:- pred unify_param(context::in, type_::in, var::in,
    constraint(solver_var)::out, P::in, P::out,
    type_var_map(string)::in, type_var_map(string)::out) is det
    <= var_source(P).

unify_param(Context, PType, ArgVar, Constraint, !Problem, !TypeVars) :-
    % XXX: Should be using TVarmap to handle type variables correctly.
    build_cp_type(Context, PType, v_named(sv_var(ArgVar)), Constraint,
        !Problem, !TypeVars).

:- pred unify_or_return_result(context::in, type_::in,
    type_or_var::out,
   	problem(solver_var)::in, problem(solver_var)::out,
    type_var_map(string)::in, type_var_map(string)::out) is det.

unify_or_return_result(_, builtin_type(Builtin),
        type_(builtin_type(Builtin)), !Problem, !TypeVars).
unify_or_return_result(_, type_variable(TypeVar),
        var(SVar), !Problem, !TypeVars) :-
    get_or_make_type_var(TypeVar, SVar, !TypeVars).
unify_or_return_result(_, func_type(_, _), _, !Problem, !TypeVars) :-
    util.sorry($file, $pred, "Function type").
unify_or_return_result(Context, type_ref(TypeId, Args),
        var(SVar), !Problem, !TypeVars) :-
    new_variable("?", SVar, !Problem),
    build_cp_type(Context, type_ref(TypeId, Args),
        SVar, Constraint, !Problem, !TypeVars),
    post_constraint(Constraint, !Problem).

%-----------------------------------------------------------------------%

:- pred build_cp_type(context::in, type_::in, solve.var(solver_var)::in,
    constraint(solver_var)::out, P::in, P::out,
    type_var_map(string)::in, type_var_map(string)::out) is det
    <= var_source(P).

build_cp_type(_, builtin_type(Builtin), Var,
    make_constraint(cl_var_builtin(Var, Builtin)), !Problem, !TypeVarMap).
build_cp_type(Context, type_variable(TypeVarStr), Var, Constraint,
        !Problem, !TypeVarMap) :-
    get_or_make_type_var(TypeVarStr, TypeVar, !TypeVarMap),
    Constraint = make_constraint(cl_var_var(Var, TypeVar, Context)).
build_cp_type(Context, type_ref(TypeId, Args), Var,
        make_conjunction([Constraint | ArgConstraints]),
        !Problem, !TypeVarMap) :-
    NumArgs = length(Args),
    new_variables("?", NumArgs, ArgVars, !Problem),
    map_corresponding_foldl2(build_cp_type(Context),
        Args, ArgVars, ArgConstraints, !Problem, !TypeVarMap),
    Constraint = make_constraint(cl_var_usertype(Var, TypeId, ArgVars,
        Context)).
build_cp_type(_, func_type(_, _), _, _, !Problem, !TypeVarMap) :-
    util.sorry($file, $pred, "Function type").

:- func build_cp_simple_type(context, simple_type,
    var(solver_var)) = constraint(solver_var).

build_cp_simple_type(_, builtin_type(Builtin), Var) =
    make_constraint(cl_var_builtin(Var, Builtin)).
build_cp_simple_type(Context, type_ref(TypeId), Var) =
    make_constraint(cl_var_usertype(Var, TypeId, [], Context)).

%-----------------------------------------------------------------------%

:- pred update_types_func(core::in, map(solver_var, type_)::in,
    function::in, result(function, compile_error)::out) is det.

update_types_func(Core, TypeMap, !.Func, Result) :-
    some [!Expr] (
        ( if func_get_body(!.Func, Varmap, Inputs, !:Expr) then
            func_get_type_signature(!.Func, _, OutputTypes, _),
            update_types_expr(Core, TypeMap, OutputTypes, !Expr),
            map.foldl(svar_type_to_var_type_map, TypeMap, map.init, VarTypes),
            func_set_body(Varmap, Inputs, !.Expr, !Func),
            func_set_vartypes(VarTypes, !Func),
            Result = ok(!.Func)
        else
            unexpected($file, $pred, "imported pred")
        )
    ).

:- pred svar_type_to_var_type_map(solver_var::in, type_::in,
    map(var, type_)::in, map(var, type_)::out) is det.

svar_type_to_var_type_map(sv_var(Var), Type, !Map) :-
    det_insert(Var, Type, !Map).
svar_type_to_var_type_map(sv_output(_), _, !Map).

:- pred update_types_expr(core::in, map(solver_var, type_)::in,
    list(type_)::in, expr::in, expr::out) is det.

update_types_expr(Core, TypeMap, Types, !Expr) :-
    !.Expr = expr(ExprType0, CodeInfo0),
    ( ExprType0 = e_tuple(Exprs0),
        map_corresponding(update_types_expr(Core, TypeMap),
            map(func(T) = [T], Types),
            Exprs0, Exprs),
        ExprType = e_tuple(Exprs)
    ; ExprType0 = e_let(LetVars, ExprLet0, ExprIn0),
        map((pred(V::in, T::out) is det :-
                lookup(TypeMap, sv_var(V), T)
            ), LetVars, TypesLet),
        update_types_expr(Core, TypeMap, TypesLet, ExprLet0, ExprLet),
        update_types_expr(Core, TypeMap, Types, ExprIn0, ExprIn),
        ExprType = e_let(LetVars, ExprLet, ExprIn)
    ; ExprType0 = e_call(_, _),
        ExprType = ExprType0
    ; ExprType0 = e_match(Var, Cases0),
        map(update_types_case(Core, TypeMap, Types), Cases0, Cases),
        ExprType = e_match(Var, Cases)
    ; ExprType0 = e_var(Var),
        ExprType = ExprType0,
        lookup(TypeMap, sv_var(Var), Type),
        ( if Types \= [Type] then
            unexpected($file, $pred, "Types do not match assertion")
        else
            true
        )
    ; ExprType0 = e_constant(Constant),
        ExprType = ExprType0,
        ( if Types \= [const_type(Constant)] then
            unexpected($file, $pred, "Types do not match assertion")
        else
            true
        )
    ; ExprType0 = e_construction(_, _),
        ExprType = ExprType0
    ),
    code_info_set_types(Types, CodeInfo0, CodeInfo),
    !:Expr = expr(ExprType, CodeInfo).

:- pred update_types_case(core::in, map(solver_var, type_)::in,
    list(type_)::in, expr_case::in, expr_case::out) is det.

update_types_case(Core, TypeMap, Types,
        e_case(Pat, Expr0), e_case(Pat, Expr)) :-
    update_types_expr(Core, TypeMap, Types, Expr0, Expr).

%-----------------------------------------------------------------------%

:- func const_type(const_type) = type_.

const_type(c_string(_)) = builtin_type(string).
const_type(c_number(_)) = builtin_type(int).
const_type(c_func(_)) = util.sorry($file, $pred, "Higher order value").
const_type(c_ctor(_)) = util.sorry($file, $pred, "Bare constructor").

%-----------------------------------------------------------------------%
