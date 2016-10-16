%-----------------------------------------------------------------------%
% Plasma typechecking
% vim: ts=4 sw=4 et
%
% Copyright (C) 2016 Plasma Team
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
:- import_module map.
:- import_module string.

:- import_module util.

:- include_module core.typecheck.solve.
:- import_module core.typecheck.solve.

%-----------------------------------------------------------------------%

typecheck(Errors, !Core) :-
    SCCs = core_all_nonimported_functions_sccs(!.Core),
    map_foldl(typecheck_scc, SCCs, ErrorsList, !Core),
    Errors = cord_list_to_cord(ErrorsList).

:- pred typecheck_scc(set(func_id)::in, errors(compile_error)::out,
    core::in, core::out) is det.

typecheck_scc(SCC, Errors, !Core) :-
    % The first step is to compute the arity of each expression.
    compute_arity(SCC, ArityErrors, !Core),
    ( if is_empty(ArityErrors) then
        % Now do the real typechecking.
        build_cp_problem(!.Core, SCC, Constraints),
        solve(Constraints, Mapping),
        update_types(Mapping, SCC, Errors, !Core)
    else
        Errors = ArityErrors
    ).

%-----------------------------------------------------------------------%

    % Determine the number of values returned by each expression in the SCC.
    %
:- pred compute_arity(set(func_id)::in, errors(compile_error)::out,
    core::in, core::out) is det.

compute_arity(SCC, Errors, !Core) :-
    ( if singleton_set(FuncId, SCC) then
        compute_arity_func(FuncId, Errors, !Core)
    else
        % TODO Need to write a fixpoint computation.
        util.sorry($file, $pred, "Mutual recursion")
    ).

:- pred compute_arity_func(func_id::in, errors(compile_error)::out,
    core::in, core::out) is det.

compute_arity_func(FuncId, Errors, !Core) :-
    core_get_function_det(!.Core, FuncId, Func0),
    func_get_signature(Func0, _, _, DeclaredArity),
    ( if func_get_body(Func0, Varmap, Args, Expr0) then
        compute_arity_expr(!.Core, Expr0, Expr, ArityResult),
        ( ArityResult = ok(Arity),
            ( if Arity = DeclaredArity then
                func_set_body(Varmap, Args, Expr, Func0, Func),
                core_set_function(FuncId, Func, !Core),
                Errors = init
            else
                Errors = error(func_get_context(Func0),
                    ce_arity_mismatch_func(DeclaredArity, Arity))
            )
        ; ArityResult = errors(Errors)
        )
    else
        % Function is imported
        Errors = init
    ).

:- pred compute_arity_expr(core::in, expr::in, expr::out,
    result(arity, compile_error)::out) is det.

compute_arity_expr(Core, expr(ExprType0, CodeInfo0), expr(ExprType, CodeInfo),
        Result) :-
    Context = code_info_get_context(CodeInfo0),
    ( ExprType0 = e_tuple(Exprs0),
        map2(compute_arity_expr(Core), Exprs0, Exprs, ListResults),
        ExprType = e_tuple(Exprs),
        code_info_set_arity(arity(length(Exprs)), CodeInfo0, CodeInfo),
        Result = result_map((func(_) = arity(length(Exprs))),
            result_list_to_result(ListResults))
    ; ExprType0 = e_let(Vars, ExprLet0, ExprIn0),
        compute_arity_expr(Core, ExprLet0, ExprLet, LetRes),
        ( LetRes = ok(LetArity),
            ( if length(Vars) = LetArity ^ a_num then
                compute_arity_expr(Core, ExprIn0, ExprIn, InRes),
                ( InRes = ok(ExprArity),
                    code_info_set_arity(ExprArity, CodeInfo0, CodeInfo),
                    ExprType = e_let(Vars, ExprLet, ExprIn),
                    Result = ok(ExprArity)
                ; InRes = errors(Errors),
                    ExprType = e_let(Vars, ExprLet, ExprIn0),
                    CodeInfo = CodeInfo0,
                    Result = errors(Errors)
                )
            else
                ExprType = e_let(Vars, ExprLet, ExprIn0),
                CodeInfo = CodeInfo0,
                Result = return_error(Context,
                    ce_arity_mismatch_expr(LetArity, arity(length(Vars))))
            )
        ; LetRes = errors(Errors),
            ExprType = e_let(Vars, ExprLet0, ExprIn0),
            CodeInfo = CodeInfo0,
            Result = errors(Errors)
        )
    ; ExprType0 = e_call(Callee, Args),
        ExprType = e_call(Callee, Args),
        core_get_function_det(Core, Callee, CalleeFn),
        func_get_signature(CalleeFn, Inputs, _, Arity),
        length(Inputs, InputsLen),
        length(Args, ArgsLen),
        ( if InputsLen = ArgsLen then
            InputErrors = init
        else
            InputErrors = error(Context, ce_parameter_number(length(Inputs),
                length(Args)))
        ),
        code_info_set_arity(Arity, CodeInfo0, CodeInfo),
        ( if is_empty(InputErrors) then
            Result = ok(Arity)
        else
            Result = errors(InputErrors)
        )
    ; ExprType0 = e_match(Var, Cases0),
        map2(compute_arity_case(Core), Cases0, Cases, CaseResults),
        Result0 = result_list_to_result(CaseResults),
        ( Result0 = ok(CaseArities),
            (
                CaseArities = [],
                CodeInfo = CodeInfo0,
                Result = return_error(Context, ce_match_has_no_cases)
            ;
                CaseArities = [Arity | _],
                ( if all_same(CaseArities) then
                    code_info_set_arity(Arity, CodeInfo0, CodeInfo),
                    Result = ok(Arity)
                else
                    CodeInfo = CodeInfo0,
                    Result = return_error(Context,
                        ce_arity_mismatch_match(CaseArities))
                )
            )
        ; Result0 = errors(Errors),
            CodeInfo = CodeInfo0,
            Result = errors(Errors)
        ),
        ExprType = e_match(Var, Cases)
    ;
        ( ExprType0 = e_var(_)
        ; ExprType0 = e_constant(_)
        ; ExprType0 = e_construction(_)
        ),
        Arity = arity(1),
        code_info_set_arity(Arity, CodeInfo0, CodeInfo),
        ExprType = ExprType0,
        Result = ok(Arity)
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

:- pred build_cp_problem(core::in, set(func_id)::in,
    problem(solver_var)::out) is det.

build_cp_problem(Core, SCC, Problem) :-
    ( if singleton_set(FuncId, SCC) then
        build_cp_func(Core, FuncId, init, Problem)
    else
        util.sorry($file, $pred, "Mutual recursion")
    ).

:- pred build_cp_func(core::in, func_id::in,
    problem(solver_var)::in, problem(solver_var)::out) is det.

build_cp_func(Core, FuncId, !Problem) :-
    core_get_function_det(Core, FuncId, Func),
    func_get_signature(Func, InputTypes, OutputTypes, _),
    ( if func_get_body(Func, _, Inputs, Expr) then
        some [!TypeVars] (
            !:TypeVars = init,
            build_cp_outputs(OutputTypes, 0, !Problem, !TypeVars),
            build_cp_inputs(InputTypes, Inputs, 0, !Problem, !.TypeVars, _),
            build_cp_expr(Core, Expr, TypesOrVars, !Problem),
            list.foldl2(unify_with_output, TypesOrVars, 0, _, !Problem)
        )
    else
        unexpected($module, $pred, "Imported pred")
    ).

:- pred build_cp_outputs(list(type_)::in, int::in,
    problem(solver_var)::in, problem(solver_var)::out,
    type_vars::in, type_vars::out) is det.

build_cp_outputs([], _, !Problem, !TypeVars).
build_cp_outputs([Out | Outs], ResNum, !Problem, !TypeVars) :-
    % TODO: Should use !TypeVars to handle type variables in the declration
    % correctly.
    build_cp_type(Out, v_named(sv_output(ResNum)), !Problem),
    build_cp_outputs(Outs, ResNum+1, !Problem, !TypeVars).

:- pred build_cp_inputs(list(type_)::in, list(varmap.var)::in,
    int::in, problem(solver_var)::in, problem(solver_var)::out,
    type_vars::in, type_vars::out) is det.

build_cp_inputs([], [], _, !Problem, !TypeVars).
build_cp_inputs([], [_ | _], _, _, _, _, _) :-
    compile_error($file, $pred, "Too many arguments").
build_cp_inputs([_ | _], [], _, _, _, _, _) :-
    util.sorry($file, $pred, "Partial application").
build_cp_inputs([Type | Types], [Var | Vars], ParamNum, !Problem, !TypeVars) :-
    % TODO: Should use !TypeVars to handle type variables in the declration
    % correctly.
    build_cp_type(Type, v_named(sv_var(Var)), !Problem),
    build_cp_inputs(Types, Vars, ParamNum + 1, !Problem, !TypeVars).

:- pred unify_with_output(type_or_var::in, int::in, int::out,
    problem(solver_var)::in, problem(solver_var)::out) is det.

unify_with_output(TypeOrVar, !ResNum, !Problem) :-
    OutputVar = v_named(sv_output(!.ResNum)),
    !:ResNum = !.ResNum + 1,
    ( TypeOrVar = type_(Type),
        build_cp_type(Type, OutputVar, !Problem)
    ; TypeOrVar = var(Var),
        post_constraint_alias(v_named(sv_var(Var)), OutputVar, !Problem)
    ; TypeOrVar = types(Types),
        post_constraint_user_types(Types, OutputVar, !Problem)
    ).

    % An expressions type is either known directly, or is the given
    % variable's type.
    %
:- type type_or_var
    --->    type_(type_)

            % One of multiple possible types.
    ;       types(set(type_id))
    ;       var(var).

:- pred build_cp_expr(core::in, expr::in, list(type_or_var)::out,
    problem(solver_var)::in, problem(solver_var)::out) is det.

build_cp_expr(Core, expr(ExprType, _CodeInfo), TypesOrVars, !Problem) :-
    ( ExprType = e_tuple(Exprs),
        map_foldl(build_cp_expr(Core), Exprs, ExprsTypesOrVars, !Problem),
        TypesOrVars = map(one_item, ExprsTypesOrVars)
    ; ExprType = e_let(LetVars, ExprLet, ExprIn),
        build_cp_expr(Core, ExprLet, LetTypesOrVars, !Problem),
        foldl_corresponding(
            (pred(Var::in, TypeOrVar::in, P0::in, P::out) is det :-
                SVar = v_named(sv_var(Var)),
                ( TypeOrVar = var(EVar),
                    post_constraint_alias(SVar, v_named(sv_var(EVar)), P0, P)
                ; TypeOrVar = type_(Type),
                    build_cp_type(Type, SVar, P0, P)
                ; TypeOrVar = types(Types),
                    post_constraint_user_types(Types, SVar, P0, P)
                )
            ), LetVars, LetTypesOrVars, !Problem),
        build_cp_expr(Core, ExprIn, TypesOrVars, !Problem)
    ; ExprType = e_call(Callee, Args),
        core_get_function_det(Core, Callee, Function),
        func_get_signature(Function, ParameterTypes, ResultTypes, _),
        unify_params(ParameterTypes, Args, !Problem, init, _TVarmap),
        % NOTE: Type variables are not properly handled in results.
        TypesOrVars = map((func(T) = type_(T)), ResultTypes)
    ; ExprType = e_match(Var, Cases),
        map_foldl(build_cp_case(Core, Var), Cases, CasesTypesOrVars,
            !Problem),
        unify_types_or_vars_list(CasesTypesOrVars, TypesOrVars, !Problem)
    ; ExprType = e_var(Var),
        TypesOrVars = [var(Var)]
    ; ExprType = e_constant(Constant),
        TypesOrVars = [type_(const_type(Constant))]
    ; ExprType = e_construction(ConsId),
        core_get_constructor_det(Core, ConsId, Constructor),
        TypesOrVars = [types(Constructor ^ c_types)]
    ).

:- pred build_cp_case(core::in, var::in, expr_case::in, list(type_or_var)::out,
    problem(solver_var)::in, problem(solver_var)::out) is det.

build_cp_case(Core, Var, e_case(Pattern, Expr), TypesOrVars,
        !Problem) :-
    build_cp_pattern(Core, Pattern, Var, !Problem),
    build_cp_expr(Core, Expr, TypesOrVars, !Problem).

:- pred build_cp_pattern(core::in, expr_pattern::in, var::in,
    problem(solver_var)::in, problem(solver_var)::out) is det.

build_cp_pattern(_, e_num(_), Var, !Problem) :-
    post_constraint_builtin(v_named(sv_var(Var)), int, !Problem).
build_cp_pattern(_, e_variable(VarA), Var, !Problem) :-
    post_constraint_alias(v_named(sv_var(VarA)), v_named(sv_var(Var)),
        !Problem).
build_cp_pattern(_, e_wildcard, _, !Problem).
build_cp_pattern(Core, e_constructor(ConsId), Var, !Problem) :-
    core_get_constructor_det(Core, ConsId, Constructor),
    Types = Constructor ^ c_types,
    post_constraint_user_types(Types, v_named(sv_var(Var)), !Problem).

%-----------------------------------------------------------------------%

:- pred unify_types_or_vars_list(list(list(type_or_var))::in,
    list(type_or_var)::out,
    problem(solver_var)::in, problem(solver_var)::out) is det.

unify_types_or_vars_list([], _, !Problem) :-
    unexpected($file, $pred, "No cases").
unify_types_or_vars_list([ToVsHead | ToVsTail], ToVs, !Problem) :-
    unify_types_or_vars_list(ToVsHead, ToVsTail, ToVs, !Problem).

:- pred unify_types_or_vars_list(list(type_or_var)::in,
    list(list(type_or_var))::in, list(type_or_var)::out,
    problem(solver_var)::in, problem(solver_var)::out) is det.

unify_types_or_vars_list(ToVs, [], ToVs, !Problem).
unify_types_or_vars_list(ToVsA, [ToVsB | ToVsTail], ToVs, !Problem) :-
    map_corresponding_foldl(unify_type_or_var, ToVsA, ToVsB, ToVs0, !Problem),
    unify_types_or_vars_list(ToVs0, ToVsTail, ToVs, !Problem).

:- pred unify_type_or_var(type_or_var::in, type_or_var::in, type_or_var::out,
    problem(solver_var)::in, problem(solver_var)::out) is det.

unify_type_or_var(type_(TypeA), ToVB, ToV, !Problem) :-
    ( ToVB = type_(TypeB),
        ( if TypeA = TypeB then
            ToV = type_(TypeA)
        else
            compile_error($file, $pred, "Compilation error, cannot unify types")
        )
    ;
        ToVB = types(Types),
        ( TypeA = builtin_type(_),
            compile_error($file, $pred, "Compilation error, cannot unify types")
        ; TypeA = type_variable(_),
            util.sorry($file, $pred, "Parametric types")
        ; TypeA = type_ref(TypeId),
            ( if member(TypeId, Types) then
                % This is probably incorrect, it doesn't record the typing
                % decision.  I'll revisit it later.
                ToV = type_(TypeA)
            else
                compile_error($file, $pred,
                    "Compilation error, cannot unify types")
            )
        )
    ;
        ToVB = var(Var),
        % It's important to return the var, rather than the type, so that
        % all the types end up getting unified with one-another by the
        % solver.
        ToV = var(Var),
        build_cp_type(TypeA, v_named(sv_var(Var)), !Problem)
    ).
unify_type_or_var(types(TypesA), ToVB, ToV, !Problem) :-
    ( ToVB = type_(_),
        unify_type_or_var(ToVB, types(TypesA), ToV, !Problem)
    ; ToVB = types(TypesB),
        Types = TypesA `intersect` TypesB,
        ( if is_empty(Types) then
            compile_error($file, $pred, "Compilation error, cannot unify types")
        else
            % This is probably incorrect, it doesn't record the typing
            % decision.  I'll revisit it later.
            ToV = types(Types)
        )
    ; ToVB = var(Var),
        post_constraint_user_types(TypesA, v_named(sv_var(Var)), !Problem),
        ToV = var(Var)
    ).
unify_type_or_var(var(VarA), ToVB, ToV, !Problem) :-
    ( ToVB = type_(Type),
        unify_type_or_var(type_(Type), var(VarA), ToV, !Problem)
    ; ToVB = types(Types),
        unify_type_or_var(types(Types), var(VarA), ToV, !Problem)
    ; ToVB = var(VarB),
        ToV = var(VarA),
        ( if VarA = VarB then
            true
        else
            post_constraint_alias(v_named(sv_var(VarA)),
                v_named(sv_var(VarB)), !Problem)
        )
    ).

:- pred unify_params(list(type_)::in, list(var)::in,
    problem(solver_var)::in, problem(solver_var)::out,
    type_vars::in, type_vars::out) is det.

unify_params([], [], !Problem, !TVarmap).
unify_params([], [_ | _], _, _, _, _) :-
    compile_error($file, $pred, "Too many arguments applied to function").
unify_params([_ | _], [], _, _, _, _) :-
    util.sorry($file, $pred, "Currying not yet supported").
unify_params([PType | PTypes], [ArgVar | ArgVars], !Problem, !TVarmap) :-
    % XXX: Should be using TVarmap to handle type variables correctly.
    build_cp_type(PType, v_named(sv_var(ArgVar)), !Problem),
    unify_params(PTypes, ArgVars, !Problem, !TVarmap).

%-----------------------------------------------------------------------%

:- pred build_cp_type(type_::in, solve.var(solver_var)::in,
    problem(solver_var)::in, problem(solver_var)::out) is det.

build_cp_type(builtin_type(Builtin), Var, !Problem) :-
    post_constraint_builtin(Var, Builtin, !Problem).
build_cp_type(type_variable(_), _, !Problem) :-
    util.sorry($file, $pred, "Type variables").
build_cp_type(type_ref(TypeId), Var, !Problem) :-
    post_constraint_user_type(TypeId, Var, !Problem).

%-----------------------------------------------------------------------%

:- pred update_types(map(solver_var, type_)::in,
    set(func_id)::in, errors(compile_error)::out, core::in, core::out) is det.

update_types(TypeMap, SCC, Errors, !Core) :-
    ( if singleton_set(FuncId, SCC) then
        update_types_func(TypeMap, FuncId, Errors, !Core)
    else
        util.sorry($file, $pred, "Mutual recursion")
    ).

:- pred update_types_func(map(solver_var, type_)::in,
    func_id::in, errors(compile_error)::out, core::in, core::out) is det.

update_types_func(TypeMap, FuncId, Errors, !Core) :-
    some [!Func, !Expr] (
        core_get_function_det(!.Core, FuncId, !:Func),
        ( if func_get_body(!.Func, Varmap, Inputs, !:Expr) then
            func_get_signature(!.Func, _, OutputTypes, _),
            update_types_expr(!.Core, TypeMap, OutputTypes, !Expr),
            Errors = init, % XXX
            map.foldl(svar_type_to_var_type_map, TypeMap, map.init, VarTypes),
            func_set_body(Varmap, Inputs, !.Expr, !Func),
            func_set_vartypes(VarTypes, !Func)
        else
            unexpected($file, $pred, "imported pred")
        ),
        core_set_function(FuncId, !.Func, !Core)
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
    ; ExprType0 = e_construction(_),
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

:- type type_vars == map(type_var, var(solver_var)).

%-----------------------------------------------------------------------%
