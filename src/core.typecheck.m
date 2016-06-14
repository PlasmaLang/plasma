%-----------------------------------------------------------------------%
% Plasma typechecking
% vim: ts=4 sw=4 et
%
% Copyright (C) 2016 Plasma Team
% Distributed under the terms of the MIT see ../LICENSE.code
%
% This module typechecks plasma core using a solver over Herbrand terms.
% Solver variables and constraints are created as follows.
%
% Consider an expression which performs a list cons:
%
% cons(elem, list)
%
% cons is declared as func(t, List(t)) -> List(t)
%
% + First, each expression has a number of results depending on its arity,
%   and each result of each expression has a type, which is represented as a
%   variable.  In this example these are: elem, list and cons(elem, list).
%   Each of these is also involved in a constraint which describes any types
%   we already know about:
%   elem = int
%   list = T0
%   cons(elem, list) = list(T1)
%
% + Parameters also have types represented by variables, a new set of these
%   must be created for each call site.  And they are matched
%   (uni-directional unification) against the type of the callee.  Matching
%   is important otherwise we could not call cons for a list(int) and a
%   list(string).
%
%   cons' first parameter has the type T1
%   cons' second parameter has the type list(T1).
%
% + Type variables also become parameters.  Here T0 ind T1 are free type
%   variables.  They also already appear in the constraints that
%   represent the types of other type variables.
%
% + A unification constraint is added for each argument - parameter pair.
%
%   T1 = int
%   list(T1) = T0
%
% Running propagation will now find the correct solutions.
%
% Other type variables and constraints are.
%
% + The parameters and return values of the current function.  Including
%   treatment of any type variables.
%
% Labeling will occur normally (trying different types) for type variables
% that do not appear in the signatures of the functions being typechecked.
% After all those symbols have been labeled then type variables appearing in
% the function's signatures are labeled.  Special values representing type
% variables rather than types are used.  This allows these values to be
% propagated and the completion of solving to be clear.
%
% TODO:
%  + Track types of variables.
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
        unexpected($file, $pred, "Mutual recursion unimplemented")
    ).

:- pred compute_arity_func(func_id::in, errors(compile_error)::out,
    core::in, core::out) is det.

compute_arity_func(FuncId, Errors, !Core) :-
    core_get_function_det(!.Core, FuncId, Func0),
    func_get_signature(Func0, _, _, DeclaredArity),
    ( if func_get_body(Func0, Varmap, Args, Expr0) then
        compute_arity_expr(!.Core, ArityResult, Expr0, Expr),
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

:- pred compute_arity_expr(core::in, result(arity, compile_error)::out,
    expr::in, expr::out) is det.

compute_arity_expr(Core, Result, expr(ExprType0, CodeInfo0),
        expr(ExprType, CodeInfo)) :-
    Context = code_info_get_context(CodeInfo0),
    ( ExprType0 = e_sequence(Exprs0),
        compute_arity_expr_list(Core, Result, Exprs0, Exprs),
        ExprType = e_sequence(Exprs),
        ( Result = ok(Arity),
            code_info_set_arity(Arity, CodeInfo0, CodeInfo)
        ; Result = errors(_),
            CodeInfo = CodeInfo0
        )
    ; ExprType0 = e_tuple(Exprs0),
        compute_arity_expr_list(Core, Result, Exprs0, Exprs),
        ExprType = e_tuple(Exprs),
        code_info_set_arity(arity(length(Exprs)), CodeInfo0, CodeInfo)
    ; ExprType0 = e_let(Vars, ExprLet0, ExprIn0),
        compute_arity_expr(Core, LetRes, ExprLet0, ExprLet),
        ( LetRes = ok(LetArity),
            ( if length(Vars) = LetArity ^ a_num then
                compute_arity_expr(Core, InRes, ExprIn0, ExprIn),
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
                unexpected($file, $pred, "Arity mismatch")
            )
        ; LetRes = errors(Errors),
            ExprType = e_let(Vars, ExprLet0, ExprIn0),
            CodeInfo = CodeInfo0,
            Result = errors(Errors)
        )
    ; ExprType0 = e_call(Callee, Args0),
        compute_arity_expr_list(Core, ArgsResult, Args0, Args),
        ExprType = e_call(Callee, Args),
        % XXX: Work around until functions have types.
        ( if Callee = expr(e_func(CalleeId), _) then
            core_get_function_det(Core, CalleeId, CalleeFn),
            func_get_signature(CalleeFn, Inputs, _, Arity)
        else
            sorry($pred, "Higher order call")
        ),
        length(Inputs, InputsLen),
        length(Args, ArgsLen),
        ( if InputsLen = ArgsLen then
            InputErrors = init
        else
            InputErrors = error(Context, ce_parameter_number(length(Inputs),
                length(Args)))
        ),
        code_info_set_arity(Arity, CodeInfo0, CodeInfo),
        ( ArgsResult = ok(_),
            ( if is_empty(InputErrors) then
                Result = ok(Arity)
            else
                Result = errors(InputErrors)
            )
        ; ArgsResult = errors(Errors),
            Result = errors(Errors ++ InputErrors)
        )
    ;
        ( ExprType0 = e_var(_)
        ; ExprType0 = e_const(_)
        ; ExprType0 = e_func(_)
        ),
        Arity = arity(1),
        code_info_set_arity(Arity, CodeInfo0, CodeInfo),
        ExprType = ExprType0,
        Result = ok(Arity)
    ).

:- pred compute_arity_expr_list(core::in, result(arity, compile_error)::out,
    list(expr)::in, list(expr)::out) is det.

compute_arity_expr_list(_, _, [], []) :-
    unexpected($file, $pred, "no expressions").
compute_arity_expr_list(Core, Result, [Expr0 | Exprs0], [Expr | Exprs]) :-
    compute_arity_expr(Core, ExprResult, Expr0, Expr),
    ( ExprResult = ok(Arity),
        compute_arity_expr_list_2(Core, Arity, Result, Exprs0, Exprs)
    ; ExprResult = errors(Errors),
        Exprs = Exprs0,
        Result = errors(Errors)
    ).

:- pred compute_arity_expr_list_2(core::in, arity::in,
    result(arity, compile_error)::out, list(expr)::in, list(expr)::out)
    is det.

compute_arity_expr_list_2(_, Arity, ok(Arity), [], []).
compute_arity_expr_list_2(Core, _, Result, [Expr0 | Exprs0], [Expr | Exprs]) :-
    compute_arity_expr(Core, ExprResult, Expr0, Expr),
    ( ExprResult = ok(Arity),
        compute_arity_expr_list_2(Core, Arity, Result, Exprs0, Exprs)
    ; ExprResult = errors(Errors),
        Exprs = Exprs0,
        Result = errors(Errors)
    ).

%-----------------------------------------------------------------------%

    % Solver variable.
:- type type_position
            % The type of an expression.
    --->    tp_expr(
                tpe_expr_num        :: int,
                tpe_result_num      :: int
            )
            % The type of an input parameter.
    ;       tp_input(
                tpi_param_num       :: int
            )

            % The type of an output value.
    ;       tp_output(
                tpo_result_num      :: int
            ).

:- pred build_cp_problem(core::in, set(func_id)::in,
    problem(type_position)::out) is det.

build_cp_problem(Core, SCC, Problem) :-
    ( if singleton_set(FuncId, SCC) then
        build_cp_func(Core, FuncId, init, Problem)
    else
        unexpected($file, $pred, "Mutual recursion unimplemented")
    ).

:- pred build_cp_func(core::in, func_id::in,
    problem(type_position)::in, problem(type_position)::out) is det.

build_cp_func(Core, FuncId, !Problem) :-
    core_get_function_det(Core, FuncId, Func),
    func_get_signature(Func, InputTypes, OutputTypes, _),
    ( if func_get_body(Func, _, Inputs, Expr) then
        some [!TypeVars] (
            !:TypeVars = init,
            build_cp_outputs(OutputTypes, 0, !Problem, !TypeVars),
            build_cp_inputs(InputTypes, Inputs, 0, !Problem, !.TypeVars, _,
                map.init, Varmap),
            build_cp_expr(Core, Varmap, Expr, ResultVars, 0, _, !Problem),
            list.foldl2(unify_with_output, ResultVars, 0, _, !Problem)
        )
    else
        unexpected($module, $pred, "Imported pred")
    ).

:- pred build_cp_outputs(list(type_)::in, int::in,
    problem(type_position)::in, problem(type_position)::out,
    type_vars::in, type_vars::out) is det.

build_cp_outputs([], _, !Problem, !TypeVars).
build_cp_outputs([Out | Outs], ResNum, !Problem, !TypeVars) :-
    build_cp_type(Out, v_named(tp_output(ResNum)), !Problem, !TypeVars),
    build_cp_outputs(Outs, ResNum+1, !Problem, !TypeVars).

:- pred build_cp_inputs(list(type_)::in, list(varmap.var)::in,
    int::in, problem(type_position)::in, problem(type_position)::out,
    type_vars::in, type_vars::out,
    map(varmap.var, type_position)::in, map(varmap.var, type_position)::out)
    is det.

build_cp_inputs([], [], _, !Problem, !TypeVars, !Varmap).
build_cp_inputs([], [_ | _], _, _, _, _, _, _, _) :-
    unexpected($file, $pred, "Mismatched lists").
build_cp_inputs([_ | _], [], _, _, _, _, _, _, _) :-
    unexpected($file, $pred, "Mismatched lists").
build_cp_inputs([Type | Types], [Var | Vars], ParamNum, !Problem, !TypeVars,
        !Varmap) :-
    Position = tp_input(ParamNum),
    build_cp_type(Type, v_named(Position), !Problem, !TypeVars),
    det_insert(Var, Position, !Varmap),
    build_cp_inputs(Types, Vars, ParamNum + 1, !Problem, !TypeVars, !Varmap).

:- pred unify_with_output(type_position::in, int::in, int::out,
    problem(type_position)::in, problem(type_position)::out) is det.

unify_with_output(Var, !ResNum, !Problem) :-
    post_constraint_alias(v_named(Var), v_named(tp_output(!.ResNum)), !Problem),
    !:ResNum = !.ResNum + 1.

:- pred build_cp_expr(core::in, map(varmap.var, type_position)::in,
    expr::in, list(type_position)::out, int::in, int::out,
    problem(type_position)::in, problem(type_position)::out) is det.

build_cp_expr(Core, Varmap, expr(ExprType, _CodeInfo), Vars, !ExprNum,
        !Problem) :-
    ( ExprType = e_sequence(Exprs),
        map_foldl2(build_cp_expr(Core, Varmap), Exprs, Varss, !ExprNum,
            !Problem),
        ( if last(Varss, VarsPrime) then
            map_foldl2(build_cp_sequence_result(!.ExprNum), VarsPrime,
                Vars, 0, _, !Problem)
        else
            unexpected($file, $pred, "Sequence has no expressions")
        )
    ; ExprType = e_tuple(Exprs),
        map_foldl2(build_cp_expr(Core, Varmap), Exprs, ExprVars0, !ExprNum,
            !Problem),
        Vars = map(condense_tuple_type_var, ExprVars0),
        foldl2((pred(Var::in, RN0::in, RN::out, P0::in, P::out) is det :-
                post_constraint_alias(v_named(tp_expr(!.ExprNum, RN0)),
                    v_named(Var), P0, P),
                RN = RN0 + 1
            ), Vars, 0, _, !Problem)
    ; ExprType = e_let(LetVars, ExprLet, ExprIn),
        build_cp_expr(Core, Varmap, ExprLet, LetTypeVars, !ExprNum,
            !Problem),
        foldl_corresponding(det_insert, LetVars, LetTypeVars, Varmap,
            NextVarmap),
        build_cp_expr(Core, NextVarmap, ExprIn, Vars, !ExprNum,
            !Problem),
        foldl2((pred(Var::in, RN0::in, RN::out, P0::in, P::out) is det :-
                post_constraint_alias(v_named(tp_expr(!.ExprNum, RN0)),
                    v_named(Var), P0, P),
                RN = RN0 + 1
            ), Vars, 0, _, !Problem)
    ; ExprType = e_call(Callee, Args),
        map_foldl2(build_cp_expr(Core, Varmap), Args, ArgVars, !ExprNum,
            !Problem),
        ( if Callee = expr(e_func(FuncId), _) then
            core_get_function_det(Core, FuncId, Function),
            func_get_signature(Function, ParameterTypes, ResultTypes, _)
        else
            sorry($file, $pred, "Higher order call")
        ),
        unify_params(ParameterTypes, map(one_result, ArgVars), !Problem,
            init, TVarmap),
        map_foldl3(build_cp_result(!.ExprNum), ResultTypes, Vars, 0, _,
            !Problem, TVarmap, _)
    ; ExprType = e_var(ProgVar),
        ( if search(Varmap, ProgVar, SubVar) then
            Var = tp_expr(!.ExprNum, 0),
            post_constraint_alias(v_named(Var), v_named(SubVar), !Problem),
            Vars = [Var]
        else
            unexpected($file, $pred, "Unknown var")
        )
    ; ExprType = e_const(ConstType),
        ( ConstType = c_string(_),
            Type = builtin_type(string)
        ; ConstType = c_number(_),
            Type = builtin_type(int)
        ),
        Position = tp_expr(!.ExprNum, 0),
        Vars = [Position],
        build_cp_type(Type, v_named(Position), !Problem, init, _)
    ; ExprType = e_func(_),
        unexpected($file, $pred, "Function type")
    ),
    !:ExprNum = !.ExprNum + 1.

:- pred build_cp_sequence_result(int::in,
    type_position::in, type_position::out, int::in, int::out,
    problem(type_position)::in, problem(type_position)::out) is det.

build_cp_sequence_result(ExprNum, SubVar, Var, !ResNum, !Problem) :-
    Var = tp_expr(ExprNum, !.ResNum),
    !:ResNum = !.ResNum + 1,
    post_constraint_alias(v_named(SubVar), v_named(Var), !Problem).

:- func condense_tuple_type_var(list(type_position)) = type_position.

condense_tuple_type_var(List) = Var :-
    ( if List = [VarPrime] then
        Var = VarPrime
    else
        unexpected($file, $pred,
            format("Expression in tuple must have an arity of 1, got %d",
                [i(length(List))]))
    ).

:- pred unify_params(list(type_)::in, list(type_position)::in,
    problem(type_position)::in, problem(type_position)::out,
    type_vars::in, type_vars::out) is det.

unify_params([], [], !Problem, !TVarmap).
unify_params([], [_ | _], _, _, _, _) :-
    unexpected($file, $pred, "Number of args and parameters mismatch").
unify_params([_ | _], [], _, _, _, _) :-
    unexpected($file, $pred, "Number of args and parameters mismatch").
unify_params([PType | PTypes], [ArgVar | ArgVars], !Problem, !TVarmap) :-
    build_cp_type(PType, v_named(ArgVar), !Problem, !TVarmap),
    unify_params(PTypes, ArgVars, !Problem, !TVarmap).

:- pred build_cp_result(int::in, type_::in, type_position::out,
    int::in, int::out,
    problem(type_position)::in, problem(type_position)::out,
    type_vars::in, type_vars::out) is det.

build_cp_result(ExprNum, Type, Position, !ResNum, !Problem, !TVarmap) :-
    Position = tp_expr(ExprNum, !.ResNum),
    build_cp_type(Type, v_named(Position), !Problem, !TVarmap),
    !:ResNum = !.ResNum + 1.

%-----------------------------------------------------------------------%

:- pred build_cp_type(type_::in, solve.var(type_position)::in,
    problem(type_position)::in, problem(type_position)::out,
    type_vars::in, type_vars::out) is det.

build_cp_type(builtin_type(Builtin), Var, !Problem, !TVarmap) :-
    post_constraint_builtin(Var, Builtin, !Problem).
build_cp_type(type_variable(TVar), Var, !Problem, !TVarmap) :-
    ( if search(!.TVarmap, TVar, SolveVarPrime) then
        SolveVar = SolveVarPrime
    else
        new_variable(SolveVar, !Problem),
        det_insert(TVar, SolveVar, !TVarmap),
        % if this is in the declaration it must be unified with a value
        % saying that this type must remain abstract.
        % XXX: make this conditional
        post_constraint_abstract(SolveVar, TVar, !Problem)
    ),
    post_constraint_alias(Var, SolveVar, !Problem).
build_cp_type(type_(Symbol, Args), Var, !Problem, !TVarmap) :-
    map_foldl2(build_cp_type_arg, Args, ArgsVars, !Problem, !TVarmap),
    post_constraint_user_type(Var, Symbol, ArgsVars, !Problem).

:- pred build_cp_type_arg(type_::in, solve.var(type_position)::out,
    problem(type_position)::in, problem(type_position)::out,
    type_vars::in, type_vars::out) is det.

build_cp_type_arg(Type, Var, !Problem, !TVarmap) :-
    new_variable(Var, !Problem),
    build_cp_type(Type, Var, !Problem, !TVarmap).

%-----------------------------------------------------------------------%

:- pred update_types(map(type_position, type_)::in,
    set(func_id)::in, errors(compile_error)::out, core::in, core::out) is det.

update_types(TypeMap, SCC, Errors, !Core) :-
    ( if singleton_set(FuncId, SCC) then
        update_types_func(TypeMap, FuncId, Errors, !Core)
    else
        unexpected($file, $pred, "Mutual recursion")
    ).

:- pred update_types_func(map(type_position, type_)::in,
    func_id::in, errors(compile_error)::out, core::in, core::out) is det.

update_types_func(TypeMap, FuncId, Errors, !Core) :-
    some [!Func, !Expr] (
        core_get_function_det(!.Core, FuncId, !:Func),
        ( if func_get_body(!.Func, Varmap, Inputs, !:Expr) then
            update_types_expr(TypeMap, !Expr, 0, _),
            Errors = init, % XXX
            func_set_body(Varmap, Inputs, !.Expr, !Func)
        else
            unexpected($file, $pred, "imported pred")
        ),
        core_set_function(FuncId, !.Func, !Core)
    ).

:- pred update_types_expr(map(type_position, type_)::in,
    expr::in, expr::out, int::in, int::out) is det.

update_types_expr(TypeMap, !Expr, !ExprNum) :-
    !.Expr = expr(ExprType0, CodeInfo0),
    ( ExprType0 = e_sequence(Exprs0),
        map_foldl(update_types_expr(TypeMap), Exprs0, Exprs, !ExprNum),
        ExprType = e_sequence(Exprs)
    ; ExprType0 = e_tuple(Exprs0),
        map_foldl(update_types_expr(TypeMap), Exprs0, Exprs, !ExprNum),
        ExprType = e_tuple(Exprs)
    ; ExprType0 = e_let(LetVars, ExprLet0, ExprIn0),
        update_types_expr(TypeMap, ExprLet0, ExprLet, !ExprNum),
        update_types_expr(TypeMap, ExprIn0, ExprIn, !ExprNum),
        ExprType = e_let(LetVars, ExprLet, ExprIn)
    ; ExprType0 = e_call(FuncId, Args0),
        map_foldl(update_types_expr(TypeMap), Args0, Args, !ExprNum),
        ExprType = e_call(FuncId, Args)
    ;
        ( ExprType0 = e_var(_)
            % Here's where we need to hook to create a var->type map.
        ; ExprType0 = e_const(_)
        ; ExprType0 = e_func(_)
        ),
        ExprType = ExprType0
    ),
    Arity = code_info_get_arity(CodeInfo0),
    Types = get_result_types(TypeMap, !.ExprNum, Arity ^ a_num - 1),
    !:ExprNum = !.ExprNum + 1,
    code_info_set_types(Types, CodeInfo0, CodeInfo),
    !:Expr = expr(ExprType, CodeInfo).

:- func get_result_types(map(type_position, type_), int, int) = list(type_).

get_result_types(TypeMap, ExprNum, ResultNum) =
    ( if ResultNum < 0 then
        []
    else
        [lookup(TypeMap, tp_expr(ExprNum, ResultNum)) |
            get_result_types(TypeMap, ExprNum, ResultNum-1)]
    ).

%-----------------------------------------------------------------------%

:- type type_vars == map(type_var, var(type_position)).

:- func one_result(list(T)) = T.

one_result(Xs) =
    ( if Xs = [X] then
        X
    else
        unexpected($file, $pred, "arity error")
    ).

%-----------------------------------------------------------------------%
