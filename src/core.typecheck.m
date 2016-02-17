%-----------------------------------------------------------------------%
% Plasma typechecking
% vim: ts=4 sw=4 et
%
% Copyright (C) 2015-2016 Plasma Team
% Distributed under the terms of the MIT see ../LICENSE.code
%
% This module typechecks plasma core.
%
% Use MCFLAGS=--trace-flag typecheck to trace this module.
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

:- import_module cord.
:- import_module map.

%-----------------------------------------------------------------------%

typecheck(Errors, !Core) :-
    SCCs = core_all_functions_sccs(!.Core),
    map_foldl(typecheck_scc, SCCs, ErrorsList, !Core),
    Errors = cord_list_to_cord(ErrorsList).

:- pred typecheck_scc(set(func_id)::in, errors(compile_error)::out,
    core::in, core::out) is det.

typecheck_scc(SCC, Errors, !Core) :-
    % The first step is to compute the arity of each expression.
    compute_arity(SCC, ArityErrors, !Core),
    ( if is_empty(ArityErrors) then
        % Now do the real typechecking.
        Errors = init
    else
        Errors = ArityErrors
    ).

%-----------------------------------------------------------------------%

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
    ; ExprType0 = e_call(CalleeId, Args0),
        compute_arity_expr_list(Core, ArgsResult, Args0, Args),
        ExprType = e_call(CalleeId, Args),
        core_get_function_det(Core, CalleeId, Callee),
        func_get_signature(Callee, Inputs, _, Arity),
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

% Typechecking requires solving a contraint problem.
%
% Each expression in the function is a constraint variable.
% Each type is a value, as a herbrand constraint.
%
% The variables representing the arguments of the function may remain
% unconstrained, they are polymorphic.  If there are no solutions then there
% is a type error.

:- type type_problem(V)
    --->    type_problem(
                tp_variables        :: map(V, domain)
            ).

:- type domain
    --->    domain.

%-----------------------------------------------------------------------%
