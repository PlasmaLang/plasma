%-----------------------------------------------------------------------%
% Solver for typechecking/inference.
% vim: ts=4 sw=4 et
%
% Copyright (C) 2016 Plasma Team
% Distributed under the terms of the MIT see ../LICENSE.code
%
% This module implements a FD solver over types.
%
% Use MCFLAGS=--trace-flag typecheck_solve to trace this module.
%
%-----------------------------------------------------------------------%
:- module core.typecheck.solve.
%-----------------------------------------------------------------------%
:- interface.

% Typechecking requires solving a constraint problem.
%
% Each expression in the function is a constraint variable.
% Each type is a value, as a herbrand constraint.
%
% The variables representing the arguments of the function may remain
% unconstrained, they are polymorphic.  If there are no solutions then there
% is a type error.

:- import_module map.

:- type var(V)
    --->    v_named(V)
    ;       v_anon(int).

:- type problem(V).

:- func init = problem(V).

:- pred new_variable(var(V)::out, problem(V)::in, problem(V)::out) is det.

:- pred post_constraint_builtin(var(V)::in, builtin_type::in,
    problem(V)::in, problem(V)::out) is det.

:- pred post_constraint_alias(var(V)::in, var(V)::in,
    problem(V)::in, problem(V)::out) is det.

:- pred post_constraint_user_type(type_id::in, var(V)::in,
    problem(V)::in, problem(V)::out) is det.

    % Post the constraint that this variable has one of the given types.
    % In other words this is a disjunction.
    %
:- pred post_constraint_user_types(set(type_id)::in, var(V)::in,
    problem(V)::in, problem(V)::out) is det.

:- pred post_constraint_abstract(var(V)::in, type_var::in,
    problem(V)::in, problem(V)::out) is det.

    % post_constraint_match(V1, V2, !Problem)
    %
    % This constraint is a half-unification, or a pattern match,  V1 and V2
    % must be "unifiable", V1 will be updated to match V2, but V2 will not
    % be updated to match V1.  For example:
    %
    % f = X     => f = X
    % X = f     => f = f
    %
    % This is used to make an argument's type (V1) match the parameter
    % type (V2) without constraining the parameter type.
    %
:- pred post_constraint_match(var(V)::in, var(V)::in,
    problem(V)::in, problem(V)::out) is det.

:- pred solve(problem(V)::in, map(V, type_)::out) is det.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
:- implementation.

:- import_module io.
:- import_module string.

:- import_module util.

:- type problem(V)
    --->    problem(
                p_vars          :: set(var(V)),
                p_domains       :: map(var(V), domain),
                p_propagators   :: map(var(V), set(propagator(V)))
            ).

:- type constraint(V)
    --->    c_alias(
                ca_var1         :: var(V),
                ca_var2         :: var(V)
            )
    ;       c_types(
                ct_var          :: var(V),
                ct_types        :: set(type_id)
            ).

:- type propagator(V)
    --->    propagator(constraint(V)).

%-----------------------------------------------------------------------%

init = problem(init, init, init).

%-----------------------------------------------------------------------%

solve(!.Problem, Solution) :-
    % XXX: This won't be 100% correct once we allow other kinds of domains.
    % At the very least we need to document which domains get setup
    % initially and which get setup by propagation.
    SetVars = list_to_set(keys(!.Problem ^ p_domains)),
    FreeVars = difference(!.Problem ^ p_vars, SetVars),
    % Start by running all the propagators.
    QueuedVars = list_to_set(keys(!.Problem ^ p_propagators)),
    solve_loop(FreeVars, QueuedVars, !Problem),
    foldl(build_results, !.Problem ^ p_domains, init, Solution).

:- pred solve_loop(set(var(V))::in, set(var(V))::in,
    problem(V)::in, problem(V)::out) is det.

solve_loop(FreeVars0, PropQueuedVars, !Problem) :-
    ( if is_empty(FreeVars0) then
        true
    else
        run_propagators(PropQueuedVars, FreeVars0, FreeVars1, !Problem),
        ( if remove_least(_Var, FreeVars1, _FreeVars) then
            util.sorry($file, $pred, "Nondet type checking")
        else
            true
        )
    ).

:- pred run_propagators(set(var(V))::in, set(var(V))::in, set(var(V))::out,
    problem(V)::in, problem(V)::out) is det.

run_propagators(!.QueueVars, !FreeVars, !Problem) :-
    ( if remove_least(Var, !QueueVars) then
        trace [io(!IO), compile_time(flag("typecheck_solve"))] (
            io.format("Propagating from %s:\n", [s(string(Var))], !IO)
        ),
        Propagators = propagators(!.Problem, Var),
        foldl3(propagate(Var), Propagators, !QueueVars, !FreeVars, !Problem),
        run_propagators(!.QueueVars, !FreeVars, !Problem)
    else
        true
    ).

:- pred propagate(var(V)::in, propagator(V)::in,
    set(var(V))::in, set(var(V))::out, set(var(V))::in, set(var(V))::out,
    problem(V)::in, problem(V)::out) is det.

propagate(_Var, P, !QueueVars, !FreeVars, !Problem) :-
    propagator(C) = P,
    Domains = !.Problem ^ p_domains,

    ( C = c_alias(LHS, RHS),
        DomLHS = get_domain(Domains, LHS),
        DomRHS = get_domain(Domains, RHS),
        ( if
            update_domain(LHS, DomRHS, NewLHS, UpdatedLHS, !Problem),
            update_domain(RHS, DomLHS, NewRHS, UpdatedRHS, !Problem)
        then
            ( UpdatedLHS = updated,
                insert(LHS, !QueueVars),
                set_remove_det(LHS, !FreeVars),
                trace [io(!IO), compile_time(flag("typecheck_solve"))] (
                    io.format("  %s := %s\n",
                        [s(string(LHS)), s(string(NewLHS))], !IO)
                )
            ; UpdatedLHS = not_updated
            ),
            ( UpdatedRHS = updated,
                insert(RHS, !QueueVars),
                set_remove_det(RHS, !FreeVars),
                trace [io(!IO), compile_time(flag("typecheck_solve"))] (
                    io.format("  %s := %s\n",
                        [s(string(RHS)), s(string(NewRHS))], !IO)
                )
            ; UpdatedRHS = not_updated
            )
        else
            compile_error($file, $pred, "Typechecking failed")
        )
    ; C = c_types(Var, Types),
        ( if is_empty(Types) then
            compile_error($file, $pred, "Typechecking failed")
        else
            Dom = get_domain(Domains, Var),
            ( Dom = d_type(Type),
                ( if member(Type, Types) then
                    true
                else
                    compile_error($file, $pred, "Typechecking failed")
                )
            ; Dom = d_builtin(_),
                compile_error($file, $pred, "Typechecking failed")
            ; Dom = d_free,
                ( if singleton_set(Type, Types) then
                    ( if
                        update_domain(Var, d_type(Type), NewDom, Updated,
                        !Problem)
                    then
                        ( Updated = updated,
                            insert(Var, !QueueVars),
                            set_remove_det(Var, !FreeVars),
                            trace [io(!IO),
                                    compile_time(flag("typecheck_solve"))] (
                                io.format("  %s := %s\n",
                                    [s(string(Var)), s(string(NewDom))], !IO)
                            )
                        ; Updated = not_updated
                        )
                    else
                        compile_error($file, $pred, "Typechecking failed")
                    )
                else
                    true
                    % TODO: Track more precise domains
                )
            )
        )
    ).

:- pred build_results(var(V)::in, domain::in,
    map(V, type_)::in, map(V, type_)::out) is det.

build_results(v_anon(_), _, !Results).
build_results(v_named(V), Domain, !Results) :-
    ( Domain = d_free,
        unexpected($file, $pred, "Free variable")
    ; Domain = d_builtin(Builtin),
        Type = builtin_type(Builtin)
    ; Domain = d_type(TypeId),
        Type = type_ref(TypeId)
    ),
    det_insert(V, Type, !Results).

%-----------------------------------------------------------------------%

:- func propagators(problem(V), var(V)) = list(propagator(V)).

propagators(Problem, Var) =
    ( if map.search(Problem ^ p_propagators, Var, Props) then
        to_sorted_list(Props)
    else
        []
    ).

:- func get_domain(map(var(V), domain), var(V)) = domain.

get_domain(Map, Var) =
    ( if map.search(Map, Var, Domain) then
        Domain
    else
        d_free
    ).

%-----------------------------------------------------------------------%

post_constraint_builtin(Var, Builtin, !Problem) :-
    trace [io(!IO), compile_time(flag("typecheck_solve"))] (
        io.format("Post: %s = %s\n",
            [s(string(Var)), s(string(Builtin))], !IO)
    ),
    ( if update_domain(Var, d_builtin(Builtin), _, Updated, !Problem)
    then
        ( Updated = updated,
            add_var(Var, !Problem)
        ; Updated = not_updated
        )
    else
        compile_error($file, $pred, "Typechecking failure")
    ).

post_constraint_alias(Var1, Var2, !Problem) :-
    trace [io(!IO), compile_time(flag("typecheck_solve"))] (
        io.format("Post: %s = %s\n",
            [s(string(Var1)), s(string(Var2))], !IO)
    ),
    C = c_alias(Var1, Var2),
    add_var(Var1, !Problem),
    add_var(Var2, !Problem),
    add_propagator(Var1, C, !Problem),
    add_propagator(Var2, C, !Problem).

post_constraint_user_types(Types, Var, !Problem) :-
    trace [io(!IO), compile_time(flag("typecheck_solve"))] (
        io.format("Post: %s = %s\n",
            [s(string(Var)), s(string(Types))], !IO)
    ),
    C = c_types(Var, Types),
    add_var(Var, !Problem),
    add_propagator(Var, C, !Problem).

post_constraint_user_type(Type, Var, !Problem) :-
    trace [io(!IO), compile_time(flag("typecheck_solve"))] (
        io.format("Post: %s = %s\n",
            [s(string(Var)), s(string(Type))], !IO)
    ),
    C = c_types(Var, make_singleton_set(Type)),
    add_var(Var, !Problem),
    add_propagator(Var, C, !Problem).

%-----------------------------------------------------------------------%

:- pred add_propagator(var(V)::in, constraint(V)::in, problem(V)::in,
    problem(V)::out) is det.

add_propagator(Var, C, !Problem) :-
    some [!Propagators] (
        !:Propagators = !.Problem ^ p_propagators,
        ( if search(!.Propagators, Var, Props0) then
            insert(propagator(C), Props0, Props),
            set(Var, Props, !Propagators)
        else
            set(Var, make_singleton_set(propagator(C)), !Propagators)
        ),
        !Problem ^ p_propagators := !.Propagators
    ).

:- pred add_var(var(V)::in, problem(V)::in, problem(V)::out) is det.

add_var(Var, !Problem) :-
    Vars0 = !.Problem ^ p_vars,
    insert(Var, Vars0, Vars),
    !Problem ^ p_vars := Vars.

%-----------------------------------------------------------------------%

:- type domain
    --->    d_free
    ;       d_builtin(builtin_type)
    ;       d_type(type_id).

:- type updated
    --->    updated
    ;       not_updated.

:- pred update_domain(var(V)::in, domain::in, domain::out,
    updated::out, problem(V)::in, problem(V)::out) is semidet.

update_domain(Var, UnifyDomain, NewDomain, Updated, !Problem) :-
    Domains0 = !.Problem ^ p_domains,
    ( if search(Domains0, Var, OldDomain) then
        unify_domains(OldDomain, UnifyDomain, NewDomain),
        ( if OldDomain \= NewDomain then
            Updated = updated,
            set(Var, NewDomain, Domains0, Domains),
            !Problem ^ p_domains := Domains
        else
            Updated = not_updated
        )
    else
        NewDomain = UnifyDomain,
        det_insert(Var, NewDomain, Domains0, Domains),
        !Problem ^ p_domains := Domains,
        ( if NewDomain = d_free then
            Updated = not_updated
        else
            Updated = updated
        )
    ).

:- pred unify_domains(domain::in, domain::in, domain::out) is semidet.

unify_domains(D1, D2, D) :-
    require_complete_switch [D1]
    ( D1 = d_free,
        ( if D2 = d_free then
            D = d_free
        else
            D = D2
        )
    ; D1 = d_builtin(B1),
        require_complete_switch [D2]
        ( D2 = d_free,
            D = D1
        ; D2 = d_builtin(B2),
            B1 = B2,
            D = d_builtin(B1)
        ; D2 = d_type(_),
            false
        )
    ; D1 = d_type(Type1),
        require_complete_switch [D2]
        ( D2 = d_free,
            D = D1
        ; D2 = d_builtin(_),
            false
        ; D2 = d_type(Type2),
            Type1 = Type2,
            D = D1
        )
    ).

:- pred is_free(map(var(V), domain)::in, var(V)::in) is semidet.

%-----------------------------------------------------------------------%

:- pred set_remove_det(T::in, set(T)::in, set(T)::out) is det.

set_remove_det(X, !Set) :-
    ( if remove(X, !Set) then
        true
    else
        unexpected($file, $pred, "Element not found")
    ).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
