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

%-----------------------------------------------------------------------%

:- type var(V)
    --->    v_named(V)
    ;       v_anon(int).

:- type problem(V).

:- func init = problem(V).

:- pred new_variable(var(V)::out, problem(V)::in, problem(V)::out) is det.

:- pred post_constraint(constraint(V)::in, problem(V)::in, problem(V)::out)
    is det.

%-----------------------------------------------------------------------%

% Constraints are boolean expressions.  Literals and their conjunctions and
% disjunctions.  Although an individual constraint is never more complex
% than disjunctive normal form.

:- type constraint_literal(V)
    --->    cl_true
    ;       cl_var_builtin(var(V), builtin_type)
    ;       cl_var_usertype(var(V), type_id)
    ;       cl_var_var(var(V), var(V)).

:- type constraint(V).

:- func make_constraint(constraint_literal(V)) = constraint(V).

    % Make the constraint that this variable has one of the given types.
    % In other words this is a disjunction.
    %
:- func make_constraint_user_types(set(type_id), var(V)) = constraint(V).

:- func make_conjunction(list(constraint_literal(V))) = constraint(V).

:- func make_disjunction(list(constraint(V))) = constraint(V).

%:- pred post_constraint_abstract(var(V)::in, type_var::in,
%    problem(V)::in, problem(V)::out) is det.
%
%    % post_constraint_match(V1, V2, !Problem)
%    %
%    % This constraint is a half-unification, or a pattern match,  V1 and V2
%    % must be "unifiable", V1 will be updated to match V2, but V2 will not
%    % be updated to match V1.  For example:
%    %
%    % f = X     => f = X
%    % X = f     => f = f
%    %
%    % This is used to make an argument's type (V1) match the parameter
%    % type (V2) without constraining the parameter type.
%    %
%:- pred post_constraint_match(var(V)::in, var(V)::in,
%    problem(V)::in, problem(V)::out) is det.

:- pred solve(problem(V)::in, map(V, type_)::out) is det.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
:- implementation.

:- import_module io.
:- import_module string.

:- import_module util.

:- type problem(V)
    --->    problem(
                p_next_anon_var :: int,
                % All the constraints in CNF form.
                p_constraints   :: constraint(V)
            ).

init = problem(0, conj([])).

new_variable(v_anon(Var), !Problem) :-
    Var = !.Problem ^ p_next_anon_var,
    !Problem ^ p_next_anon_var := Var + 1.

%-----------------------------------------------------------------------%

    % Constraints are kept in CNF form.
    %
:- type constraint(V)
    --->    conj(list(clause(V))).

:- type clause(V)
    --->    disj(list(constraint_literal(V))).

make_constraint(Lit) = conj([disj([Lit])]).

make_conjunction(Literals0) = conj(Conj) :-
    % Remove any true literals, they cannot affect the conjunction.
    Literals1 = filter((pred(L::in) is semidet :- L \= cl_true), Literals0),
    Literals = sort_and_remove_dups(Literals1),
    Conj = map(func(L) = disj([L]), Literals).

    % Perform make_disjunction by by algebraicly manipulating the equation.
    %
    % It is in the form (A /\ B) v (C /\ D)
    %
    % We need to take each literal in the first clause, and factor it into
    % every other cluase. Resulting in:
    %
    % (A v C) /\ (A v D) /\ (B v C) /\ (B v D)
    %
make_disjunction(Disjs) = conj(make_disjunction_2(Disjs)).

:- func make_disjunction_2(list(constraint(V))) = list(clause(V)).

make_disjunction_2([]) = [disj([])].
make_disjunction_2([conj(Cs) | Ds]) =
    condense(map((func(C) = make_disjunction_3(C, make_disjunction_2(Ds))),
        Cs)).

:- func make_disjunction_3(clause(V), list(clause(V))) = list(clause(V)).

make_disjunction_3(D, []) = [D].
make_disjunction_3(D, [C0 | Cs0]) = [C | Cs] :-
    D = disj(LitsA),
    C0 = disj(LitsB),
    C = disj(LitsA ++ LitsB),
    Cs = make_disjunction_3(D, Cs0).

post_constraint(conj(ConjsA), !Problem) :-
    conj(ConjsB) = !.Problem ^ p_constraints,
    Conjs = merge_and_remove_dups(ConjsA, ConjsB),
    !Problem ^ p_constraints := conj(Conjs).

:- func constraint_vars(constraint(V)) = set(var(V)).

constraint_vars(conj(Clauses)) = union_list(map(clause_vars, Clauses)).

:- func clause_vars(clause(V)) = set(var(V)).

clause_vars(disj(Lits)) = union_list(map(literal_vars, Lits)).

:- func literal_vars(constraint_literal(V)) = set(var(V)).

literal_vars(cl_true) = init.
literal_vars(cl_var_builtin(Var, _)) = make_singleton_set(Var).
literal_vars(cl_var_usertype(Var, _)) = make_singleton_set(Var).
literal_vars(cl_var_var(VarA, VarB)) = from_list([VarA, VarB]).

%-----------------------------------------------------------------------%

solve(problem(_, Constraints), Solution) :-
    some [!Problem] (
        AllVars = constraint_vars(Constraints),
        !:Problem = problem(AllVars, init, init),
        Constraints = conj(Clauses),
        run_clauses(Clauses, !Problem),

        foldl(build_results, !.Problem ^ p_domains, init, Solution)
    ).

%-----------------------------------------------------------------------%

:- type problem_solving(V)
    --->    problem(
                p_vars          :: set(var(V)),
                p_domains       :: map(var(V), domain),
                p_propagators   :: map(var(V), set(propagator(V)))
            ).

:- type propagator(V)
    --->    propagator(constraint(V)).

:- pred run_clauses(list(clause(V))::in,
    problem_solving(V)::in, problem_solving(V)::out) is det.

run_clauses(Clauses, !Problem) :-
    run_clauses(Clauses, [], length(Clauses), !Problem).

    % Run the clauses until we can't make any further progress.
    %
:- pred run_clauses(list(clause(V))::in, list(clause(V))::in, int::in,
    problem_solving(V)::in, problem_solving(V)::out) is det.

run_clauses([], [], _, !Problem).
run_clauses([], Cs@[_ | _], OldLen, !Problem) :-
    Len = length(Cs),
    ( if Len < OldLen then
        % Before running the delayed clauses we check to see if we are
        % indeed making progress.
        run_clauses(Cs, [], Len, !Problem)
    else
        util.sorry($file, $pred,
            format("Floundering %d:%d: %s", [i(Len), i(OldLen), s(string(Cs))]))
    ).
run_clauses([C | Cs], Delays0, ProgressCheck, !Problem) :-
    run_clause(C, Delays0, Delays, !Problem),
    run_clauses(Cs, Delays, ProgressCheck, !Problem).

:- pred run_clause(clause(V)::in, list(clause(V))::in, list(clause(V))::out,
    problem_solving(V)::in, problem_solving(V)::out) is det.

run_clause(disj(Lits), !Delays, !Problem) :-
    ( Lits = [],
        util.compile_error($file, $pred,
            "Typechecking failed, empty disjunction")
    ; Lits = [Lit],
        run_literal(Lit, Delayed, _, !Problem)
    ; Lits = [_, _ | _],
        run_disj(Lits, Delayed, !Problem)
    ),
    ( Delayed = succeeded
    ; Delayed = failed,
        util.compile_error($file, $pred, "Typechecking failed")
    ; Delayed = delayed,
        !:Delays = [disj(Lits) | !.Delays]
    ).

    % A disjunction normally needs at least one literal to be true for the
    % disjunction to be true.  However for typechecking we want to find a
    % unique solution to the type problem, therefore we need _exacty one_
    % literal to be true.
    %
    % This will not implement choice points, and will only execute
    % disjunctions that we know will not generate choices.  If a disjunction
    % would generate a choice then it will be delayed and hopefully executed
    % later.
    %
    % This is broken into two stages, first iterate over the literals until
    % we find the first true one, then iterate over the remaining literals
    % to ensure they're all false.  If we find need to update the problem,
    % then delay.
    %
    % TODO: If an item is the last one and it would update the problem and
    % is true, then allow it to execute.
    %
:- pred run_disj(list(constraint_literal(V))::in, executed::out,
    problem_solving(V)::in, problem_solving(V)::out) is det.

run_disj(Disjs, Delayed, !Problem) :-
    run_disj(Disjs, no, Delayed, !Problem).

:- pred run_disj(list(constraint_literal(V))::in,
    maybe(constraint_literal(V))::in, executed::out,
    problem_solving(V)::in, problem_solving(V)::out) is det.

run_disj([], MaybeDelayed, Succeeded, !Problem) :-
    run_disj_last(MaybeDelayed, Succeeded, !Problem).
run_disj([Lit | Lits], MaybeDelayed, Succeeded, Problem0, Problem) :-
    run_literal(Lit, Succeeded0, Updated, Problem0, Problem1),
    ( Updated = updated,
        Succeeded1 = delayed
    ; Updated = not_updated,
        Succeeded1 = Succeeded0
    ),
    ( Succeeded1 = delayed,
        ( MaybeDelayed = yes(_),
            Succeeded = delayed,
            Problem = Problem0
        ; MaybeDelayed = no,
            run_disj(Lits, yes(Lit), Succeeded, Problem0, Problem)
        )
    ; Succeeded1 = succeeded,
        % Switch to checking that the remaining items are false.
        run_disj_all_false(Lits, MaybeDelayed, Problem1, Succeeded),
        Problem = Problem1
    ; Succeeded1 = failed,
        run_disj(Lits, MaybeDelayed, Succeeded, Problem1, Problem)
    ).

:- pred run_disj_all_false(list(constraint_literal(V))::in,
    maybe(constraint_literal(V))::in, problem_solving(V)::in,
    executed::out) is det.

run_disj_all_false([], MaybeDelayed, Problem, Succeeded) :-
    run_disj_last(MaybeDelayed, Succeeded0, Problem, _),
    invert_success(Succeeded0, Succeeded).
run_disj_all_false([Lit | Lits], MaybeDelayed, Problem, Succeeded) :-
    run_literal(Lit, Succeeded0, Updated, Problem, _),
    ( Updated = updated,
        Succeeded1 = delayed
    ; Updated = not_updated,
        Succeeded1 = Succeeded0
    ),
    ( Succeeded1 = delayed,
        ( MaybeDelayed = yes(_),
            Succeeded = delayed
        ; MaybeDelayed = no,
            run_disj_all_false(Lits, yes(Lit), Problem, Succeeded)
        )
    ; Succeeded1 = succeeded,
        Succeeded = failed
    ; Succeeded1 = failed,
        run_disj_all_false(Lits, MaybeDelayed, Problem, Succeeded)
    ).

:- pred invert_success(executed, executed).
:- mode invert_success(in, out) is det.

invert_success(delayed,   delayed).
invert_success(succeeded, failed).
invert_success(failed,    succeeded).

:- pred run_disj_last(maybe(constraint_literal(V))::in, executed::out,
    problem_solving(V)::in, problem_solving(V)::out) is det.

run_disj_last(no, failed, !Problem).
run_disj_last(yes(Lit), Succeeded, Problem0, Problem) :-
    run_literal(Lit, Succeeded0, _, Problem0, Problem1),
    (
        ( Succeeded0 = delayed
        ; Succeeded0 = failed
        ),
        Problem = Problem0,
        Succeeded = delayed
    ;
        Succeeded0 = succeeded,
        Problem = Problem1,
        Succeeded = Succeeded0
    ).

:- type executed
    --->    succeeded
    ;       failed
    ;       delayed.

    % Run the literal immediately.  Directly update domains and add
    % propagators.
    %
:- pred run_literal(constraint_literal(V)::in, executed::out,
    updated::out, problem_solving(V)::in, problem_solving(V)::out) is det.

run_literal(cl_true, succeeded, not_updated, !Problem).
run_literal(cl_var_builtin(Var, Builtin), Succeeded, Updated, !Problem) :-
    trace [io(!IO), compile_time(flag("typecheck_solve"))] (
        io.format("Run: %s = %s\n",
            [s(string(Var)), s(string(Builtin))], !IO)
    ),
    ( if update_domain(Var, d_builtin(Builtin), _, UpdatedP, !Problem) then
        Succeeded = succeeded,
        Updated = UpdatedP
    else
        trace [io(!IO), compile_time(flag("typecheck_solve"))] (
            io.write_string("..failed\n", !IO)
        ),
        Succeeded = failed,
        Updated = not_updated
    ).
run_literal(cl_var_var(Var1, Var2), Succeeded, Updated, !Problem) :-
    trace [io(!IO), compile_time(flag("typecheck_solve"))] (
        io.format("Run: %s = %s\n",
            [s(string(Var1)), s(string(Var2))], !IO)
    ),
    DomainMap0 = !.Problem ^ p_domains,
    Dom1 = get_domain(DomainMap0, Var1),
    Dom2 = get_domain(DomainMap0, Var2),
    ( Dom1 = d_free,
        ( Dom2 = d_free,
            Dom = d_free,
            Updated = not_updated
        ;
            ( Dom2 = d_builtin(_)
            ; Dom2 = d_type(_)
            ),
            Dom = Dom2,
            Updated = updated
        )
    ; Dom1 = d_builtin(Builtin1),
        ( Dom2 = d_free,
            Dom = Dom1,
            Updated = updated
        ; Dom2 = d_builtin(Builtin2),
            ( if Builtin1 = Builtin2 then
                Dom = Dom1,
                Updated = not_updated
            else
                util.compile_error($file, $pred, "Type error")
            )
        ; Dom2 = d_type(_),
            util.compile_error($file, $pred, "Type error")
        )
    ; Dom1 = d_type(Type1),
        ( Dom2 = d_free,
            Dom = Dom1,
            Updated = updated
        ; Dom2 = d_builtin(_),
            util.compile_error($file, $pred, "Type error")
        ; Dom2 = d_type(Type2),
            ( if Type1 = Type2 then
                Dom = Dom1,
                Updated = not_updated
            else
                util.compile_error($file, $pred, "Type error")
            )
        )
    ),
    ( Dom = d_free,
        Succeeded = delayed,
        trace [io(!IO), compile_time(flag("typecheck_solve"))] (
            io.write_string("..delayed.\n", !IO)
        )
    ;
        ( Dom = d_type(_)
        ; Dom = d_builtin(_)
        ),
        Succeeded = succeeded
    ),
    ( Updated = updated,
        set(Var1, Dom, DomainMap0, DomainMap1),
        set(Var2, Dom, DomainMap1, DomainMap),
        !Problem ^ p_domains := DomainMap
    ; Updated = not_updated
    ).
run_literal(cl_var_usertype(Var, Type), Succeeded, Updated, !Problem) :-
    trace [io(!IO), compile_time(flag("typecheck_solve"))] (
        io.format("Run: %s = %s\n",
            [s(string(Var)), s(string(Type))], !IO)
    ),
    ( if update_domain(Var, d_type(Type), _, UpdatedP, !Problem) then
        Succeeded = succeeded,
        Updated = UpdatedP
    else
        trace [io(!IO), compile_time(flag("typecheck_solve"))] (
            io.write_string("..failed.\n", !IO)
        ),
        Succeeded = failed,
        Updated = not_updated
    ).

%-----------------------------------------------------------------------%

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

:- func get_domain(map(var(V), domain), var(V)) = domain.

get_domain(Map, Var) =
    ( if map.search(Map, Var, Domain) then
        Domain
    else
        d_free
    ).

%-----------------------------------------------------------------------%

:- type domain
    --->    d_free
    ;       d_builtin(builtin_type)
    ;       d_type(type_id).

:- type updated
    --->    updated
    ;       not_updated.

:- pred update_domain(var(V)::in, domain::in, domain::out,
    updated::out, problem_solving(V)::in, problem_solving(V)::out) is semidet.

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

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
