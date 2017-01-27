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
% than disjunctive normal form.  This representation and algorithm is
% simple.  But it may be too simple to handle generics well and it is
% definitly to simple to give useful/helpful type errors.

:- type constraint_literal(V)
    --->    cl_true
    ;       cl_var_builtin(var(V), builtin_type)
    ;       cl_var_usertype(var(V), type_id)
    ;       cl_var_var(var(V), var(V), context).

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

%-----------------------------------------------------------------------%

make_constraint(Lit) = conj([disj([Lit])]).

%-----------------------------------------------------------------------%

make_conjunction(Literals0) = conj(Conj) :-
    % Remove any true literals, they cannot affect the conjunction.
    Literals1 = filter((pred(L::in) is semidet :- L \= cl_true), Literals0),
    Literals = sort_and_remove_dups(Literals1),
    Conj = map(func(L) = disj([L]), Literals).

%-----------------------------------------------------------------------%

    % Perform make_disjunction by by algebraicly manipulating the equation.
    %
make_disjunction([]) = conj([disj([])]).
make_disjunction([D | []]) = D.
make_disjunction([D | Ds@[_ | _]]) =
    list.foldl(make_disjunction_2, Ds, D).

    % This is a pairwise creation of a disjunction.
    %
    % It is in the form (A1 /\ A2 /\ ...) v (B1 /\ B2 /\ ...)
    %
    % We take each literal in the first clause, and factor it into the
    % second clause. Resulting in:
    %
    % (A1 v B1) /\ (A1 v B2) /\ (A2 v B1) /\ (A2 v B2)
    %
:- func make_disjunction_2(constraint(V), constraint(V)) =
    constraint(V).

make_disjunction_2(conj(ConjsA), conj(ConjsB)) = conj(Conjs) :-
    % The outer loop, loop over ConjsB.
    Conjs = condense(map(make_disjunction_clauses(ConjsA), ConjsB)).

:- func make_disjunction_clauses(list(clause(V)), clause(V)) =
    list(clause(V)).

make_disjunction_clauses(Clauses, Clause) =
    map(make_disjunction_clause(Clause), Clauses).

:- func make_disjunction_clause(clause(V), clause(V)) = clause(V).

make_disjunction_clause(disj(Ds1), disj(Ds2)) = disj(Ds1 ++ Ds2).

%-----------------------------------------------------------------------%

post_constraint(conj(ConjsA), !Problem) :-
    conj(ConjsB) = !.Problem ^ p_constraints,
    Conjs = merge_and_remove_dups(ConjsA, ConjsB),
    !Problem ^ p_constraints := conj(Conjs).

%-----------------------------------------------------------------------%

:- func constraint_vars(constraint(V)) = set(var(V)).

constraint_vars(conj(Clauses)) = union_list(map(clause_vars, Clauses)).

:- func clause_vars(clause(V)) = set(var(V)).

clause_vars(disj(Lits)) = union_list(map(literal_vars, Lits)).

:- func literal_vars(constraint_literal(V)) = set(var(V)).

literal_vars(cl_true) = init.
literal_vars(cl_var_builtin(Var, _)) = make_singleton_set(Var).
literal_vars(cl_var_usertype(Var, _)) = make_singleton_set(Var).
literal_vars(cl_var_var(VarA, VarB, _)) = from_list([VarA, VarB]).

%-----------------------------------------------------------------------%

:- import_module pretty_utils.

:- func pretty_problem(constraint(V)) = cord(string).

pretty_problem(conj(Conjs)) = join(nl, map(pretty_clause, Conjs)).

:- func pretty_clause(clause(V)) = cord(string).

pretty_clause(disj(Disjs)) = singleton("Clause:") ++
    cord_list_to_cord(map((func(L) = line(2) ++ pretty_literal(L)), Disjs)).

:- func pretty_literal(constraint_literal(V)) = cord(string).

pretty_literal(cl_true) = singleton("true").
pretty_literal(cl_var_builtin(Var, Builtin)) = cord_string(Var) ++ spceqspc
    ++ cord_string(Builtin).
pretty_literal(cl_var_usertype(Var, Usertype)) = cord_string(Var) ++ spceqspc
    ++ cord_string(Usertype).
pretty_literal(cl_var_var(Var1, Var2, _)) = cord_string(Var1) ++ spceqspc
    ++ cord_string(Var2).

:- func spceqspc = cord(string).
spceqspc = singleton(" = ").

:- func cord_string(T) = cord(string).
cord_string(X) = singleton(string(X)).

%-----------------------------------------------------------------------%

solve(problem(_, Constraints), Solution) :-
    trace [io(!IO), compile_time(flag("typecheck_solve"))] (
        write_string("Typecheck solver starting\n", !IO),
        PrettyProblem = pretty_problem(Constraints),
        write_string(append_list(list(PrettyProblem)), !IO),
        nl(!IO)
    ),
    AllVars = constraint_vars(Constraints),
    Problem0 = problem(AllVars, init, init),
    Constraints = conj(Clauses),
    run_clauses(Clauses, Problem0, Result),
    ( Result = ok(Problem),
        foldl(build_results, Problem ^ p_domains, init, Solution)
    ; Result = failed(Reason),
        compile_error($module, $pred, "Typechecking failed: " ++ Reason)
    ).

%-----------------------------------------------------------------------%

:- type problem_solving(V)
    --->    problem(
                p_vars          :: set(var(V)),
                p_domains       :: map(var(V), domain),
                p_propagators   :: map(var(V), set(propagator(V)))
            ).

:- type problem_result(V)
    --->    ok(problem_solving(V))
    ;       failed(string).

:- type propagator(V)
    --->    propagator(constraint(V)).

:- pred run_clauses(list(clause(V))::in,
    problem_solving(V)::in, problem_result(V)::out) is det.

run_clauses(Clauses, Problem, Result) :-
    run_clauses(Clauses, [], length(Clauses), Problem, Result).

    % Run the clauses until we can't make any further progress.
    %
:- pred run_clauses(list(clause(V))::in, list(clause(V))::in, int::in,
    problem_solving(V)::in, problem_result(V)::out) is det.

run_clauses([], [], _, Problem, ok(Problem)).
run_clauses([], Cs@[_ | _], OldLen, Problem, Result) :-
    Len = length(Cs),
    ( if Len < OldLen then
        % Before running the delayed clauses we check to see if we are
        % indeed making progress.
        run_clauses(Cs, [], Len, Problem, Result)
    else
        util.sorry($file, $pred,
            format("Floundering %d:%d: %s", [i(Len), i(OldLen), s(string(Cs))]))
    ).
run_clauses([C | Cs], Delays0, ProgressCheck, !.Problem, Result) :-
    run_clause(C, Delays0, Delays, !.Problem, ClauseResult),
    ( ClauseResult = ok(!:Problem),
        run_clauses(Cs, Delays, ProgressCheck, !.Problem, Result)
    ; ClauseResult = failed(_),
        Result = ClauseResult
    ).

:- pred run_clause(clause(V)::in, list(clause(V))::in, list(clause(V))::out,
    problem_solving(V)::in, problem_result(V)::out) is det.

run_clause(disj(Lits), !Delays, !.Problem, Result) :-
    ( Lits = [],
        util.compile_error($file, $pred,
            "Typechecking failed, empty disjunction")
    ; Lits = [Lit],
        run_literal(Lit, Success, !Problem)
    ; Lits = [_, _ | _],
        run_disj(Lits, Success, !Problem)
    ),
    (
        ( Success = success_updated
        ; Success = success_not_updated
        ),
        Result = ok(!.Problem)
    ; Success = failed(Reason),
        Result = failed(Reason)
    ; Success = delayed,
        !:Delays = [disj(Lits) | !.Delays],
        Result = ok(!.Problem)
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

run_disj([], no, failed("all disjuncts failed"), !Problem).
run_disj([], yes(Lit), Success, Problem0, Problem) :-
    run_literal(Lit, Success0, Problem0, Problem1),
    ( Success0 = delayed,
        Success = delayed,
        Problem = Problem0
    ; Success0 = failed(Reason),
        Success = failed(Reason),
        Problem = Problem0
    ;
        ( Success0 = success_updated
        ; Success0 = success_not_updated
        ),
        Problem = Problem1,
        Success = Success0
    ).
run_disj([Lit | Lits], MaybeDelayed, Success, Problem0, Problem) :-
    run_literal(Lit, Success0, Problem0, Problem1),
    ( Success0 = success_updated,
        trace [io(!IO), compile_time(flag("typecheck_solve"))] (
            write_string("  disjunct updates domain, delaying\n", !IO)
        ),
        % TODO. Only would-update literals should cause execution to
        % continue.
        ( MaybeDelayed = yes(_),
            Success = delayed,
            Problem = Problem0
        ; MaybeDelayed = no,
            run_disj(Lits, yes(Lit), Success, Problem0, Problem)
        )
    ; Success0 = success_not_updated,
        % Switch to checking that the remaining items are false.
        run_disj_all_false(Lits, MaybeDelayed, Problem1, Success),
        Problem = Problem1
    ; Success0 = delayed,
        Success = delayed,
        Problem = Problem1
    ; Success0 = failed(_), % XXX: Keep the reason.
        run_disj(Lits, MaybeDelayed, Success, Problem0, Problem)
    ).

:- pred run_disj_all_false(list(constraint_literal(V))::in,
    maybe(constraint_literal(V))::in, problem_solving(V)::in,
    executed::out) is det.

run_disj_all_false([], no, _, success_not_updated).
run_disj_all_false([], yes(Lit), Problem, Success) :-
    run_literal(Lit, Success0, Problem, _),
    (
        ( Success0 = success_updated
        ; Success0 = success_not_updated
        ),
        unexpected($file, $pred, "Ambigious types")
    ; Success0 = failed(_Reason),
        Success = success_not_updated
    ; Success0 = delayed,
        Success = delayed
    ).
run_disj_all_false([Lit | Lits], MaybeDelayed, Problem, Success) :-
    run_literal(Lit, Success0, Problem, _),
    ( Success0 = success_updated,
        trace [io(!IO), compile_time(flag("typecheck_solve"))] (
            write_string("  disjunct would write updates, delaying\n", !IO)
        ),
        ( MaybeDelayed = yes(_),
            Success = delayed
        ; MaybeDelayed = no,
            run_disj_all_false(Lits, yes(Lit), Problem, Success)
        )
    ; Success0 = success_not_updated,
        unexpected($file, $pred, "Ambigious types")
    ; Success0 = delayed,
        Success = delayed
    ; Success0 = failed(_Reason),
        run_disj_all_false(Lits, MaybeDelayed, Problem, Success)
    ).

:- type executed
    --->    success_updated
    ;       success_not_updated
    ;       failed(string)
    ;       delayed.

:- inst executed_no_delay
    --->    success_updated
    ;       success_not_updated
    ;       failed(ground).

    % Run the literal immediately.  Directly update domains and add
    % propagators.
    %
:- pred run_literal(constraint_literal(V)::in, executed::out,
    problem_solving(V)::in, problem_solving(V)::out) is det.

run_literal(cl_true, success_not_updated, !Problem).
run_literal(cl_var_builtin(Var, Builtin), Success, !Problem) :-
    trace [io(!IO), compile_time(flag("typecheck_solve"))] (
        io.format("Run: %s = %s\n",
            [s(string(Var)), s(string(Builtin))], !IO)
    ),
    update_domain(Var, d_builtin(Builtin), Success, !Problem),
    ( if Success = failed(Reason) then
        trace [io(!IO), compile_time(flag("typecheck_solve"))] (
            io.format("..failed: %s\n", [s(Reason)], !IO)
        )
    else
        true
    ).
run_literal(cl_var_var(Var1, Var2, Context), Success, !Problem) :-
    trace [io(!IO), compile_time(flag("typecheck_solve"))] (
        io.format("Run: %s = %s\n",
            [s(string(Var1)), s(string(Var2))], !IO)
    ),
    DomainMap0 = !.Problem ^ p_domains,
    Dom1 = get_domain(DomainMap0, Var1),
    Dom2 = get_domain(DomainMap0, Var2),
    unify_domains(Dom1, Dom2, Dom),
    ( Dom = delayed,
        Success = delayed,
        trace [io(!IO), compile_time(flag("typecheck_solve"))] (
            io.write_string("..delayed.\n", !IO)
        )
    ; Dom = failed(Reason),
        ContextStr = context_string(Context),
        Success = failed(Reason ++ " at " ++ ContextStr),
        trace [io(!IO), compile_time(flag("typecheck_solve"))] (
            io.format("..failed %s at %s\n", [s(Reason), s(ContextStr)], !IO)
        )
    ; Dom = new_domain(NewDom),
        set(Var1, NewDom, DomainMap0, DomainMap1),
        set(Var2, NewDom, DomainMap1, DomainMap),
        !Problem ^ p_domains := DomainMap,
        Success = success_updated
    ; Dom = old_domain,
        Success = success_not_updated
    ).
run_literal(cl_var_usertype(Var, Type), Success, !Problem) :-
    trace [io(!IO), compile_time(flag("typecheck_solve"))] (
        io.format("Run: %s = %s\n",
            [s(string(Var)), s(string(Type))], !IO)
    ),
    update_domain(Var, d_type(Type), Success, !Problem),
    ( if Success = failed(Reason) then
        trace [io(!IO), compile_time(flag("typecheck_solve"))] (
            io.format("..failed %s\n", [s(Reason)], !IO)
        )
    else
        true
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

:- type domain
    --->    d_free
    ;       d_builtin(builtin_type)
    ;       d_type(type_id).

:- func get_domain(map(var(V), domain), var(V)) = domain.

get_domain(Map, Var) =
    ( if map.search(Map, Var, Domain) then
        Domain
    else
        d_free
    ).

%-----------------------------------------------------------------------%

:- pred update_domain(var(V)::in, domain::in,
    executed::out(executed_no_delay),
    problem_solving(V)::in, problem_solving(V)::out) is det.

update_domain(Var, UnifyDomain, Success, !Problem) :-
    Domains0 = !.Problem ^ p_domains,
    ( if search(Domains0, Var, OldDomain) then
        trace [io(!IO), compile_time(flag("typecheck_solve"))] (
            write_string("..updating existing domain\n", !IO),
            format("  old: %s, unify: %s\n",
                [s(string(OldDomain)), s(string(UnifyDomain))], !IO)
        ),
        unify_domains(OldDomain, UnifyDomain, MaybeNewDomain),
        trace [io(!IO), compile_time(flag("typecheck_solve"))] (
            format("  maybe new: %s\n", [s(string(MaybeNewDomain))], !IO)
        ),
        ( MaybeNewDomain = new_domain(NewDomain),
            set(Var, NewDomain, Domains0, Domains),
            !Problem ^ p_domains := Domains,
            Success = success_updated
        ; MaybeNewDomain = old_domain,
            Success = success_not_updated
        ; MaybeNewDomain = failed(Reason),
            Success = failed(Reason)
        ; MaybeNewDomain = delayed,
            unexpected($file, $pred, "Caller must provide a concrete type")
        )
    else
        NewDomain = UnifyDomain,
        det_insert(Var, NewDomain, Domains0, Domains),
        !Problem ^ p_domains := Domains,
        Success = success_updated,
        ( if NewDomain = d_free then
            unexpected($file, $pred, "Can this really happen?")
        else
            true
        ),
        trace [io(!IO), compile_time(flag("typecheck_solve"))] (
            format("  inserted new domain %s\n", [s(string(NewDomain))], !IO)
        )
    ).

:- type maybe_new_domain
    --->    new_domain(domain)
    ;       old_domain
    ;       failed(string)
    ;       delayed.

:- pred unify_domains(domain::in, domain::in, maybe_new_domain::out) is det.

unify_domains(Dom1, Dom2, Dom) :-
    ( Dom1 = d_free,
        ( Dom2 = d_free,
            Dom = delayed
        ;
            ( Dom2 = d_builtin(_)
            ; Dom2 = d_type(_)
            ),
            Dom = new_domain(Dom2)
        )
    ; Dom1 = d_builtin(Builtin1),
        ( Dom2 = d_free,
            Dom = new_domain(Dom1)
        ; Dom2 = d_builtin(Builtin2),
            ( if Builtin1 = Builtin2 then
                Dom = old_domain
            else
                Dom = failed("Builtin types don't match")
            )
        ; Dom2 = d_type(_),
            Dom = failed("builtin and user types don't match")
        )
    ; Dom1 = d_type(Type1),
        ( Dom2 = d_free,
            Dom = new_domain(Dom1)
        ; Dom2 = d_builtin(_),
            Dom = failed("user and builtin types don't match")
        ; Dom2 = d_type(Type2),
            ( if Type1 = Type2 then
                Dom = old_domain
            else
                Dom = failed("user types don't match")
            )
        )
    ).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
