%-----------------------------------------------------------------------%
% Solver for typechecking/inference.
% vim: ts=4 sw=4 et
%
% Copyright (C) 2016-2017 Plasma Team
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
    ;       v_anon(int)
    ;       v_type_var(int).

:- type problem(V).

:- func init = problem(V).

:- pred new_variable(var(V)::out, problem(V)::in, problem(V)::out) is det.

:- pred new_variables(int::in, list(var(V))::out,
    problem(V)::in, problem(V)::out) is det.

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
    ;       cl_var_usertype(var(V), type_id, list(var(V)), context)
    ;       cl_var_free_type_var(var(V), type_var)
    ;       cl_var_var(var(V), var(V), context).

:- type constraint(V).

:- func make_constraint(constraint_literal(V)) = constraint(V).

    % Make the constraint that this variable has one of the given types.
    % In other words this is a disjunction.
    %
:- func make_constraint_user_types(set(type_id), var(V)) = constraint(V).

:- func make_conjunction_from_lits(list(constraint_literal(V))) = constraint(V).

:- func make_conjunction(list(constraint(V))) = constraint(V).

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
%
% Type variable handling.
%
% Type variables are scoped to their declrations.  so x in one declration is
% a different variable from x in another.  While the constraint problem is
% built, we map these different variables (with or without the same names)
% to distict variables in the constraint problem.  Therefore we track type
% variables throughout building the problem but switch to and from building
% and using a map (as we read each declration).
%

:- type type_vars.

:- type type_var_map(T).

:- func init_type_vars = type_vars.

:- pred start_type_var_mapping(type_vars::in, type_var_map(T)::out)
    is det.

:- pred end_type_var_mapping(type_var_map(T)::in, type_vars::out)
    is det.

:- pred get_or_make_type_var(T::in, var(U)::out, type_var_map(T)::in,
    type_var_map(T)::out) is det.

:- pred make_type_var(T::in, var(U)::out, type_var_map(T)::in,
    type_var_map(T)::out) is det.

:- func lookup_type_var(type_var_map(T), T) = var(U).

    % TODO: make type_var a type variable to decrease cupling with the
    % solver.
:- pred maybe_add_free_type_var(type_var::in, constraint_literal(V)::out,
    type_var_map(type_var)::in, type_var_map(type_var)::out) is det.

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

new_variables(N, Vars, !Problem) :-
    ( if N > 0 then
        new_variable(V, !Problem),
        new_variables(N - 1, Vs, !Problem),
        Vars = [V | Vs]
    else
        Vars = []
    ).

%-----------------------------------------------------------------------%

    % Constraints are kept in CNF form.
    %
:- type constraint(V)
    --->    conj(list(clause(V))).

:- type clause(V)
    --->    single(constraint_literal(V))
    ;       disj(constraint_literal(V), list(constraint_literal(V))).

%-----------------------------------------------------------------------%

make_constraint(Lit) = conj([single(Lit)]).

%-----------------------------------------------------------------------%

make_conjunction_from_lits(Literals0) = conj(Conj) :-
    % Remove any true literals, they cannot affect the conjunction.
    Literals1 = filter((pred(L::in) is semidet :- L \= cl_true), Literals0),
    Literals = sort_and_remove_dups(Literals1),
    Conj = map(func(L) = single(L), Literals).

make_conjunction(Constraints) = conj(Conj) :-
    make_conjunction(Constraints, [], Conj).

:- pred make_conjunction(list(constraint(V))::in, list(clause(V))::in,
    list(clause(V))::out) is det.

make_conjunction([], !Clauses).
make_conjunction([conj(Conj) | Conjs], !Clauses) :-
    !:Clauses = Conj ++ !.Clauses,
    make_conjunction(Conjs, !Clauses).

%-----------------------------------------------------------------------%

    % Perform make_disjunction by by algebraicly manipulating the equation.
    %
make_disjunction([]) = unexpected($file, $pred, "Empty list").
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

make_disjunction_clause(single(D1), single(D2)) = disj(D1, [D2]).
make_disjunction_clause(single(D1), disj(D2, Ds3)) = disj(D1, [D2 | Ds3]).
make_disjunction_clause(disj(D1, Ds2), single(D3)) = disj(D1, [D3 | Ds2]).
make_disjunction_clause(disj(D1, Ds2), disj(D3, Ds4)) =
    disj(D1, [D3 | Ds2 ++ Ds4]).

%-----------------------------------------------------------------------%

post_constraint(conj(ConjsA), !Problem) :-
    conj(ConjsB) = !.Problem ^ p_constraints,
    Conjs = merge_and_remove_dups(ConjsA, ConjsB),
    !Problem ^ p_constraints := conj(Conjs).

%-----------------------------------------------------------------------%

:- func constraint_vars(constraint(V)) = set(var(V)).

constraint_vars(conj(Clauses)) = union_list(map(clause_vars, Clauses)).

:- func clause_vars(clause(V)) = set(var(V)).

clause_vars(single(Lit)) = literal_vars(Lit).
clause_vars(disj(Lit, Lits)) =
    literal_vars(Lit) `union` union_list(map(literal_vars, Lits)).

:- func literal_vars(constraint_literal(V)) = set(var(V)).

literal_vars(cl_true) = init.
literal_vars(cl_var_builtin(Var, _)) = make_singleton_set(Var).
literal_vars(cl_var_usertype(Var, _, ArgVars, _)) =
    insert(from_list(ArgVars), Var).
literal_vars(cl_var_free_type_var(Var, _)) = make_singleton_set(Var).
literal_vars(cl_var_var(VarA, VarB, _)) = from_list([VarA, VarB]).

%-----------------------------------------------------------------------%

:- import_module pretty_utils.

:- func pretty_problem(constraint(V)) = cord(string).

pretty_problem(conj(Conjs)) = join(nl, map(pretty_clause, Conjs)).

:- func pretty_clause(clause(V)) = cord(string).

pretty_clause(single(Lit)) = pretty_literal(Lit).
pretty_clause(disj(Lit, Lits)) = singleton("Disjunction:") ++
    cord_list_to_cord(map(
        (func(L) = line(2) ++ pretty_literal(L) ++ singleton(" or")),
        Lits)) ++
    line(2) ++ pretty_literal(Lit).

:- func pretty_literal(constraint_literal(V)) = cord(string).

pretty_literal(cl_true) = singleton("true").
pretty_literal(cl_var_builtin(Var, Builtin)) = cord_string(Var) ++ spceqspc
    ++ cord_string(Builtin).
pretty_literal(cl_var_usertype(Var, Usertype, ArgVars, Context)) =
    cord_string(Var) ++ spceqspc ++ cord_string(Usertype) ++
        pretty_optional_args(cord_string, ArgVars) ++
        spcatspc ++ singleton(context_string(Context)).
pretty_literal(cl_var_free_type_var(Var, TypeVar)) =
    cord_string(Var) ++ spceqspc ++ cord.singleton(TypeVar).
pretty_literal(cl_var_var(Var1, Var2, Context)) =
    cord_string(Var1) ++ spceqspc ++ cord_string(Var2) ++ spcatspc ++
        singleton(context_string(Context)).

:- func pretty_store(problem_solving(V)) = cord(string).

pretty_store(problem(Vars, Domains)) = Pretty :-
    Pretty = line(0) ++ singleton("Store:") ++
        cord_list_to_cord(VarDomsPretty),
    VarDomsPretty = map(pretty_var_domain(Domains), to_sorted_list(Vars)).

:- func pretty_var_domain(map(var(V), domain), var(V)) = cord(string).

pretty_var_domain(Domains, Var) = Pretty :-
    Pretty = line(2) ++ cord_string(Var) ++ spceqspc ++ cord_string(Domain),
    Domain = get_domain(Domains, Var).

:- func spceqspc = cord(string).
spceqspc = singleton(" = ").

:- func spcatspc = cord(string).
spcatspc = singleton(" @ ").

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
    Problem0 = problem(AllVars, init),
    Constraints = conj(Clauses),
    run_clauses(Clauses, Problem0, Result),
    ( Result = ok(Problem),
        foldl(build_results(Problem ^ p_domains), AllVars, init, Solution)
    ; Result = failed(Reason),
        compile_error($module, $pred, "Typechecking failed: " ++ Reason)
    ).

%-----------------------------------------------------------------------%

:- type problem_solving(V)
    --->    problem(
                p_vars          :: set(var(V)),
                p_domains       :: map(var(V), domain)
                % Not currently using propagators.
                % p_propagators   :: map(var(V), set(propagator(V)))
            ).

:- type problem_result(V)
    --->    ok(problem_solving(V))
    ;       failed(string).

% We're not currently using propagators in the solver.
% :- type propagator(V)
%     --->    propagator(constraint(V)).

:- pred run_clauses(list(clause(V))::in,
    problem_solving(V)::in, problem_result(V)::out) is det.

run_clauses(Clauses, Problem, Result) :-
    run_clauses(Clauses, [], length(Clauses), domains_not_updated,
        Problem, Result).

:- type domains_updated
    --->    domains_not_updated
    ;       domains_updated.

    % Run the clauses until we can't make any further progress.
    %
:- pred run_clauses(list(clause(V))::in, list(clause(V))::in, int::in,
    domains_updated::in,
    problem_solving(V)::in, problem_result(V)::out) is det.

run_clauses([], [], _, _, Problem, ok(Problem)).
run_clauses([], Cs@[_ | _], OldLen, Updated, Problem, Result) :-
    Len = length(Cs),
    ( if Len < OldLen ; Updated = domains_updated then
        % Before running the delayed clauses we check to see if we are
        % indeed making progress.
        run_clauses(reverse(Cs), [], Len, domains_not_updated, Problem,
            Result)
    else
        util.sorry($file, $pred,
            format("Floundering %d >= %d and %s: %s",
                [i(Len), i(OldLen), s(string(Updated)), s(string(Cs))]))
    ).
run_clauses([C | Cs], Delays0, ProgressCheck, Updated0, !.Problem, Result) :-
    run_clause(C, Delays0, Delays, Updated0, Updated, !.Problem, ClauseResult),
    ( ClauseResult = ok(!:Problem),
        run_clauses(Cs, Delays, ProgressCheck, Updated, !.Problem, Result)
    ; ClauseResult = failed(_),
        Result = ClauseResult
    ).

:- pred run_clause(clause(V)::in, list(clause(V))::in, list(clause(V))::out,
    domains_updated::in, domains_updated::out,
    problem_solving(V)::in, problem_result(V)::out) is det.

run_clause(Clause, !Delays, !Updated, Problem0, Result) :-
    ( Clause = single(Lit),
        run_literal(Lit, Success, Problem0, Problem)
    ; Clause = disj(Lit, Lits),
        run_disj([Lit | Lits], Success, Problem0, Problem)
    ),
    ( Success = success_updated,
        Result = ok(Problem),
        !:Updated = domains_updated
    ; Success = success_not_updated,
        Result = ok(Problem0)
    ; Success = failed(Reason),
        Result = failed(Reason)
    ;
        ( Success = delayed_updated,
            Result = ok(Problem),
            !:Updated = domains_updated
        ; Success = delayed_not_updated,
            Result = ok(Problem0)
        ),
        !:Delays = [Clause | !.Delays]
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
:- pred run_disj(list(constraint_literal(V))::in, executed::out,
    problem_solving(V)::in, problem_solving(V)::out) is det.

run_disj(Disjs, Delayed, !Problem) :-
    trace [io(!IO), compile_time(flag("typecheck_solve"))] (
        io.write_string("Running disjunction", !IO)
    ),
    run_disj(Disjs, no, Delayed, !Problem),
    trace [io(!IO), compile_time(flag("typecheck_solve"))] (
        io.write_string("Finished disjunction", !IO)
    ).

:- pred run_disj(list(constraint_literal(V))::in,
    maybe(constraint_literal(V))::in, executed::out,
    problem_solving(V)::in, problem_solving(V)::out) is det.

run_disj([], no, failed("all disjuncts failed"), !Problem).
run_disj([], yes(Lit), Success, Problem0, Problem) :-
    run_literal(Lit, Success, Problem0, Problem1),
    % Since all the other disjuncts have failed (or don't exist) then we may
    % update the problem, because we have proven that this disjunct
    % is always the only true one.
    ( if is_updated(Success) then
        Problem = Problem1
    else
        Problem = Problem0
    ).
run_disj([Lit | Lits], MaybeDelayed, Success, Problem0, Problem) :-
    run_literal(Lit, Success0, Problem0, Problem1),
    (
        ( Success0 = success_updated
        ; Success0 = delayed_updated
        ),
        trace [io(!IO), compile_time(flag("typecheck_solve"))] (
            write_string("  disjunct updates domain, delaying\n", !IO)
        ),
        ( MaybeDelayed = yes(_),
            Success = delayed_not_updated,
            Problem = Problem0
        ; MaybeDelayed = no,
            % If an item is the last one and it would update the problem
            % and might be true, then it'll eventually be re-executed above.
            run_disj(Lits, yes(Lit), Success, Problem0, Problem)
        )
    ; Success0 = success_not_updated,
        % Switch to checking that the remaining items are false.
        run_disj_all_false(Lits, MaybeDelayed, Problem1, Success),
        Problem = Problem1
    ; Success0 = delayed_not_updated,
        Success = delayed_not_updated,
        Problem = Problem0
    ; Success0 = failed(_), % XXX: Keep the reason.
        run_disj(Lits, MaybeDelayed, Success, Problem0, Problem)
    ).

:- pred run_disj_all_false(list(constraint_literal(V))::in,
    maybe(constraint_literal(V))::in, problem_solving(V)::in,
    executed::out) is det.

run_disj_all_false([], no, _, success_not_updated).
run_disj_all_false([], yes(Lit), Problem, Success) :-
    run_literal(Lit, Success0, Problem, _),
    ( Success0 = success_updated,
        Success = delayed_not_updated
    ; Success0 = success_not_updated,
        unexpected($file, $pred, "Ambigious types") % XXX: Shouldn't this delay?
    ; Success0 = failed(_Reason),
        Success = success_not_updated
    ; Success0 = delayed_not_updated,
        Success = delayed_not_updated
    ; Success0 = delayed_updated,
        Success = delayed_not_updated
    ).
run_disj_all_false([Lit | Lits], MaybeDelayed, Problem, Success) :-
    run_literal(Lit, Success0, Problem, _),
    (
        ( Success0 = success_updated
        ; Success0 = delayed_updated
        ),
        trace [io(!IO), compile_time(flag("typecheck_solve"))] (
            write_string("  disjunct would write updates, delaying\n", !IO)
        ),
        ( MaybeDelayed = yes(_),
            Success = delayed_not_updated
        ; MaybeDelayed = no,
            run_disj_all_false(Lits, yes(Lit), Problem, Success)
        )
    ; Success0 = success_not_updated,
        unexpected($file, $pred, "Ambigious types")
    ; Success0 = delayed_not_updated,
        Success = delayed_not_updated
    ; Success0 = failed(_Reason),
        run_disj_all_false(Lits, MaybeDelayed, Problem, Success)
    ).

:- type executed
    --->    success_updated
    ;       success_not_updated
    ;       failed(string)

            % We've updated the problem but something in theis constraint
            % can't be run now, so revisit the whole constraint later.
    ;       delayed_updated

    ;       delayed_not_updated.

:- inst executed_no_delay
    --->    success_updated
    ;       success_not_updated
    ;       failed(ground).

:- pred is_updated(executed::in) is semidet.

is_updated(success_updated).
is_updated(delayed_updated).

:- pred mark_delayed(executed::in, executed::out) is det.

mark_delayed(success_updated,     delayed_updated).
mark_delayed(success_not_updated, delayed_not_updated).
mark_delayed(failed(_),           _) :-
    unexpected($file, $pred, "Cannot delay after failure").
mark_delayed(delayed_updated,     delayed_updated).
mark_delayed(delayed_not_updated, delayed_not_updated).

:- pred mark_updated(executed::in, executed::out) is det.

mark_updated(success_updated,     success_updated).
mark_updated(success_not_updated, success_updated).
mark_updated(failed(_),           _) :-
    unexpected($file, $pred, "Cannot update after failure").
mark_updated(delayed_updated,     delayed_updated).
mark_updated(delayed_not_updated, delayed_not_updated).

    % Run the literal immediately.  Directly update domains and add
    % propagators.
    %
:- pred run_literal(constraint_literal(V)::in, executed::out,
    problem_solving(V)::in, problem_solving(V)::out) is det.

run_literal(Lit, Success, !Problem) :-
    trace [io(!IO), compile_time(flag("typecheck_solve"))] (
        PrettyDomains = pretty_store(!.Problem) ++ nl,
        write_string(append_list(list(PrettyDomains)), !IO),
        io.write_string(append_list(list(
            singleton("Run: ") ++ pretty_literal(Lit) ++ nl)), !IO)
    ),
    run_literal_2(Lit, Success, !Problem),
    trace [io(!IO), compile_time(flag("typecheck_solve"))] (
        io.format("  %s\n", [s(string(Success))], !IO)
    ).

:- pred run_literal_2(constraint_literal(V)::in, executed::out,
    problem_solving(V)::in, problem_solving(V)::out) is det.

run_literal_2(cl_true, success_not_updated, !Problem).
run_literal_2(cl_var_builtin(Var, Builtin), Success, !Problem) :-
    Domains0 = !.Problem ^ p_domains,
    OldDomain = get_domain(Domains0, Var),
    NewDomain = d_builtin(Builtin),
    ( OldDomain = d_free,
        map.set(Var, NewDomain, Domains0, Domains),
        !Problem ^ p_domains := Domains,
        Success = success_updated
    ; OldDomain = d_builtin(ExistBuiltin),
        ( if Builtin = ExistBuiltin then
            Success = success_not_updated
        else
            Success = failed("Distinct builtins conflict")
        )
    ;
        ( OldDomain = d_type(_, _)
        ; OldDomain = d_univ_var(_)
        ),
        Success = failed("Builtin and user-type or " ++
            "universal type var conflict")
    ).
run_literal_2(cl_var_var(Var1, Var2, Context), Success, !Problem) :-
    DomainMap0 = !.Problem ^ p_domains,
    Dom1 = get_domain(DomainMap0, Var1),
    Dom2 = get_domain(DomainMap0, Var2),
    trace [io(!IO), compile_time(flag("typecheck_solve"))] (
        format("  left: %s right: %s\n",
            [s(string(Dom1)), s(string(Dom2))], !IO)
    ),
    unify_domains(Dom1, Dom2, Dom),
    ( Dom = delayed,
        Success = delayed_not_updated
    ; Dom = failed(Reason),
        ContextStr = context_string(Context),
        Success = failed(Reason ++ " at " ++ ContextStr)
    ; Dom = new_domain(NewDom),
        set(Var1, NewDom, DomainMap0, DomainMap1),
        set(Var2, NewDom, DomainMap1, DomainMap),
        !Problem ^ p_domains := DomainMap,
        Groundness = groundness(NewDom),
        ( Groundness = bound_with_holes_or_free,
            Success = delayed_updated
        ; Groundness = ground,
            Success = success_updated
        ),
        trace [io(!IO), compile_time(flag("typecheck_solve"))] (
            format("  new: %s\n", [s(string(NewDom))], !IO)
        )
    ; Dom = old_domain,
        Success = success_not_updated
    ).
run_literal_2(cl_var_free_type_var(Var, TypeVar), Success, !Problem) :-
    Domains0 = !.Problem ^ p_domains,
    OldDomain = get_domain(Domains0, Var),
    NewDomain = d_univ_var(TypeVar),
    ( OldDomain = d_free,
        map.set(Var, NewDomain, Domains0, Domains),
        !Problem ^ p_domains := Domains,
        Success = success_updated
    ; OldDomain = d_univ_var(ExistTypeVar),
        ( if TypeVar = ExistTypeVar then
            Success = success_not_updated
        else
            Success = failed("Distinct free type variables conflict")
        )
    ;
        ( OldDomain = d_type(_, _)
        ; OldDomain = d_builtin(_)
        ),
        Success = failed("Universal type var and builtin or user-type " ++
            "use type conflict")
    ).
run_literal_2(cl_var_usertype(Var, TypeUnify, ArgsUnify, _Context), Success,
		!Problem) :-
    Domains0 = !.Problem ^ p_domains,
    Domain = get_domain(Domains0, Var),
    ArgDomainsUnify = map(get_domain(Domains0), ArgsUnify),
    NewDomain = d_type(TypeUnify, ArgDomainsUnify),
    ( Domain = d_free,
        map.set(Var, NewDomain, Domains0, Domains),
        !Problem ^ p_domains := Domains,
        Groundness = groundness(NewDomain),
        ( Groundness = bound_with_holes_or_free,
            Success = delayed_updated
        ; Groundness = ground,
            Success = success_updated
        )
    ; Domain = d_type(Type0, Args0),
        ( if Type0 = TypeUnify then
            % Save any information from this domain back into the variables
            % being unified with the type's arguments.
            update_args(Args0, ArgsUnify, success_not_updated, Success0,
                !.Problem, MaybeProblem),
            ( if Success0 = failed(Reason) then
                Success = failed(Reason)
            else
                ( if is_updated(Success0) then
                    !:Problem = MaybeProblem
                else
                    true
                ),
                Args = map(get_domain(MaybeProblem ^ p_domains), ArgsUnify),
                ( if Args \= Args0 then
                    map.set(Var, d_type(Type0, Args), !.Problem ^ p_domains,
                        Domains),
                    !Problem ^ p_domains := Domains,
                    mark_updated(Success0, Success)
                else
                    % We can use the delayed/success from update_args, since
                    % it depends on groundness.
                    Success = Success0
                )
            )
        else
            Success = failed("Distinct user types")
        )
    ;
        ( Domain = d_builtin(_)
        ; Domain = d_univ_var(_)
        ),
        Success = failed("User-type and builtin or " ++
            "universal type var conflict")
    ).

:- pred update_args(list(domain)::in, list(var(V))::in,
    executed::in, executed::out,
    problem_solving(V)::in, problem_solving(V)::out) is det.

update_args([], [], !Success, !Problem).
update_args([], [_ | _], !Success, !Problem) :-
    unexpected($file, $pred, "Mismatched type argument lists").
update_args([_ | _], [], !Success, !Problem) :-
    unexpected($file, $pred, "Mismatched type argument lists").
update_args([D0 | Ds], [V | Vs], !Success, !Problem) :-
    ( if !.Success = failed(_) then
        true
    else
        Domains0 = !.Problem ^ p_domains,
        VD = get_domain(Domains0, V),
        unify_domains(D0, VD, MaybeD),
        ( MaybeD = failed(Reason),
            !:Success = failed(Reason)
        ; MaybeD = delayed,
            mark_delayed(!Success)
        ; MaybeD = old_domain,
            true
        ; MaybeD = new_domain(D),
            map.set(V, D, Domains0, Domains),
            !Problem ^ p_domains := Domains,
            mark_updated(!Success)
        ),

        update_args(Ds, Vs, !Success, !Problem)
    ).

%-----------------------------------------------------------------------%

:- pred build_results(map(var(V), domain)::in, var(V)::in,
    map(V, type_)::in, map(V, type_)::out) is det.

build_results(_,   v_anon(_),     !Results).
build_results(_,   v_type_var(_), !Results). % XXX
build_results(Map, v_named(V),    !Results) :-
    lookup(Map, v_named(V), Domain),
    Type = domain_to_type(Domain),
    det_insert(V, Type, !Results).

:- func domain_to_type(domain) = type_.

domain_to_type(d_free) = unexpected($file, $pred, "Free variable").
domain_to_type(d_builtin(Builtin)) = builtin_type(Builtin).
domain_to_type(d_type(TypeId, Args)) =
    type_ref(TypeId, map(domain_to_type, Args)).
domain_to_type(d_univ_var(TypeVar)) = type_variable(TypeVar).

%-----------------------------------------------------------------------%

:- type domain
    --->    d_free
    ;       d_builtin(builtin_type)
    ;       d_type(type_id, list(domain))
            % A type variable from the function's signature.
    ;       d_univ_var(type_var).

:- func get_domain(map(var(V), domain), var(V)) = domain.

get_domain(Map, Var) =
    ( if map.search(Map, Var, Domain) then
        Domain
    else
        d_free
    ).

:- type groundness
    --->    bound_with_holes_or_free
    ;       ground.

:- func groundness(domain) = groundness.

groundness(d_free) = bound_with_holes_or_free.
groundness(d_builtin(_)) = ground.
groundness(d_type(_, Args)) = Groundness :-
    ( if
        some [Arg] (
            member(Arg, Args),
            ArgGroundness = groundness(Arg),
            ArgGroundness = bound_with_holes_or_free
        )
    then
        Groundness = bound_with_holes_or_free
    else
        Groundness = ground
    ).
groundness(d_univ_var(_)) = ground.

%-----------------------------------------------------------------------%

:- type maybe_new_domain
    --->    new_domain(domain)
    ;       old_domain
    ;       failed(string)
    ;       delayed.

:- pred unify_domains(domain::in, domain::in, maybe_new_domain::out)
    is det.

unify_domains(Dom1, Dom2, Dom) :-
    ( Dom1 = d_free,
        ( Dom2 = d_free,
            Dom = delayed
        ;
            ( Dom2 = d_builtin(_)
            ; Dom2 = d_type(_, _)
            ; Dom2 = d_univ_var(_)
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
        ; Dom2 = d_type(_, _),
            Dom = failed("builtin and user types don't match")
        ; Dom2 = d_univ_var(_),
            Dom = failed("builtin and universal type parameter don't match")
        )
    ; Dom1 = d_type(Type1, Args1),
        ( Dom2 = d_free,
            Dom = new_domain(Dom1)
        ; Dom2 = d_builtin(_),
            Dom = failed("user and builtin types don't match")
        ; Dom2 = d_type(Type2, Args2),
            ( if Type1 = Type2 then
                MaybeNewArgs = unify_args_domains(Args1, Args2),
                ( MaybeNewArgs = new_domains(ArgsRev),
                    Args = reverse(ArgsRev),
                    Dom = new_domain(d_type(Type1, Args))
                ; MaybeNewArgs = old_domains(_),
                    Dom = old_domain
                ; MaybeNewArgs = failed(Reason),
                    Dom = failed(Reason)
                ; MaybeNewArgs = delayed,
                    % Surely there's some information we can save.
                    Dom = delayed
                )
            else
                Dom = failed("user types don't match")
            )
        ; Dom2 = d_univ_var(_),
            Dom = failed("user type and user types don't match")
        )
    ; Dom1 = d_univ_var(Var1),
        ( Dom2 = d_free,
            Dom = new_domain(Dom1)
        ; Dom2 = d_builtin(_),
            Dom = failed("Universal type var and builtin type don't match")
        ; Dom2 = d_type(_, _),
            Dom = failed("Universal type var and user types don't match")
        ; Dom2 = d_univ_var(Var2),
            ( if Var1 = Var2 then
                Dom = old_domain
            else
                Dom = failed("Universal type vars don't match")
            )
        )
    ).

:- type maybe_new_domain_args
    --->    new_domains(list(domain))
    ;       old_domains(list(domain))
    ;       failed(string)
    ;       delayed.

:- func unify_args_domains(list(domain), list(domain)) = maybe_new_domain_args.

unify_args_domains(Args1, Args2) =
    unify_args_domains_2(Args1, Args2, old_domains([])).

:- func unify_args_domains_2(list(domain), list(domain),
    maybe_new_domain_args) = maybe_new_domain_args.

unify_args_domains_2([], [], Doms) = Doms.
unify_args_domains_2([], [_ | _], _) =
    unexpected($file, $pred, "Mismatched type argument lists").
unify_args_domains_2([_ | _], [], _) =
    unexpected($file, $pred, "Mismatched type argument lists").
unify_args_domains_2([DA | DAs], [DB | DBs], Doms0) = Doms :-
    unify_domains(DA, DB, D),
    Old = DA,
    Doms1 = unify_args_domains_2(DAs, DBs, Doms0),
    ( D = failed(Reason),
        Doms = failed(Reason)
    ; D = delayed,
        ( Doms1 = failed(Reason),
            Doms = failed(Reason)
        ;
            ( Doms1 = delayed
            ; Doms1 = new_domains(_)
            ; Doms1 = old_domains(_)
            ),
            Doms = delayed
        )
    ; D = old_domain,
        ( Doms1 = failed(Reason),
            Doms = failed(Reason)
        ; Doms1 = delayed,
            Doms = delayed
        ; Doms1 = old_domains(Olds),
            Doms = old_domains([Old | Olds])
        ; Doms1 = new_domains(News),
            Doms = new_domains([Old | News])
        )
    ; D = new_domain(New),
        ( Doms1 = failed(Reason),
            Doms = failed(Reason)
        ; Doms1 = delayed,
            Doms = delayed
        ; Doms1 = old_domains(Olds),
            Doms = new_domains([New | Olds])
        ; Doms1 = new_domains(News),
            Doms = new_domains([New | News])
        )
    ).

%-----------------------------------------------------------------------%

:- type type_vars == int.

:- type type_var_map(T)
    --->    type_var_map(
                tvm_source      :: int,
                tvm_map         :: map(T, int)
            ).

init_type_vars = 0.

start_type_var_mapping(Source, type_var_map(Source, init)).

end_type_var_mapping(type_var_map(Source, _), Source).

get_or_make_type_var(Name, Var, !Map) :-
    ( if search(!.Map ^ tvm_map, Name, Id) then
        Var = v_type_var(Id)
    else
        make_type_var(Name, Var, !Map)
    ).

make_type_var(Name, Var, !Map) :-
    Id = !.Map ^ tvm_source,
    det_insert(Name, Id, !.Map ^ tvm_map, NewMap),
    Var = v_type_var(Id),
    !:Map = type_var_map(Id + 1, NewMap).

lookup_type_var(Map, Name) = v_type_var(map.lookup(Map ^ tvm_map, Name)).

maybe_add_free_type_var(Name, cl_var_free_type_var(Var, Name), !Map) :-
    get_or_make_type_var(Name, Var, !Map).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
