%-----------------------------------------------------------------------%
% Solver for typechecking/inference.
% vim: ts=4 sw=4 et
%
% Copyright (C) 2016-2018, 2020-2021 Plasma Team
% Distributed under the terms of the MIT see ../LICENSE.code
%
% This module implements a FD solver over types.
%
% Use MCFLAGS=--trace-flag typecheck_solve to trace this module.
%
%-----------------------------------------------------------------------%
:- module core.type_chk.solve.
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

:- type svar
    --->    v_named(varmap.var)
            % The type of an output value in this position.
    ;       v_output(int)
    ;       v_anon(int)
    ;       v_type_var(int).

    % A subset of the above, just the "user" variables, those that the
    % typechecker itself uses.
    %
:- type svar_user
    --->    vu_named(varmap.var)
    ;       vu_output(int).

:- type problem.

:- func init = problem.

% A typeclass is used here to allow callers to restrict the possible things
% that may be done to a problem.  In this case using only this typeclass
% guarantee that some code won't post new constraints.
:- typeclass var_source(S) where [
    pred new_variable(string::in, svar::out, S::in, S::out) is det,

    pred new_variables(string::in, int::in, list(svar)::out,
        S::in, S::out) is det
].

:- instance var_source(problem).

:- pred post_constraint(constraint::in, problem::in, problem::out) is det.

%-----------------------------------------------------------------------%

% Constraints are boolean expressions.  Literals and their conjunctions and
% disjunctions.  Although an individual constraint is never more complex
% than disjunctive normal form.  This representation and algorithm is
% simple.  But it may be too simple to handle generics well and it is
% definitely to simple to give useful/helpful type errors.

:- type constraint_literal
    --->    cl_true
    ;       cl_var_builtin(svar, builtin_type, context)
    ;       cl_var_usertype(svar, type_id, list(svar), context)
    ;       cl_var_func(
                clvf_var        :: svar,
                clvf_inputs     :: list(svar),
                clvf_outputs    :: list(svar),
                clvf_resources  :: maybe_resources,
                clvf_context    :: context
            )
    ;       cl_var_free_type_var(svar, type_var, context)
    ;       cl_var_var(svar, svar, context).

:- type constraint
    --->    single(constraint_literal)
    ;       conj(list(constraint))
    ;       disj(list(constraint)).

:- func make_constraint(constraint_literal) = constraint.

    % Make the constraint that this variable has one of the given types.
    % In other words this is a disjunction.
    %
% :- func make_constraint_user_types(set(type_id), svar) = constraint.

:- func make_conjunction_from_lits(list(constraint_literal)) = constraint.

    % Make conjunctions and disjunctions and flatten them as they're
    % created..
    %
:- func make_conjunction(list(constraint)) = constraint.

:- func make_disjunction(list(constraint)) = constraint.

%:- pred post_constraint_abstract(svar::in, type_var::in,
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
%:- pred post_constraint_match(svar::in, svar::in,
%    problem(V)::in, problem(V)::out) is det.

:- func solve(core, varmap, problem) =
    result(map(svar_user, type_), compile_error).

%-----------------------------------------------------------------------%
%
% Type variable handling.
%
% Type variables are scoped to their declarations.  So x in one declaration is
% a different variable from x in another.  While the constraint problem is
% built, we map these different variables (with or without the same names)
% to distinct variables in the constraint problem.  Therefore we track type
% variables throughout building the problem but switch to and from building
% and using a map (as we read each declaration).
%

:- type type_vars.

:- type type_var_map(T).

:- func init_type_vars = type_vars.

:- pred start_type_var_mapping(type_vars::in, type_var_map(T)::out)
    is det.

:- pred end_type_var_mapping(type_var_map(T)::in, type_vars::out)
    is det.

:- pred get_or_make_type_var(T::in, svar::out, type_var_map(T)::in,
    type_var_map(T)::out) is det.

:- pred make_type_var(T::in, svar::out, type_var_map(T)::in,
    type_var_map(T)::out) is det.

:- func lookup_type_var(type_var_map(T), T) = svar.

:- pred maybe_add_free_type_var(context::in, type_var::in,
    constraint_literal::out,
    type_var_map(type_var)::in, type_var_map(type_var)::out) is det.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
:- implementation.

:- import_module io.
:- import_module set.
:- import_module set_tree234.
% Use this set when we need better behaviour for larger sets.
:- type big_set(T) == set_tree234(T).
:- import_module string.

:- import_module core.pretty.
:- import_module util.

:- type problem
    --->    problem(
                p_next_anon_var :: int,
                p_var_comments  :: map(svar, string),
                % All the constraints one conjunction.
                p_constraints   :: list(constraint)
            ).

init = problem(0, init, []).

:- instance var_source(problem) where [
    (new_variable(Comment, v_anon(Var), !Problem) :-
        Var = !.Problem ^ p_next_anon_var,
        !Problem ^ p_next_anon_var := Var + 1,
        map.det_insert(v_anon(Var), Comment, !.Problem ^ p_var_comments,
            VarComments),
        !Problem ^ p_var_comments := VarComments
    ),

    (new_variables(Comment, N, Vars, !Problem) :-
        ( if N > 0 then
            new_variable(format("%s no %d", [s(Comment), i(N)]), V, !Problem),
            new_variables(Comment, N - 1, Vs, !Problem),
            Vars = [V | Vs]
        else
            Vars = []
        ))
].

%-----------------------------------------------------------------------%

post_constraint(Cons, !Problem) :-
    Conjs0 = !.Problem ^ p_constraints,
    ( Cons = conj(NewConjs),
        Conjs = NewConjs ++ Conjs0
    ;
        ( Cons = single(_)
        ; Cons = disj(_)
        ),
        Conjs = [Cons | Conjs0]
    ),
    !Problem ^ p_constraints := Conjs.

%-----------------------------------------------------------------------%

make_constraint(Lit) = single(Lit).

%-----------------------------------------------------------------------%

make_conjunction_from_lits(Lits) =
    make_conjunction(map(func(L) = single(L), Lits)).

make_conjunction(Conjs) = conj(foldl(make_conjunction_loop, Conjs, [])).

:- func make_conjunction_loop(constraint, list(constraint)) =
    list(constraint).

make_conjunction_loop(Conj@single(_), Conjs) = [Conj | Conjs].
make_conjunction_loop(conj(NewConjs), Conjs) =
    foldl(make_conjunction_loop, NewConjs, Conjs).
make_conjunction_loop(Conj@disj(_), Conjs) = [Conj | Conjs].

make_disjunction(Disjs) = disj(foldl(make_disjunction_loop, Disjs, [])).

:- func make_disjunction_loop(constraint, list(constraint)) =
    list(constraint).

make_disjunction_loop(Disj@single(_), Disjs) = [Disj | Disjs].
make_disjunction_loop(Disj@conj(_), Disjs) = [Disj | Disjs].
make_disjunction_loop(disj(NewDisjs), Disjs) =
    foldl(make_disjunction_loop, NewDisjs, Disjs).

%-----------------------------------------------------------------------%

:- type pretty_info
    --->    pretty_info(
                pi_varmap   :: varmap,
                pi_core     :: core
            ).

:- func pretty_problem(pretty_info, list(constraint)) = list(pretty).

pretty_problem(PrettyInfo, Conjs) =
    pretty_constraints(PrettyInfo, Conjs) ++ [p_str(".")].

:- func pretty_constraints(pretty_info, list(constraint)) = list(pretty).

pretty_constraints(PrettyInfo, Conjs) =
    condense(list_join([[p_str(","), p_nl_hard]],
        map(pretty_constraint(PrettyInfo), Conjs))).

:- func pretty_constraint(pretty_info, constraint) = list(pretty).

pretty_constraint(PrettyInfo, single(Lit)) =
    pretty_literal(PrettyInfo, Lit).
pretty_constraint(PrettyInfo, conj(Conjs)) =
    pretty_constraints(PrettyInfo, Conjs).
pretty_constraint(PrettyInfo, disj(Disjs)) =
    [p_str("( "), p_nl_hard] ++
    list_join([p_nl_hard, p_str(";"), p_nl_hard],
        map((func(D) = p_expr(pretty_constraint(PrettyInfo, D))), Disjs)) ++
    [p_nl_hard, p_str(")")].

:- func pretty_problem_flat(pretty_info, list(clause)) = list(pretty).

pretty_problem_flat(PrettyInfo, Conjs) =
    condense(list_join([[p_str(","), p_nl_hard]],
        map(pretty_clause(PrettyInfo), Conjs))) ++ [p_str(".")].

:- func pretty_clause(pretty_info, clause) = list(pretty).

pretty_clause(PrettyInfo, single(Lit)) =
    pretty_literal(PrettyInfo, Lit).
pretty_clause(PrettyInfo, disj(Lit, Lits)) =
    [p_str("(")] ++
    list_join([p_nl_hard, p_str(";"), p_nl_hard],
        map((func(L) = p_expr(pretty_literal(PrettyInfo, L))),
            [Lit | Lits])) ++
    [p_nl_hard, p_str(")")].

:- func pretty_literal(pretty_info, constraint_literal) = list(pretty).

pretty_literal(_,          cl_true) = [p_str("true")].
pretty_literal(PrettyInfo, cl_var_builtin(Var, Builtin, Context)) =
    pretty_context_comment(Context) ++
    [unify(pretty_var(PrettyInfo, Var), p_str(string(Builtin)))].
pretty_literal(PrettyInfo, cl_var_usertype(Var, Usertype, ArgVars, Context)) =
    pretty_context_comment(Context) ++
    [unify(pretty_var(PrettyInfo, Var),
        pretty_user_type(Usertype, map(pretty_var(PrettyInfo), ArgVars)))].
pretty_literal(PrettyInfo,
        cl_var_func(Var, Inputs, Outputs, MaybeResources, Context)) =
    pretty_context_comment(Context) ++
    [unify(pretty_var(PrettyInfo, Var),
        pretty_func_type(PrettyInfo, map(pretty_var(PrettyInfo), Inputs),
            map(pretty_var(PrettyInfo), Outputs), MaybeResources))].
pretty_literal(PrettyInfo, cl_var_free_type_var(Var, TypeVar, Context)) =
    pretty_context_comment(Context) ++
    [unify(pretty_var(PrettyInfo, Var), p_str(TypeVar))].
pretty_literal(PrettyInfo, cl_var_var(Var1, Var2, Context)) =
    pretty_context_comment(Context) ++
    [unify(pretty_var(PrettyInfo, Var1),
        pretty_var(PrettyInfo, Var2))].

:- func pretty_store(problem_solving) = pretty.

pretty_store(problem(Vars, VarComments, Domains, PrettyInfo)) = Pretty :-
    Pretty = p_expr([p_str("Store: "), p_nl_hard] ++ VarDomsPretty),
    VarDomsPretty = pretty_seperated([p_nl_hard],
        map(pretty_var_domain(PrettyInfo, Domains, VarComments),
            to_sorted_list(Vars))).

:- func pretty_var_domain(pretty_info, map(svar, domain), map(svar, string),
    svar) = pretty.

pretty_var_domain(PrettyInfo, Domains, VarComments, Var) = Pretty :-
    Pretty = p_expr(
        [unify(pretty_var(PrettyInfo, Var),
            pretty_domain(PrettyInfo, Domain))] ++
        Comment),
    Domain = get_domain(Domains, Var),
    ( if map.search(VarComments, Var, VarComment) then
        Comment = [p_str(" # "), p_str(VarComment)]
    else
        Comment = []
    ).

:- func pretty_var(pretty_info, svar) = pretty.

pretty_var(PrettyInfo, Var) = p_str(String) :-
    ( Var = v_named(NamedVar),
        Name = get_var_name(PrettyInfo ^ pi_varmap, NamedVar),
        String = format("'Sv_%s", [s(Name)])
    ;
        ( Var = v_output(N),
            Label = "Output"
        ; Var = v_anon(N),
            Label = "Anon"
        ; Var = v_type_var(N),
            Label = "TypeVar"
        ),
        String = format("'%s_%d", [s(Label), i(N)])
    ).

:- func pretty_var_user(pretty_info, svar_user) = pretty.

pretty_var_user(PrettyInfo, vu_named(NamedVar)) = p_str(String) :-
    Name = get_var_name(PrettyInfo ^ pi_varmap, NamedVar),
    String = format("'Sv_%s", [s(Name)]).
pretty_var_user(_, vu_output(N)) = p_str(String) :-
    String = format("'Output_%d", [i(N)]).

:- func pretty_domain(pretty_info, domain) = pretty.

pretty_domain(_,          d_free) = p_str("_").
pretty_domain(_,          d_builtin(Builtin)) = p_str(string(Builtin)).
pretty_domain(PrettyInfo, d_type(TypeId, Domains)) =
    pretty_user_type(TypeId, map(pretty_domain(PrettyInfo), Domains)).
pretty_domain(PrettyInfo, d_func(Inputs, Outputs, MaybeResources)) =
    pretty_func_type(PrettyInfo, map(pretty_domain(PrettyInfo), Inputs),
        map(pretty_domain(PrettyInfo), Outputs), MaybeResources).
pretty_domain(_,          d_univ_var(TypeVar)) = p_str("'" ++ TypeVar).

:- func pretty_user_type(type_id, list(pretty)) = pretty.

pretty_user_type(type_id(TypeNo), Args) =
        pretty_callish(Functor, Args) :-
    Functor = p_str(format("type_%i", [i(TypeNo)])).

:- func pretty_domain_or_svar(pretty_info, domain, svar) = pretty.

pretty_domain_or_svar(Info, Domain, SVar) =
    ( if Domain = d_free then
        pretty_var(Info, SVar)
    else
        pretty_domain(Info, Domain)
    ).

:- func pretty_func_type(pretty_info, list(pretty), list(pretty),
    maybe_resources) = pretty.

pretty_func_type(PrettyInfo, Inputs, Outputs, MaybeResources)
        = Pretty :-
    Pretty = func_pretty_template(p_str("func"), Inputs, Outputs, PrettyUses,
        PrettyObserves),
    ( MaybeResources = unknown_resources,
        PrettyUses = [],
        PrettyObserves = []
    ; MaybeResources = resources(Uses, Observes),
        Core = PrettyInfo ^ pi_core,
        PrettyUses = map(resource_pretty(Core), to_sorted_list(Uses)),
        PrettyObserves = map(resource_pretty(Core), to_sorted_list(Observes))
    ).

:- func unify(pretty, pretty) = pretty.

unify(A, B) = p_expr([A, p_str(" = "), B]).

:- func pretty_context_comment(context) = list(pretty).

pretty_context_comment(C) =
    ( if is_nil_context(C) then
        []
    else
        [p_comment(singleton("% "), [p_str(context_string(C))]), p_nl_hard]
    ).

%-----------------------------------------------------------------------%

solve(Core, Varmap, problem(_, VarComments, Constraints)) = Result :-
    PrettyInfo = pretty_info(Varmap, Core),
    Problem0 = problem(AllVars, VarComments, init, PrettyInfo),
    % Flatten to CNF form.
    flattern(Constraints, Clauses, Aliases),
    AllVars = union_list(map(clause_vars, Clauses)),

    trace [io(!IO), compile_time(flag("typecheck_solve"))] (
        Pretties = [p_str("Typecheck solver starting"), p_nl_double,
            p_expr([p_str("Problem:"), p_nl_hard] ++
                pretty_problem(PrettyInfo, sort(Constraints))),
            p_nl_hard,
            p_expr([p_str("Aliases:"), p_nl_hard] ++
                pretty_comma_seperated(
                    map(pretty_simple_alias(pretty_var_user(PrettyInfo)),
                        Aliases))),
            p_nl_hard,
            p_expr([p_str("Flattened problem:"), p_nl_hard] ++
                pretty_problem_flat(PrettyInfo, Clauses))],
        write_string(pretty_str(Pretties), !IO),
        nl(!IO)
    ),

    run_clauses(Clauses, Problem0, Result0),
    ( Result0 = ok(Problem),
        trace [io(!IO), compile_time(flag("typecheck_solve"))] (
            write_string("\nsolver finished\n", !IO)
        ),
        foldl(build_results(Problem ^ ps_domains), AllVars, init, Solution0),
        foldl((pred(simple_alias(A, B)::in, Map0::in, Map::out) is det :-
                map.lookup(Map0, A, V),
                map.det_insert(B, V, Map0, Map)
            ), Aliases, Solution0, Solution),
        Result = ok(Solution)
    ; Result0 = failed(Context, Why),
        ( Why = mismatch(Domain1, Domain2),
            Result = return_error(Context, ce_type_unification_failed(
                pretty_domain(PrettyInfo, Domain1),
                pretty_domain(PrettyInfo, Domain2)))
        ; Why = occurs(OccursVar, UserType, ArgDomains, ArgVars),
            Result = return_error(Context, ce_type_unification_occurs(
                pretty_var(PrettyInfo, OccursVar),
                pretty_user_type(UserType,
                    map_corresponding(pretty_domain_or_svar(PrettyInfo),
                        ArgDomains, ArgVars))))
        )
    ).

    % Note that this is probably O(N^2).  While the solver itself is
    % O(NlogN).  We do this because it simplifies the problem and allows
    % easier tracing of the type checker.  The checker also has larger
    % constant factors so we'd need to measure before optimising anyway.
    %
    % The returned list of aliases (to be un-applied in order) contains only
    % those involving user variables.  This should not be a problem since
    % those sort before other variables and therefore a normalised list of
    % clauses will place them first, causing substitutions to keep those in
    % the program.
    %
:- pred flattern(list(constraint)::in, list(clause)::out,
    list(simple_alias(svar_user))::out) is det.

flattern(Constraints, !:Clauses, Aliases) :-
    !:Clauses = to_sorted_list(to_normal_form(conj(Constraints))),
    flattern_2(!Clauses, [], Aliases).

:- pred flattern_2(list(clause)::in, list(clause)::out,
        list(simple_alias(svar_user))::in, list(simple_alias(svar_user))::out)
    is det.

flattern_2(!Clauses, !Aliases) :-
    ( if remove_first_match_map(is_simple_alias, Alias, !Clauses) then
        substitute(Alias, !Clauses),
        simple_alias(To0, From0) = Alias,
        ( if
            svar_to_svar_user(To0, To),
            svar_to_svar_user(From0, From)
        then
            !:Aliases = [simple_alias(To, From) | !.Aliases]
        else
            true
        ),
        foldl(simplify_clause, !.Clauses, init, ClausesSet),
        !:Clauses = to_sorted_list(ClausesSet),
        flattern_2(!Clauses, !Aliases)
    else
        true
    ).

%-----------------------------------------------------------------------%

:- type simple_alias(V)
    --->    simple_alias(V, V).

:- type simple_alias == simple_alias(svar).

:- pred is_simple_alias(clause::in, simple_alias::out) is semidet.

is_simple_alias(single(cl_var_var(Var1, Var2, _)), simple_alias(Var1, Var2)).

:- func pretty_simple_alias(func(V) = pretty, simple_alias(V)) =
    pretty.

pretty_simple_alias(PrettyVar, simple_alias(V1, V2)) =
    unify(PrettyVar(V1), PrettyVar(V2)).

:- pred substitute(simple_alias::in, list(clause)::in, list(clause)::out)
    is det.

substitute(Alias, !Clauses) :-
    map(substitute_clause(Alias), !Clauses).

:- pred substitute_clause(simple_alias::in, clause::in, clause::out) is det.

substitute_clause(Alias, !Clause) :-
    ( !.Clause = single(Lit0),
        substitute_lit(Alias, Lit0, Lit),
        !:Clause = single(Lit)
    ; !.Clause = disj(Lit0, Lits0),
        substitute_lit(Alias, Lit0, Lit),
        map(substitute_lit(Alias), Lits0, Lits),
        !:Clause = disj(Lit, Lits)
    ).

:- pred substitute_lit(simple_alias::in,
    constraint_literal::in, constraint_literal::out) is det.

substitute_lit(_,     cl_true, cl_true).
substitute_lit(Alias, cl_var_builtin(V0, B, C), cl_var_builtin(V, B, C)) :-
    substitute_var(Alias, V0, V).
substitute_lit(Alias,
        cl_var_usertype(V0, TypeId, Vars0, Context),
        cl_var_usertype(V, TypeId, Vars, Context)) :-
    substitute_var(Alias, V0, V),
    map(substitute_var(Alias), Vars0, Vars).
substitute_lit(Alias,
        cl_var_func(V0, Is0, Os0, Res, C), cl_var_func(V, Is, Os, Res, C)) :-
    substitute_var(Alias, V0, V),
    map(substitute_var(Alias), Is0, Is),
    map(substitute_var(Alias), Os0, Os).
substitute_lit(Alias,
        cl_var_free_type_var(V0, TV, C), cl_var_free_type_var(V, TV, C)) :-
    substitute_var(Alias, V0, V).
substitute_lit(Alias, cl_var_var(Va0, Vb0, C),
        simplify_literal(cl_var_var(Va, Vb, C))) :-
    substitute_var(Alias, Va0, Va),
    substitute_var(Alias, Vb0, Vb).

:- pred substitute_var(simple_alias::in, svar::in, svar::out) is det.

substitute_var(simple_alias(V1, V2), V0, V) :-
    ( if V2 = V0 then
        V = V1
    else
        V = V0
    ).

%-----------------------------------------------------------------------%

:- type clause
    --->    single(constraint_literal)
    ;       disj(constraint_literal, list(constraint_literal)).

%-----------------------------------------------------------------------%

:- func to_normal_form(constraint) = set(clause).

to_normal_form(single(Lit0)) = Clauses :-
    Lit = simplify_literal(Lit0),
    ( if Lit = cl_true then
        Clauses = set.init
    else
        Clauses = make_singleton_set(single(Lit))
    ).
to_normal_form(conj(Conjs0)) = union_list(map(to_normal_form, Conjs0)).
to_normal_form(disj(Disjs0)) = Conj :-
    Disjs1 = map(to_normal_form, Disjs0),
    ( Disjs1 = [],
        unexpected($file, $pred, "Empty disjunction")
    ; Disjs1 = [Conj]
    ; Disjs1 = [D | Ds@[_ | _]],
        Conj = foldl(disj_to_nf, Ds, D)
    ).

:- pred simplify_clause(clause::in,
    big_set(clause)::in, big_set(clause)::out) is det.

simplify_clause(single(Lit0), !Clauses) :-
    Lit = simplify_literal(Lit0),
    ( if Lit = cl_true then
        true
    else
        insert(single(Lit), !Clauses)
    ).
simplify_clause(Disj@disj(_, _), !Clauses) :-
    % We don't need to simplify within disjunctions.
    insert(Disj, !Clauses).

    % Create the disjunction by combining a pair of disjuncts at a time.
    %
    % It is in the form (A1 /\ A2 /\ ...) v (B1 /\ B2 /\ ...)
    %
    % We take each literal in the first clause, and factor it into the
    % second clause. Resulting in:
    %
    % (A1 v B1) /\ (A1 v B2) /\ (A2 v B1) /\ (A2 v B2)
    %
:- func disj_to_nf(set(clause), set(clause)) = set(clause).

disj_to_nf(ConjsA, ConjsB) = set_cross(disj_to_nf_clause, ConjsA, ConjsB).

:- func set_cross(func(A, B) = C, set(A), set(B)) = set(C).

set_cross(F, As, Bs) =
    union_list(map(
        (func(A) = list_to_set(map((func(B) = F(A, B)), to_sorted_list(Bs)))),
        to_sorted_list(As))).

:- func disj_to_nf_clause(clause, clause) = clause.

disj_to_nf_clause(single(D1), single(D2)) = disj(D1, [D2]).
disj_to_nf_clause(single(D1), disj(D2, Ds3)) = disj(D1, [D2 | Ds3]).
disj_to_nf_clause(disj(D1, Ds2), single(D3)) = disj(D1, [D3 | Ds2]).
disj_to_nf_clause(disj(D1, Ds2), disj(D3, Ds4)) =
    disj(D1, [D3 | Ds2 ++ Ds4]).

:- func simplify_literal(constraint_literal) = constraint_literal.

simplify_literal(cl_true) = cl_true.
simplify_literal(L@cl_var_builtin(_, _, _)) = L.
simplify_literal(L@cl_var_func(_, _, _, _, _)) = L.
simplify_literal(L@cl_var_usertype(_, _, _, _)) = L.
simplify_literal(L@cl_var_free_type_var(_, _, _)) = L.
simplify_literal(cl_var_var(VarA, VarB, Context)) = Literal :-
    compare(C, VarA, VarB),
    ( C = (=),
        Literal = cl_true
    ; C = (<),
        Literal = cl_var_var(VarA, VarB, Context)
    ; C = (>),
        Literal = cl_var_var(VarB, VarA, Context)
    ).

%-----------------------------------------------------------------------%

:- func clause_vars(clause) = set(svar).

clause_vars(single(Lit)) = literal_vars(Lit).
clause_vars(disj(Lit, Lits)) =
    literal_vars(Lit) `union` union_list(map(literal_vars, Lits)).

:- func literal_vars(constraint_literal) = set(svar).

literal_vars(cl_true) = init.
literal_vars(cl_var_builtin(Var, _, _)) = make_singleton_set(Var).
literal_vars(cl_var_func(Var, Inputs, Outputs, _, _)) =
    make_singleton_set(Var) `union` from_list(Inputs) `union`
        from_list(Outputs).
literal_vars(cl_var_usertype(Var, _, ArgVars, _)) =
    insert(from_list(ArgVars), Var).
literal_vars(cl_var_free_type_var(Var, _, _)) = make_singleton_set(Var).
literal_vars(cl_var_var(VarA, VarB, _)) = from_list([VarA, VarB]).

%-----------------------------------------------------------------------%

:- type problem_solving
    --->    problem(
                ps_vars             :: set(svar),
                ps_var_comments     :: map(svar, string),
                ps_domains          :: map(svar, domain),
                ps_pretty_info      :: pretty_info
                % Not currently using propagators.
                % p_propagators   :: map(svar, set(propagator(V)))
            ).

:- type problem_result
    --->    ok(problem_solving)
    ;       failed(context, why_failed).

:- type why_failed
    --->    mismatch(domain, domain)
    ;       occurs(svar, type_id, list(domain), list(svar)).

% We're not currently using propagators in the solver.
% :- type propagator(V)
%     --->    propagator(constraint).

:- pred run_clauses(list(clause)::in,
    problem_solving::in, problem_result::out) is det.

run_clauses(Clauses, Problem, Result) :-
    run_clauses(Clauses, [], length(Clauses), domains_not_updated, Problem,
        Result).

:- type domains_updated
    --->    domains_not_updated
    ;       domains_updated.

    % Run the clauses until we can't make any further progress.
    %
:- pred run_clauses(list(clause)::in, list(clause)::in, int::in,
    domains_updated::in, problem_solving::in, problem_result::out) is det.

run_clauses([], [], _, _, Problem, ok(Problem)) :-
    trace [io(!IO), compile_time(flag("typecheck_solve"))] (
        write_string("\nNo more clauses\n", !IO)
    ).
run_clauses([], Cs@[_ | _], OldLen, Updated, Problem, Result) :-
    Len = length(Cs),
    ( if
        % Before running the delayed clauses we check to see if we are
        % indeed making progress.
        Len < OldLen ;
        Updated = domains_updated
    then
        trace [io(!IO), compile_time(flag("typecheck_solve"))] (
            format("Running %d delayed clauses\n", [i(Len)], !IO)
        ),
        run_clauses(reverse(Cs), [], Len, domains_not_updated, Problem, Result)
    else if
        % We can accept the solution if the only unbound variables are
        % potentially existentially quantified.  If they aren't then the
        % the typechecker itself will be able to raise an error.
        all [Var] (
            member(Var, Problem ^ ps_vars) =>
            (
                require_complete_switch [Var]
                ( Var = v_anon(_)
                ; Var = v_type_var(_)
                ;
                    ( Var = v_named(_)
                    ; Var = v_output(_)
                    ),
                    Ground = groundness(
                        get_domain(Problem ^ ps_domains, Var)),
                    require_complete_switch [Ground]
                    ( Ground = ground
                    ; Ground = ground_maybe_resources
                    ; Ground = bound_with_holes_or_free,
                        false
                    )
                )
            )
        )
    then
        trace [io(!IO), compile_time(flag("typecheck_solve"))] (
            write_string("Delayed goals probably don't matter\n", !IO)
        ),
        Result = ok(Problem)
    else
        PrettyInfo = Problem ^ ps_pretty_info,
        util.exception.sorry($file, $pred,
            append_list(["Floundering\n"] ++ list(
                pretty([
                    p_str(format("Floundering %d >= %d and %s",
                        [i(Len), i(OldLen), s(string(Updated))])),
                    p_nl_hard,
                    p_expr([p_str("Remaining constraints:"), p_nl_hard] ++
                        pretty_problem_flat(PrettyInfo, Cs)), p_nl_hard,
                    pretty_store(Problem), p_nl_hard])
            )))
    ).
run_clauses([C | Cs], Delays0, ProgressCheck, Updated0,
        !.Problem, Result) :-
    run_clause(C, Delays0, Delays, Updated0, Updated, !.Problem, ClauseResult),
    ( ClauseResult = ok(!:Problem),
        run_clauses(Cs, Delays, ProgressCheck, Updated, !.Problem, Result)
    ; ClauseResult = failed(_, _),
        Result = ClauseResult
    ).

:- pred run_clause(clause::in, list(clause)::in, list(clause)::out,
    domains_updated::in, domains_updated::out,
    problem_solving::in, problem_result::out) is det.

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
    ; Success = failed(Context, Why),
        Result = failed(Context, Why)
    ; Success = failed_disj,
        compile_error($file, $pred, "Failed disjunction")
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
    % unique solution to the type problem, therefore we need _exactly one_
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
:- pred run_disj(list(constraint_literal)::in, executed::out,
    problem_solving::in, problem_solving::out) is det.

run_disj(Disjs, Delayed, !Problem) :-
    trace [io(!IO), compile_time(flag("typecheck_solve"))] (
        io.write_string("Running disjunction\n", !IO)
    ),
    run_disj(Disjs, no, Delayed, !Problem),
    trace [io(!IO), compile_time(flag("typecheck_solve"))] (
        io.write_string("Finished disjunction\n", !IO)
    ).

:- pred run_disj(list(constraint_literal)::in,
    maybe(constraint_literal)::in, executed::out,
    problem_solving::in, problem_solving::out) is det.

run_disj([], no, failed_disj, !Problem).
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
    ;
        ( Success0 = failed(_, _) % XXX: Keep the reason.
        ; Success0 = failed_disj
        ),
        run_disj(Lits, MaybeDelayed, Success, Problem0, Problem)
    ).

:- pred run_disj_all_false(list(constraint_literal)::in,
    maybe(constraint_literal)::in, problem_solving::in, executed::out) is det.

run_disj_all_false([], no, _, success_not_updated).
run_disj_all_false([], yes(Lit), Problem, Success) :-
    run_literal(Lit, Success0, Problem, _),
    (
        ( Success0 = success_updated
        ; Success0 = success_not_updated
        ),
        Success = delayed_not_updated
    ;
        ( Success0 = failed(_, _)
        ; Success0 = failed_disj
        ),
        Success = success_not_updated
    ;
        ( Success0 = delayed_not_updated
        ; Success0 = delayed_updated
        ),
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
    ;
        ( Success0 = failed(_, _)
        ; Success0 = failed_disj
        ),
        run_disj_all_false(Lits, MaybeDelayed, Problem, Success)
    ).

:- type executed
    --->    success_updated
    ;       success_not_updated
    ;       failed(context, why_failed)
    ;       failed_disj

            % We've updated the problem but something in this constraint
            % can't be run now, so revisit the whole constraint later.
    ;       delayed_updated

    ;       delayed_not_updated.

:- inst executed_no_delay for executed/0
    --->    success_updated
    ;       success_not_updated
    ;       failed(ground, ground)
    ;       failed_disj.

:- pred is_updated(executed::in) is semidet.

is_updated(success_updated).
is_updated(delayed_updated).

:- pred mark_delayed(executed::in, executed::out) is det.

mark_delayed(success_updated,     delayed_updated).
mark_delayed(success_not_updated, delayed_not_updated).
mark_delayed(Failed,              _) :-
    ( Failed = failed(_, _)
    ; Failed = failed_disj
    ),
    unexpected($file, $pred, "Cannot delay after failure").
mark_delayed(delayed_updated,     delayed_updated).
mark_delayed(delayed_not_updated, delayed_not_updated).

:- pred mark_updated(executed::in, executed::out) is det.

mark_updated(success_updated,     success_updated).
mark_updated(success_not_updated, success_updated).
mark_updated(Failed,              _) :-
    ( Failed = failed(_, _)
    ; Failed = failed_disj
    ),
    unexpected($file, $pred, "Cannot update after failure").
mark_updated(delayed_updated,     delayed_updated).
mark_updated(delayed_not_updated, delayed_updated).

    % Run the literal immediately.  Directly update domains and add
    % propagators.
    %
:- pred run_literal(constraint_literal::in, executed::out,
    problem_solving::in, problem_solving::out) is det.

run_literal(Lit, Success, !Problem) :-
    trace [io(!IO), compile_time(flag("typecheck_solve"))] (
        PrettyInfo = !.Problem ^ ps_pretty_info,
        PrettyTitle = [p_str("Running step"), p_nl_hard],
        PrettyDomains = pretty_store(!.Problem),
        PrettyRun = p_expr([p_str("Run:"), p_nl_hard] ++
            pretty_literal(PrettyInfo, Lit)),
        io.write_string(pretty_str(
            PrettyTitle ++ [PrettyDomains, p_nl_hard, PrettyRun, p_nl_hard]),
            !IO)
    ),
    run_literal_2(Lit, Success, !Problem),
    trace [io(!IO), compile_time(flag("typecheck_solve"))] (
        io.format("  %s\n", [s(string(Success))], !IO)
    ).

:- pred run_literal_2(constraint_literal::in, executed::out,
    problem_solving::in, problem_solving::out) is det.

run_literal_2(cl_true, success_not_updated, !Problem).
run_literal_2(cl_var_builtin(Var, Builtin, Context), Success, !Problem) :-
    Domains0 = !.Problem ^ ps_domains,
    OldDomain = get_domain(Domains0, Var),
    NewDomain = d_builtin(Builtin),
    ( OldDomain = d_free,
        map.set(Var, NewDomain, Domains0, Domains),
        !Problem ^ ps_domains := Domains,
        Success = success_updated
    ; OldDomain = d_builtin(ExistBuiltin),
        ( if Builtin = ExistBuiltin then
            Success = success_not_updated
        else
            Success = failed(Context, mismatch(OldDomain, NewDomain))
        )
    ;
        ( OldDomain = d_type(_, _)
        ; OldDomain = d_func(_, _, _)
        ; OldDomain = d_univ_var(_)
        ),
        Success = failed(Context, mismatch(OldDomain, NewDomain))
    ).
run_literal_2(cl_var_var(Var1, Var2, Context), Success, !Problem) :-
    PrettyInfo = !.Problem ^ ps_pretty_info,
    DomainMap0 = !.Problem ^ ps_domains,
    Dom1 = get_domain(DomainMap0, Var1),
    Dom2 = get_domain(DomainMap0, Var2),
    trace [io(!IO), compile_time(flag("typecheck_solve"))] (
        Pretty = [p_str("  left: "), pretty_domain(PrettyInfo, Dom1),
            p_str(" right: "), pretty_domain(PrettyInfo, Dom2), p_nl_hard],
        write_string(pretty_str(Pretty), !IO)
    ),
    Dom = unify_domains(Dom1, Dom2),
    ( Dom = failed,
        Success = failed(Context, mismatch(Dom1, Dom2))
    ; Dom = unified(NewDom, Updated),
        some [!Success] (
            !:Success = success_not_updated,
            ( Updated = delayed,
                mark_delayed(!Success)
            ;
                ( Updated = new_domain,
                    set(Var1, NewDom, DomainMap0, DomainMap1),
                    set(Var2, NewDom, DomainMap1, DomainMap),
                    !Problem ^ ps_domains := DomainMap,
                    mark_updated(!Success)
                ; Updated = old_domain
                ),
                Groundness = groundness(NewDom),
                (
                    ( Groundness = bound_with_holes_or_free
                    ; Groundness = ground_maybe_resources
                    ),
                    mark_delayed(!Success)
                ; Groundness = ground
                )
            ),
            Success = !.Success
        ),
        trace [io(!IO), compile_time(flag("typecheck_solve"))] (
            Pretty = [p_str("  new: "), pretty_domain(PrettyInfo, NewDom),
                p_nl_hard],
            write_string(pretty_str(Pretty), !IO)
        )
    ).
run_literal_2(cl_var_free_type_var(Var, TypeVar, Context), Success, !Problem) :-
    Domains0 = !.Problem ^ ps_domains,
    OldDomain = get_domain(Domains0, Var),
    NewDomain = d_univ_var(TypeVar),
    ( OldDomain = d_free,
        map.set(Var, NewDomain, Domains0, Domains),
        !Problem ^ ps_domains := Domains,
        Success = success_updated
    ; OldDomain = d_univ_var(ExistTypeVar),
        ( if TypeVar = ExistTypeVar then
            Success = success_not_updated
        else
            Success = failed(Context, mismatch(OldDomain, NewDomain))
        )
    ;
        ( OldDomain = d_type(_, _)
        ; OldDomain = d_func(_, _, _)
        ; OldDomain = d_builtin(_)
        ),
        Success = failed(Context, mismatch(OldDomain, NewDomain))
    ).
run_literal_2(cl_var_usertype(Var, TypeUnify, ArgsUnify, Context), Success,
		!Problem) :-
    Domains0 = !.Problem ^ ps_domains,
    Domain = get_domain(Domains0, Var),
    ArgDomainsUnify = map(get_domain(Domains0), ArgsUnify),
    NewDomain = d_type(TypeUnify, ArgDomainsUnify),
    ( if member(Var, ArgsUnify) then
        Success = failed(Context, occurs(Var, TypeUnify, ArgDomainsUnify,
            ArgsUnify))
    else
        ( Domain = d_free,
            map.set(Var, NewDomain, Domains0, Domains),
            !Problem ^ ps_domains := Domains,
            Groundness = groundness(NewDomain),
            (
                ( Groundness = bound_with_holes_or_free
                ; Groundness = ground_maybe_resources
                ),
                Success = delayed_updated
            ; Groundness = ground,
                Success = success_updated
            )
        ; Domain = d_type(Type0, Args0),
            ( if Type0 = TypeUnify then
                % Save any information from this domain back into the variables
                % being unified with the type's arguments.
                update_args(Context, Args0, ArgsUnify, success_not_updated,
                    Success0, !.Problem, MaybeProblem),
                ( if Success0 = failed(C, Why) then
                    Success = failed(C, Why)
                else
                    ( if is_updated(Success0) then
                        !:Problem = MaybeProblem
                    else
                        true
                    ),
                    Args = map(get_domain(MaybeProblem ^ ps_domains), ArgsUnify),
                    ( if Args \= Args0 then
                        map.set(Var, d_type(Type0, Args), !.Problem ^ ps_domains,
                            Domains),
                        !Problem ^ ps_domains := Domains,
                        mark_updated(Success0, Success)
                    else
                        % We can use the delayed/success from update_args, since
                        % it depends on groundness.
                        Success = Success0
                    )
                )
            else
                Success = failed(Context, mismatch(Domain, NewDomain))
            )
        ;
            ( Domain = d_builtin(_)
            ; Domain = d_univ_var(_)
            ; Domain = d_func(_, _, _)
            ),
            Success = failed(Context, mismatch(Domain, NewDomain))
        )
    ).
run_literal_2(
        cl_var_func(Var, InputsUnify, OutputsUnify, MaybeResourcesUnify,
            Context),
        Success, !Problem) :-
    ( if
        member(Var, InputsUnify) ;
        member(Var, OutputsUnify)
    then
        compile_error($file, $pred, "Occurs check")
    else
        Domains0 = !.Problem ^ ps_domains,
        Domain = get_domain(Domains0, Var),
        InputDomainsUnify = map(get_domain(Domains0), InputsUnify),
        OutputDomainsUnify = map(get_domain(Domains0), OutputsUnify),
        NewDomainUnify = d_func(InputDomainsUnify, OutputDomainsUnify,
            MaybeResourcesUnify),
        ( Domain = d_free,
            map.set(Var, NewDomainUnify, Domains0, Domains),
            !Problem ^ ps_domains := Domains,
            Groundness = groundness(NewDomainUnify),
            (
                ( Groundness = bound_with_holes_or_free
                ; Groundness = ground_maybe_resources
                ),
                Success = delayed_updated
            ; Groundness = ground,
                Success = success_updated
            )
        ; Domain = d_func(Inputs0, Outputs0, MaybeResources0),
            some [!TmpProblem, !Success] (
                !:TmpProblem = !.Problem,
                ( if
                    length(Inputs0, InputsLen),
                    length(InputsUnify, InputsLen),
                    length(Outputs0, OutputsLen),
                    length(OutputsUnify, OutputsLen)
                then
                    update_args(Context, Inputs0, InputsUnify,
                        success_not_updated, !:Success, !TmpProblem),
                    update_args(Context, Outputs0, OutputsUnify,
                        !Success, !TmpProblem),
                    ( if !.Success \= failed(_, _) then
                        ( if is_updated(!.Success) then
                            !:Problem = !.TmpProblem
                        else
                            true
                        ),
                        Inputs = map(get_domain(!.TmpProblem ^ ps_domains),
                            InputsUnify),
                        Outputs = map(get_domain(!.TmpProblem ^ ps_domains),
                            OutputsUnify),
                        unify_resources(MaybeResourcesUnify, MaybeResources0,
                            MaybeResources, _),
                        ( if
                            Inputs \= Inputs0 ;
                            Outputs \= Outputs0 ;
                            MaybeResources \= MaybeResources0
                            % We don't compare with MaybeResourcesUnify since we
                            % can't update that.
                        then
                            map.set(Var, d_func(Inputs, Outputs,
                                    MaybeResources),
                                !.Problem ^ ps_domains, Domains),
                            !Problem ^ ps_domains := Domains,
                            mark_updated(!Success)
                        else
                            % We can use the delayed/success from
                            % update_args, since it depends on groundness.
                            true
                        )
                    else
                        true
                    ),
                    Success = !.Success
                else
                    Success = failed(Context,
                        mismatch(Domain, NewDomainUnify))
                )
            )
        ;
            ( Domain = d_type(_, _)
            ; Domain = d_builtin(_)
            ; Domain = d_univ_var(_)
            ),
            Success = failed(Context, mismatch(Domain, NewDomainUnify))
        )
    ).

:- pred update_args(context::in, list(domain)::in, list(svar)::in,
    executed::in, executed::out,
    problem_solving::in, problem_solving::out) is det.

update_args(_, [], [], !Success, !Problem).
update_args(_, [], [_ | _], !Success, !Problem) :-
    unexpected($file, $pred, "Mismatched type argument lists").
update_args(_, [_ | _], [], !Success, !Problem) :-
    unexpected($file, $pred, "Mismatched type argument lists").
update_args(Context, [D0 | Ds], [V | Vs], !Success, !Problem) :-
    (
        ( !.Success = failed(_, _)
        ; !.Success = failed_disj
        )
    ;
        ( !.Success = success_updated
        ; !.Success = success_not_updated
        ; !.Success = delayed_updated
        ; !.Success = delayed_not_updated
        ),

        Domains0 = !.Problem ^ ps_domains,
        VD = get_domain(Domains0, V),
        MaybeD = unify_domains(D0, VD),
        ( MaybeD = failed,
            !:Success = failed(Context, mismatch(D0, VD))
        ; MaybeD = unified(D, Updated),
            ( Updated = delayed,
                mark_delayed(!Success)
            ; Updated = old_domain,
                true
            ; Updated = new_domain,
                Groundness = groundness(D),
                (
                    ( Groundness = bound_with_holes_or_free
                    ; Groundness = ground_maybe_resources
                    ),
                    mark_delayed(!Success)
                ; Groundness = ground
                ),
                map.set(V, D, Domains0, Domains),
                !Problem ^ ps_domains := Domains,
                mark_updated(!Success)
            )
        ),

        update_args(Context, Ds, Vs, !Success, !Problem)
    ).

%-----------------------------------------------------------------------%

:- pred build_results(map(svar, domain)::in, svar::in,
    map(svar_user, type_)::in, map(svar_user, type_)::out) is det.

build_results(_,   v_anon(_),     !Results).
build_results(_,   v_type_var(_), !Results). % XXX
build_results(Map, Var,           !Results) :-
    ( Var = v_named(V),
        VarUser = vu_named(V)
    ; Var = v_output(N),
        VarUser = vu_output(N)
    ),
    lookup(Map, Var, Domain),
    Type = domain_to_type(Var, Domain),
    det_insert(VarUser, Type, !Results).

:- pred svar_to_svar_user(svar::in, svar_user::out) is semidet.

svar_to_svar_user(v_named(N), vu_named(N)).
svar_to_svar_user(v_output(O), vu_output(O)).

:- func domain_to_type(V, domain) = type_.

domain_to_type(Var, d_free) =
    unexpected($file, $pred,
        string.format("Free variable in '%s'", [s(string(Var))])).
domain_to_type(_, d_builtin(Builtin)) = builtin_type(Builtin).
domain_to_type(Var, d_type(TypeId, Args)) =
    type_ref(TypeId, map(domain_to_type(Var), Args)).
domain_to_type(Var, d_func(Inputs, Outputs, MaybeResources)) = Type :-
    ( MaybeResources = unknown_resources,
        % The resource-checking pass will fix this.
        Used = set.init,
        Observed = set.init
    ; MaybeResources = resources(Used, Observed)
    ),
    Type = func_type(map(domain_to_type(Var), Inputs),
        map(domain_to_type(Var), Outputs), Used, Observed).
domain_to_type(_, d_univ_var(TypeVar)) = type_variable(TypeVar).

%-----------------------------------------------------------------------%

:- type domain
    --->    d_free
    ;       d_builtin(builtin_type)
    ;       d_type(type_id, list(domain))
    ;       d_func(list(domain), list(domain), maybe_resources)
            % A type variable from the function's signature.
    ;       d_univ_var(type_var).

:- func get_domain(map(svar, domain), svar) = domain.

get_domain(Map, Var) =
    ( if map.search(Map, Var, Domain) then
        Domain
    else
        d_free
    ).

:- type groundness
    --->    bound_with_holes_or_free
            % Type information is ground but resource information always has
            % unknown groundness.
    ;       ground_maybe_resources
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
groundness(d_func(Inputs, Outputs, _)) = Groundness :-
    ( if
        some [Arg] (
            ( member(Arg, Inputs) ; member(Arg, Outputs) ),
            ArgGroundness = groundness(Arg),
            ArgGroundness = bound_with_holes_or_free
        )
    then
        Groundness = bound_with_holes_or_free
    else
        Groundness = ground_maybe_resources
    ).
groundness(d_univ_var(_)) = ground.

%-----------------------------------------------------------------------%

:- type unify_result(D)
    --->    unified(D, domain_status)
    ;       failed.

:- type domain_status
            % new_domain can include delays
    --->    new_domain
    ;       old_domain
    ;       delayed.

:- type unify_result == unify_result(domain).

:- func unify_domains(domain, domain) = unify_result.

unify_domains(Dom1, Dom2) = Dom :-
    ( Dom1 = d_free,
        ( Dom2 = d_free,
            Dom = unified(d_free, delayed)
        ;
            ( Dom2 = d_builtin(_)
            ; Dom2 = d_type(_, _)
            ; Dom2 = d_func(_, _, _)
            ; Dom2 = d_univ_var(_)
            ),
            Dom = unified(Dom2, new_domain)
        )
    ; Dom1 = d_builtin(Builtin1),
        ( Dom2 = d_free,
            Dom = unified(Dom1, new_domain)
        ; Dom2 = d_builtin(Builtin2),
            ( if Builtin1 = Builtin2 then
                Dom = unified(Dom1, old_domain)
            else
                Dom = failed
            )
        ;
            ( Dom2 = d_type(_, _)
            ; Dom2 = d_func(_, _, _)
            ; Dom2 = d_univ_var(_)
            ),
            Dom = failed
        )
    ; Dom1 = d_type(Type1, Args1),
        ( Dom2 = d_free,
            Dom = unified(Dom1, new_domain)
        ; Dom2 = d_type(Type2, Args2),
            ( if
                Type1 = Type2,
                length(Args1, ArgsLen),
                length(Args2, ArgsLen)
            then
                MaybeNewArgs = unify_args_domains(Args1, Args2),
                ( MaybeNewArgs = unified(Args, ArgsUpdated),
                    ( ArgsUpdated = new_domain,
                        Dom = unified(d_type(Type1, Args), new_domain)
                    ; ArgsUpdated = old_domain,
                        Dom = unified(d_type(Type1, Args1), old_domain)
                    ; ArgsUpdated = delayed,
                        Dom = unified(d_type(Type1, Args), delayed)
                    )
                ; MaybeNewArgs = failed,
                    Dom = failed
                )
            else
                Dom = failed
            )
        ;
            ( Dom2 = d_builtin(_)
            ; Dom2 = d_func(_, _, _)
            ; Dom2 = d_univ_var(_)
            ),
            Dom = failed
        )
    ; Dom1 = d_func(Inputs1, Outputs1, MaybeRes1),
        ( Dom2 = d_free,
            Dom = unified(Dom1, new_domain)
        ; Dom2 = d_func(Inputs2, Outputs2, MaybeRes2),
            ( if
                length(Inputs1, InputsLen),
                length(Inputs2, InputsLen),
                length(Outputs1, OutputsLen),
                length(Outputs2, OutputsLen)
            then
                MaybeNewInputs = unify_args_domains(Inputs1, Inputs2),
                MaybeNewOutputs = unify_args_domains(Outputs1, Outputs2),
                ( MaybeNewInputs = failed,
                    Dom = failed
                ; MaybeNewInputs = unified(Inputs, InputsUpdated),
                    ( MaybeNewOutputs = failed,
                        Dom = failed
                    ; MaybeNewOutputs = unified(Outputs, OutputsUpdated),
                        unify_resources(MaybeRes1, MaybeRes2, MaybeRes,
                            ResUpdated),
                        NewDom = d_func(Inputs, Outputs, MaybeRes),
                        Dom = unified(NewDom, greatest_domain_status(
                            greatest_domain_status(InputsUpdated,
                                OutputsUpdated),
                            ResUpdated))
                    )
                )
            else
                Dom = failed
            )
        ;
            ( Dom2 = d_builtin(_)
            ; Dom2 = d_type(_, _)
            ; Dom2 = d_univ_var(_)
            ),
            Dom = failed
        )
    ; Dom1 = d_univ_var(Var1),
        ( Dom2 = d_free,
            Dom = unified(Dom1, new_domain)
        ; Dom2 = d_univ_var(Var2),
            ( if Var1 = Var2 then
                Dom = unified(Dom1, old_domain)
            else
                Dom = failed
            )
        ;
            ( Dom2 = d_builtin(_)
            ; Dom2 = d_type(_, _)
            ; Dom2 = d_func(_, _, _)
            ),
            Dom = failed
        )
    ).

:- func unify_args_domains(list(domain), list(domain)) =
    unify_result(list(domain)).

unify_args_domains(Args1, Args2) = Doms :-
    MaybeNewArgs = map_corresponding(unify_domains, Args1, Args2),
    RevDoms = foldl(unify_args_domains_2, MaybeNewArgs,
        unified([], old_domain)),
    ( RevDoms = unified(Rev, Updated),
        Doms0 = reverse(Rev),
        Doms = unified(Doms0, Updated)
    ; RevDoms = failed,
        Doms = failed
    ).

:- func unify_args_domains_2(unify_result,
    unify_result(list(domain))) = unify_result(list(domain)).

unify_args_domains_2(A, !.R) = !:R :-
    ( !.R = failed
    ; !.R = unified(RD, Updated0),
        ( A = failed,
            !:R = failed
        ; A = unified(AD, UpdatedA),
            !:R = unified([AD | RD], greatest_domain_status(Updated0, UpdatedA))
        )
    ).

:- func greatest_domain_status(domain_status, domain_status) = domain_status.

greatest_domain_status(A, B) =
    ( if A = new_domain ; B = new_domain then
        new_domain
    else if A = delayed ; B = delayed then
        delayed
    else if A = old_domain , B = old_domain then
        old_domain
    else
        unexpected($file, $pred, "Case not covered")
    ).

%-----------------------------------------------------------------------%

:- pred unify_resources(maybe_resources::in, maybe_resources::in,
    maybe_resources::out, domain_status::out) is det.

unify_resources(unknown_resources, unknown_resources, unknown_resources,
    delayed).
unify_resources(unknown_resources, resources(Us, Os), resources(Us, Os),
    new_domain).
unify_resources(resources(Us, Os), unknown_resources, resources(Us, Os),
    new_domain).
unify_resources(resources(UsA, OsA), resources(UsB, OsB), resources(Us, Os),
        Status) :-
    Us = union(UsA, UsB),
    Os = union(OsA, OsB),
    % Technically we could subtract the used items from the observed. but
    % that may make this computation non-monotonic and I think we need that
    % for the type solver to terminate.

    ( if equal(UsA, UsB), equal(OsA, OsB) then
        Status = delayed
    else
        Status = new_domain
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

maybe_add_free_type_var(Context, Name,
        cl_var_free_type_var(Var, Name, Context), !Map) :-
    get_or_make_type_var(Name, Var, !Map).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
