%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module parsing.gen.
%
% Generate an LL(1) parser.
%
% Copyright (C) 2015 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
%-----------------------------------------------------------------------%
:- interface.

:- import_module parsing.bnf.

:- func make_parser(bnf(T, NT, R)) = parser(T, NT, R).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

:- import_module require.

:- type terminal(T)
    --->    empty
    ;       eof
    ;       terminal(T).

%-----------------------------------------------------------------------%

    % This uses the algorithm from
    % https://en.wikipedia.org/wiki/LL_parser#Constructing_an_LL.281.29_parsing_table
    %
    % Some points of clarification.
    %   + the first and follow sets have been typed as set(terminal(T)),
    %     they're stored in maps indexed by non-terminals.
    %   + When building first sets Fi(A) \ {ε} ∪ Fi(w') is assumed to be
    %     (Fi(A) \ {ε}) ∪ Fi(w')
    %
make_parser(bnf(Start, EOFTerminal, Rules0)) =
        parser(Start, EOFTerminal, Table) :-
    Rules = condense(map(expand_rules, Rules0)),
    make_first_sets(Rules, map.init, FirstSets),
    trace [compile_time(flag("debug-parser-table")), io(!IO)] (
        io.write_string("First sets:\n", !IO),
        foldl(print_terminal_set, FirstSets, !IO),
        io.nl(!IO)
    ),

    det_insert(Start, make_singleton_set(eof), map.init, FollowSets0),
    make_follow_sets(FirstSets, Rules, FollowSets0, FollowSets),
    trace [compile_time(flag("debug-parser-table")), io(!IO)] (
        io.write_string("Follow sets:\n", !IO),
        foldl(print_terminal_set, FollowSets, !IO),
        io.nl(!IO)
    ),
    foldl(make_table(EOFTerminal, FirstSets, FollowSets),
        Rules, table.init, Table).

:- type rule(T, NT, R)
    --->    rule(
                r_name      :: string,
                r_lhs       :: NT,
                r_rhs       :: list(bnf_atom(T, NT)),
                r_func      :: func(list(R)) = maybe(R)
            ).

:- func expand_rules(bnf_rule(T, NT, R)) = list(rule(T, NT, R)).

expand_rules(bnf_rule(Name, LHS, RHSs)) =
    map((func(RHS) =
        rule(Name, LHS, RHS ^ bnf_rhs, RHS ^ bnf_func)), RHSs).

%-----------------------------------------------------------------------%

:- pred make_first_sets(list(rule(T, NT, R))::in,
    map(NT, set(terminal(T)))::in, map(NT, set(terminal(T)))::out) is det.

make_first_sets(Rules, !FirstSets) :-
    map_foldl(make_first_set, Rules, Changed, !FirstSets),
    ( if member(yes, Changed) then
        make_first_sets(Rules, !FirstSets)
    else
        true
    ).

:- pred make_first_set(rule(T, NT, R)::in, bool::out,
    map(NT, set(terminal(T)))::in, map(NT, set(terminal(T)))::out) is det.

make_first_set(Rule, Changed, !FirstSets) :-
    First = first(!.FirstSets, Rule ^ r_rhs),
    LHS = Rule ^ r_lhs,
    ( if search(!.FirstSets, LHS, OldFirstSet) then
        ( if superset(OldFirstSet, First) then
            Changed = no
        else
            Changed = yes,
            det_update(LHS, union(OldFirstSet, First), !FirstSets)
        )
    else
        Changed = yes,
        det_insert(LHS, First, !FirstSets)
    ).

:- func first(map(NT, set(terminal(T))), list(bnf_atom(T, NT))) =
    set(terminal(T)).

first(_, []) = make_singleton_set(empty).
first(FirstSets, [Atom | Atoms]) = First :-
    ( Atom = t(T),
        First = make_singleton_set(terminal(T))
    ; Atom = nt(NT),
        ( if search(FirstSets, NT, NTFirstSet) then
            ( if member(empty, NTFirstSet) then
                First = union(delete(NTFirstSet, empty),
                    first(FirstSets, Atoms))
            else
                First = NTFirstSet
            )
        else
            % No information on this other set yet.
            First = set.init
        )
    ).

%-----------------------------------------------------------------------%

:- pred make_follow_sets(map(NT, set(terminal(T)))::in,
    list(rule(T, NT, R))::in,
    map(NT, set(terminal(T)))::in, map(NT, set(terminal(T)))::out) is det.

make_follow_sets(FirstSets, Rules, !FollowSets) :-
    map_foldl(make_follow_sets_2(FirstSets), Rules, Changed, !FollowSets),
    ( if member(yes, Changed) then
        make_follow_sets(FirstSets, Rules, !FollowSets)
    else
        true
    ).

:- pred make_follow_sets_2(map(NT, set(terminal(T)))::in,
    rule(T, NT, R)::in, bool::out,
    map(NT, set(terminal(T)))::in, map(NT, set(terminal(T)))::out) is det.

make_follow_sets_2(FirstSets, Rule, Changed, !FollowSets) :-
    make_follow_sets_3(Rule ^ r_rhs, Rule ^ r_lhs, FirstSets, no, Changed,
        !FollowSets).

:- pred make_follow_sets_3(list(bnf_atom(T, NT))::in, NT::in,
    map(NT, set(terminal(T)))::in, bool::in, bool::out,
    map(NT, set(terminal(T)))::in, map(NT, set(terminal(T)))::out) is det.

make_follow_sets_3([], _, _, !Changed, !FollowSets).
make_follow_sets_3([t(_) | Atoms], LHS, FirstSets, !Changed, !FollowSets) :-
    make_follow_sets_3(Atoms, LHS, FirstSets, !Changed, !FollowSets).
make_follow_sets_3([nt(NT) | Atoms], LHS, FirstSets, !Changed, !FollowSets) :-
    ( Atoms = [],
        % Copy the follows set for LHS to the follows set for NT.
        Follows = get_set_from_map_or_empty(!.FollowSets, LHS)
    ; Atoms = [_ | _],
        % Add all the nonterminals from from the first sets of Atoms to the
        % first set of NT.
        FirstInAtoms = first(FirstSets, Atoms),
        ( if member(empty, FirstInAtoms) then
            FollowsLHS = get_set_from_map_or_empty(!.FollowSets, LHS)
        else
            FollowsLHS = init
        ),
        Follows = union(difference(FirstInAtoms, set([empty, eof])),
            FollowsLHS)
    ),

    ( if search(!.FollowSets, NT, OldFollows) then
        UpdatedFollows = union(OldFollows, Follows),
        ( if equal(UpdatedFollows, OldFollows) then
            true
        else
            !:Changed = yes,
            det_update(NT, UpdatedFollows, !FollowSets)
        )
    else
        det_insert(NT, Follows, !FollowSets)
    ),

    make_follow_sets_3(Atoms, LHS, FirstSets, !Changed, !FollowSets).

:- func get_set_from_map_or_empty(map(K, set(V)), K) = set(V).

get_set_from_map_or_empty(Map, K) = Set :-
    ( if search(Map, K, SetPrime) then
        Set = SetPrime
    else
        Set = init
    ).

%-----------------------------------------------------------------------%

:- pred make_table(T::in, map(NT, set(terminal(T)))::in,
    map(NT, set(terminal(T)))::in, rule(T, NT, R)::in,
    table(T, NT, table_entry(T, NT, R))::in,
    table(T, NT, table_entry(T, NT, R))::out) is det.

make_table(EOFTerminal, FirstSets, FollowSets, Rule, !Table) :-
    NT = Rule ^ r_lhs,
    FirstSet = first(FirstSets, Rule ^ r_rhs),
    ( if member(empty, FirstSet) then
        lookup(FollowSets, NT, FollowSet),
        Terminals = union(delete(FirstSet, empty), FollowSet)
    else
        Terminals = FirstSet
    ),
    fold((pred(T0::in, Ta0::in, Ta::out) is det :-
            ( T0 = terminal(Te),
                T = Te
            ; T0 = eof,
                T = EOFTerminal
            ; T0 = empty,
                unexpected($file, $pred, "empty")
            ),
            table_insert(NT, T, rule_to_table_entry(Rule),
                Ta0, Ta)
        ), Terminals, !Table).

%-----------------------------------------------------------------------%

:- pred print_terminal_set(NT::in, set(terminal(T))::in, io::di, io::uo) is det.

print_terminal_set(NT, Set, !IO) :-
    format("%s: %s\n", [s(string(NT)), s(terminal_set_string(Set))], !IO).

:- func terminal_set_string(set(terminal(T))) = string.

terminal_set_string(Set) = join_list(", ", map(string, to_sorted_list(Set))).

%-----------------------------------------------------------------------%

:- func rule_to_table_entry(rule(T, NT, R)) =
    table_entry(T, NT, R).

rule_to_table_entry(Rule) = table_entry(StackItems) :-
    StackItems = map(atom_to_stack_item, Rule ^ r_rhs) ++
        [stack_reduce(Rule ^ r_name, length(Rule ^ r_rhs), Rule ^ r_func)].

:- func atom_to_stack_item(bnf_atom(T, NT)) = stack_item(T, NT, R).

atom_to_stack_item(t(T)) = stack_t(T).
atom_to_stack_item(nt(NT)) = stack_nt(NT).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
