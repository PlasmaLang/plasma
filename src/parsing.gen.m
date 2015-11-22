%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module parsing.gen.
%
% Generate an LL(1) parser.
%
% Copyright (C) 2015 Paul Bone
% Distributed under the terms of the GPLv2 see ../LICENSE.tools
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
make_parser(bnf(Start, Rules)) = parser(Start, Table) :-
    % _NonTerminals = all_non_terminals(Rules),
    RulesArray = array(Rules),
    NumRules = size(RulesArray),

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
    make_table(RulesArray, NumRules-1, FirstSets, FollowSets, map.init, Table).

%-----------------------------------------------------------------------%

:- pred make_first_sets(list(bnf_production(T, NT, R))::in,
    map(NT, set(terminal(T)))::in, map(NT, set(terminal(T)))::out) is det.

make_first_sets(Rules, !FirstSets) :-
    map_foldl(make_first_set, Rules, Changed, !FirstSets),
    ( member(yes, Changed) ->
        make_first_sets(Rules, !FirstSets)
    ;
        true
    ).

:- pred make_first_set(bnf_production(T, NT, R)::in, bool::out,
    map(NT, set(terminal(T)))::in, map(NT, set(terminal(T)))::out) is det.

make_first_set(Rule, Changed, !FirstSets) :-
    First = first(!.FirstSets, Rule ^ bnf_rhs),
    LHS = Rule ^ bnf_lhs,
    ( search(!.FirstSets, LHS, OldFirstSet) ->
        ( superset(OldFirstSet, First) ->
            Changed = no
        ;
            Changed = yes,
            det_update(LHS, union(OldFirstSet, First), !FirstSets)
        )
    ;
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
        ( search(FirstSets, NT, NTFirstSet) ->
            ( member(empty, NTFirstSet) ->
                First = union(delete(NTFirstSet, empty),
                    first(FirstSets, Atoms))
            ;
                First = NTFirstSet
            )
        ;
            % No information on this other set yet.
            First = set.init
        )
    ).

%-----------------------------------------------------------------------%

:- pred make_follow_sets(map(NT, set(terminal(T)))::in,
    list(bnf_production(T, NT, R))::in,
    map(NT, set(terminal(T)))::in, map(NT, set(terminal(T)))::out) is det.

make_follow_sets(FirstSets, Rules, !FollowSets) :-
    map_foldl(make_follow_sets_2(FirstSets), Rules, Changed, !FollowSets),
    ( member(yes, Changed) ->
        make_follow_sets(FirstSets, Rules, !FollowSets)
    ;
        true
    ).

:- pred make_follow_sets_2(map(NT, set(terminal(T)))::in,
    bnf_production(T, NT, R)::in, bool::out,
    map(NT, set(terminal(T)))::in, map(NT, set(terminal(T)))::out) is det.

make_follow_sets_2(FirstSets, Rule, Changed, !FollowSets) :-
    make_follow_sets_3(Rule ^ bnf_rhs, Rule ^ bnf_lhs, FirstSets, no, Changed,
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
        ( member(empty, FirstInAtoms) ->
            FollowsLHS = get_set_from_map_or_empty(!.FollowSets, LHS)
        ;
            FollowsLHS = init
        ),
        Follows = union(difference(FirstInAtoms, set([empty, eof])),
            FollowsLHS)
    ),

    ( search(!.FollowSets, NT, OldFollows) ->
        UpdatedFollows = union(OldFollows, Follows),
        ( equal(UpdatedFollows, OldFollows) ->
            true
        ;
            !:Changed = yes,
            det_update(NT, UpdatedFollows, !FollowSets)
        )
    ;
        det_insert(NT, Follows, !FollowSets)
    ),

    make_follow_sets_3(Atoms, LHS, FirstSets, !Changed, !FollowSets).

:- func get_set_from_map_or_empty(map(K, set(V)), K) = set(V).

get_set_from_map_or_empty(Map, K) = Set :-
    ( search(Map, K, SetPrime) ->
        Set = SetPrime
    ;
        Set = init
    ).

%-----------------------------------------------------------------------%

:- pred make_table(array(bnf_production(T, NT, R))::in, int::in,
    map(NT, set(terminal(T)))::in, map(NT, set(terminal(T)))::in,
    table(T, NT, R)::in, table(T, NT, R)::out) is det.

make_table(Rules, RuleNum, FirstSets, FollowSets, !Table) :-
    ( RuleNum >= 0 ->
        Rule = Rules ^ elem(RuleNum),
        NT = Rule ^ bnf_lhs,
        FirstSet = first(FirstSets, Rule ^ bnf_rhs),
        ( member(empty, FirstSet) ->
            lookup(FollowSets, NT, FollowSet),
            Terminals = union(FirstSet, FollowSet)
        ;
            Terminals = FirstSet
        ),
        fold((pred(T0::in, Ta0::in, Ta::out) is det :-
                ( T0 = terminal(Te),
                    T = terminal(Te)
                ; T0 = eof,
                    T = end_of_input
                ; T0 = empty,
                    unexpected($file, $pred, "empty")
                ),
                table_insert(NT, T, Rule, Ta0, Ta)
            ), Terminals, !Table),

        make_table(Rules, RuleNum - 1, FirstSets, FollowSets, !Table)
    ;
        true
    ).

%-----------------------------------------------------------------------%

:- pred print_terminal_set(NT::in, set(terminal(T))::in, io::di, io::uo) is det.

print_terminal_set(NT, Set, !IO) :-
    format("%s: %s\n", [s(string(NT)), s(terminal_set_string(Set))], !IO).

:- func terminal_set_string(set(terminal(T))) = string.

terminal_set_string(Set) = join_list(", ", map(string, to_sorted_list(Set))).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
