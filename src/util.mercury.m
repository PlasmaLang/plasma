%-----------------------------------------------------------------------%
% Mercury Utility code
% vim: ts=4 sw=4 et
%
% Copyright (C) Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
%-----------------------------------------------------------------------%
:- module util.mercury.

:- interface.

:- import_module bag.
:- import_module cord.
:- import_module getopt.
:- import_module io.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module set.

%-----------------------------------------------------------------------%

    % Print the error to stderror and set the exit code to 1.
    %
    % Does not terminate the program.
    %
:- pred exit_error(string::in, io::di, io::uo) is det.

:- func curry(func(A, B) = C, pair(A, B)) = C.

    % one_item([X]) = X.
    %
:- func one_item(list(T)) = T.

:- func one_item_in_set(set(T)) = T.

:- func first_item(list(T)) = T.

:- func maybe_list(maybe(X)) = list(X).

:- func list_maybe_to_list(list(maybe(X))) = list(X).

:- func maybe_cord(maybe(X)) = cord(X).

    % find_duplicates(List, DupsInList),
    %
    % DupsInList is the set of duplicate items in List, if DupsInList is
    % empty, then List contains no duplicates.
    %
:- pred find_duplicates(list(X)::in, set(X)::out) is det.

    % Mercury does not provide a map over maybe_error.
    %
:- func maybe_error_map(func(A) = B, maybe_error(A, E)) = maybe_error(B, E).

:- func maybe_error_list(list(maybe_error(A, E))) =
    maybe_error(list(A), list(E)).

    % set_map_foldl2(Pred, Set0, Set, !Acc1, !Acc2),
    %
:- pred set_map_foldl2(pred(X, Y, A, A, B, B),
    set(X), set(Y), A, A, B, B).
:- mode set_map_foldl2(pred(in, out, in, out, in, out) is det,
    in, out, in, out, in, out) is det.

:- pred map2_corresponding(pred(X, Y, A, B), list(X), list(Y), list(A),
    list(B)).
:- mode map2_corresponding(pred(in, in, out, out) is det, in, in, out, out)
    is det.

:- pred map4_corresponding2(pred(A, B, C, D, X, Y), list(A), list(B),
    list(C), list(D), list(X), list(Y)).
:- mode map4_corresponding2(pred(in, in, in, in, out, out) is det, in, in,
    in, in, out, out) is det.

:- pred remove_first_match_map(pred(X, Y), Y, list(X), list(X)).
:- mode remove_first_match_map(pred(in, out) is semidet, out, in, out)
    is semidet.

    % det_uint32_to_int
    %
    % For some reason Mercury 20.01 doesn't provide this (it would be
    % uint32.det_to_int).
    %
:- func det_uint32_to_int(uint32) = int.

:- func det_uint64_to_int(uint64) = int.

%-----------------------------------------------------------------------%

:- func list_join(list(T), list(T)) = list(T).

:- func bag_list_to_bag(list(bag(T))) = bag(T).

:- func string_join(string, list(string)) = string.

    % delete_first_match(L1, Pred, L) :-
    %
    % L is L1 with the first element that satisfies Pred removed, it fails
    % if there is no element satisfying Pred.
    %
:- pred list_delete_first_match(list(T), pred(T), list(T)).
:- mode list_delete_first_match(in, pred(in) is semidet, out) is semidet.

%-----------------------------------------------------------------------%

:- func power_intersect_list(list(set(T))) = set(T).

%-----------------------------------------------------------------------%

:- func handle_bool_option(option_table(O), O, T, T) = T.

%-----------------------------------------------------------------------%

:- pred map_set_or_update(func(V) = V, K, V, map(K, V), map(K, V)).
:- mode map_set_or_update(in, in, in, in, out) is det.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
:- implementation.

:- import_module bool.
:- import_module require.
:- import_module string.
:- import_module uint32.
:- import_module uint64.

%-----------------------------------------------------------------------%

exit_error(ErrMsg, !IO) :-
    write_string(stderr_stream, ErrMsg ++ "\n", !IO),
    set_exit_status(1, !IO).

%-----------------------------------------------------------------------%

curry(F, A - B) = F(A, B).

%-----------------------------------------------------------------------%

one_item(Xs) =
    ( if Xs = [X] then
        X
    else
        unexpected($file, $pred, "Expected a list with only one item")
    ).

first_item([]) =
    unexpected($file, $pred, "Expected a list with at least one item").
first_item([X | _]) =
    X.

one_item_in_set(Set) = X :-
    ( if is_singleton(Set, X0) then
        X = X0
    else
        unexpected($file, $pred, "Expected a set with only one item")
    ).

%-----------------------------------------------------------------------%

maybe_list(yes(X)) = [X].
maybe_list(no) = [].

list_maybe_to_list([]) = [].
list_maybe_to_list([no | List]) = list_maybe_to_list(List).
list_maybe_to_list([yes(X) | List]) = [X | list_maybe_to_list(List)].

%-----------------------------------------------------------------------%

maybe_cord(yes(X)) = singleton(X).
maybe_cord(no) = init.

%-----------------------------------------------------------------------%

find_duplicates(List, Dups) :-
    find_duplicates_2(List, set.init, set.init, Dups).

:- pred find_duplicates_2(list(X)::in, set(X)::in, set(X)::in, set(X)::out) is det.

find_duplicates_2([], _, !Dups).
find_duplicates_2([X | Xs], !.Seen, !Dups) :-
    ( if member(X, !.Seen) then
        insert(X, !Dups)
    else
        insert(X, !Seen)
    ),
    find_duplicates_2(Xs, !.Seen, !Dups).

%-----------------------------------------------------------------------%

maybe_error_map(_, error(Error)) = error(Error).
maybe_error_map(Func, ok(X)) = ok(Func(X)).

%-----------------------------------------------------------------------%

maybe_error_list(Results) =
    maybe_error_list_ok(Results, []).

:- func maybe_error_list_ok(list(maybe_error(A, E)), list(A)) =
    maybe_error(list(A), list(E)).

maybe_error_list_ok([], Rev) = ok(reverse(Rev)).
maybe_error_list_ok([ok(R) | Rs], Rev) =
    maybe_error_list_ok(Rs, [R | Rev]).
maybe_error_list_ok([error(E) | Rs], _) =
    maybe_error_list_error(Rs, [E]).

:- func maybe_error_list_error(list(maybe_error(A, E)), list(E)) =
    maybe_error(list(A), list(E)).

maybe_error_list_error([], Rev) = error(reverse(Rev)).
maybe_error_list_error([error(E) | Rs], Rev) =
    maybe_error_list_error(Rs, [E | Rev]).
maybe_error_list_error([ok(_) | Rs], Rev) =
    maybe_error_list_error(Rs, Rev).

%-----------------------------------------------------------------------%

set_map_foldl2(Pred, Set0, Set, !Acc1, !Acc2) :-
    List0 = to_sorted_list(Set0),
    list.map_foldl2(Pred, List0, List, !Acc1, !Acc2),
    Set = list_to_set(List).

%-----------------------------------------------------------------------%

map2_corresponding(P, Xs0, Ys0, As, Bs) :-
    ( if
        Xs0 = [],
        Ys0 = []
    then
        As = [],
        Bs = []
    else if
        Xs0 = [X | Xs],
        Ys0 = [Y | Ys]
    then
        P(X, Y, A, B),
        map2_corresponding(P, Xs, Ys, As0, Bs0),
        As = [A | As0],
        Bs = [B | Bs0]
    else
        unexpected($file, $pred, "Mismatched inputs")
    ).

map4_corresponding2(P, As0, Bs0, Cs0, Ds0, Xs, Ys) :-
    ( if
        As0 = [],
        Bs0 = [],
        Cs0 = [],
        Ds0 = []
    then
        Xs = [],
        Ys = []
    else if
        As0 = [A | As],
        Bs0 = [B | Bs],
        Cs0 = [C | Cs],
        Ds0 = [D | Ds]
    then
        P(A, B, C, D, X, Y),
        map4_corresponding2(P, As, Bs, Cs, Ds, Xs0, Ys0),
        Xs = [X | Xs0],
        Ys = [Y | Ys0]
    else
        unexpected($file, $pred, "Mismatched inputs")
    ).

%-----------------------------------------------------------------------%

remove_first_match_map(Pred, Y, [X | Xs], Ys) :-
    ( if Pred(X, YP) then
        Y = YP,
        Ys = Xs
    else
        remove_first_match_map(Pred, Y, Xs, Ys0),
        Ys = [X | Ys0]
    ).

%-----------------------------------------------------------------------%

det_uint32_to_int(Uint32) = Int :-
    Int = cast_to_int(Uint32),
    % This should catch cases when this doesn't work.
    ( if from_int(Int, Uint32) then
        true
    else
        unexpected($file, $pred, "Uint32 out of range")
    ).

%-----------------------------------------------------------------------%

det_uint64_to_int(Uint64) = Int :-
    Int = cast_to_int(Uint64),
    ( if from_int(Int, Uint64) then
        true
    else
        unexpected($file, $pred, "Uint64 out of range")
    ).

%-----------------------------------------------------------------------%

list_join(_, []) = [].
list_join(_, [X]) = [X].
list_join(J, [X1, X2 | Xs]) =
    [X1 | J ++ list_join(J, [X2 | Xs])].

%-----------------------------------------------------------------------%

bag_list_to_bag(LoB) =
    foldl(union, LoB, init).

%-----------------------------------------------------------------------%

string_join(Sep, List) = append_list(list_join([Sep], List)).

%-----------------------------------------------------------------------%

list_delete_first_match(Xs0, Pred, Xs) :-
    list_delete_first_match(Xs0, Pred, [], Xs).

:- pred list_delete_first_match(list(T), pred(T), list(T), list(T)).
:- mode list_delete_first_match(in, pred(in) is semidet, in, out) is semidet.

list_delete_first_match([X | Xs0], Pred, !Xs) :-
    ( if Pred(X) then
        reverse(!Xs)
    else
        !:Xs = [X | !.Xs],
        list_delete_first_match(Xs0, Pred, !Xs)
    ).

%-----------------------------------------------------------------------%

power_intersect_list([]) = unexpected($file, $pred, "Answer is infinite").
power_intersect_list([H | T]) =
    power_intersect_list_2(H, T).

:- func power_intersect_list_2(set(T), list(set(T))) = set(T).

power_intersect_list_2(A, []) = A.
power_intersect_list_2(A, [B | Ls]) =
    power_intersect_list_2(A `intersect` B, Ls).

%-----------------------------------------------------------------------%

handle_bool_option(OptionTable, Option, True, False) = Result :-
    lookup_bool_option(OptionTable, Option, Bool),
    ( Bool = yes,
        Result = True
    ; Bool = no,
        Result = False
    ).

%-----------------------------------------------------------------------%

map_set_or_update(UpdateFn, Key, Value, !Map) :-
    ( if search(!.Map, Key, OldValue) then
        det_update(Key, UpdateFn(OldValue), !Map)
    else
        set(Key, Value, !Map)
    ).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
