%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module pretty_utils2.
%
% Pretty printer utils 2.
%
% Copyright (C) 2019 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
%-----------------------------------------------------------------------%
:- interface.

:- import_module cord.
:- import_module int.
:- import_module list.
:- import_module string.

%-----------------------------------------------------------------------%

:- type pretty
    --->    p_unit(cord(string))
    ;       p_group(list(pretty))
    ;       p_spc
    ;       p_nl_hard
    ;       p_nl_soft.

:- func p_str(string) = pretty.
:- func p_cord(cord(string)) = pretty.

:- func p_parens(list(pretty), list(pretty), list(pretty), list(pretty)) =
    pretty.

%:- func nl = pretty.
%
%:- func join(pretty, list(pretty)) = pretty.

:- func pretty(int, pretty) = cord(string).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
:- implementation.

:- import_module pretty_utils.
:- import_module require.
:- import_module util.

%-----------------------------------------------------------------------%

p_str(String) = p_unit(singleton(String)).
p_cord(Cord) = p_unit(Cord).

%-----------------------------------------------------------------------%

p_parens(Left, Right, D, Items) =
    p_group(Left ++ list_join(D, Items) ++ Right).

%-----------------------------------------------------------------------%

pretty(Indent, Pretty) = Cord :-
    pretty(Indent, Pretty, Cord, _, Indent, _).

:- pred pretty(int::in, pretty::in, cord(string)::out, int::out,
    int::in, int::out) is det.

pretty(_,      p_unit(Cord),      Cord, MaxPos, !Pos) :-
    !:Pos = !.Pos + cord_string_len(Cord),
    MaxPos = !.Pos.
pretty(Indent, p_group(Pretties), Cord, MaxPos, !Pos) :-
    map2_foldl(pretty(Indent + 1), Pretties, Cords, Maxes, !Pos),
    MaxPos = foldl(max, Maxes, !.Pos),
    Cord = cord_list_to_cord(Cords).
pretty(_,      p_spc,     Cord, MaxPos, !Pos) :-
    Cord = singleton(" "),
    !:Pos = !.Pos + 1,
    MaxPos = !.Pos.
pretty(Indent, p_nl_hard, Cord, MaxPos, _, Pos) :-
    Cord = line(Indent),
    Pos = Indent,
    MaxPos = Indent.
pretty(Indent, p_nl_soft, Cord, MaxPos, _, Pos) :-
    Cord = line(Indent),
    Pos = Indent,
    MaxPos = Indent.

:- func cord_string_len(cord(string)) = int.

cord_string_len(Cord) =
    foldl(func(S, L) = length(S) + L, Cord, 0).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
