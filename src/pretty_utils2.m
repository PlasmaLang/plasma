%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module pretty_utils2.
%
% Pretty printer utils 2.
%
% Copyright (C) 2019-2020 Plasma Team
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
    ;       p_nl_soft
    ;       p_tabstop.

:- func p_str(string) = pretty.
:- func p_cord(cord(string)) = pretty.

:- func p_parens(list(pretty), list(pretty), list(pretty), list(pretty)) =
    pretty.

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
    pretty(Pretty, Cord, _, Indent, _, Indent, _).

:- pred pretty(pretty::in, cord(string)::out, int::out,
    int::in, int::out, int::in, int::out) is det.

pretty(p_unit(Cord),      Cord, MaxPos, !Pos,   !Indent) :-
    !:Pos = !.Pos + cord_string_len(Cord),
    MaxPos = !.Pos.
pretty(p_group(Pretties), Cord, MaxPos, !Pos,   !Indent) :-
    map2_foldl2(pretty, Pretties, Cords, Maxes, !Pos, !.Indent + unit, _),
    MaxPos = foldl(max, Maxes, !.Pos),
    Cord = cord_list_to_cord(Cords).
pretty(p_spc,             Cord, MaxPos, !Pos,   !Indent) :-
    Cord = singleton(" "),
    !:Pos = !.Pos + 1,
    MaxPos = !.Pos.
pretty(p_nl_hard,         Cord, MaxPos, _, Pos, !Indent) :-
    Cord = line(!.Indent),
    Pos = !.Indent,
    MaxPos = !.Indent.
pretty(p_nl_soft,         Cord, MaxPos, _, Pos, !Indent) :-
    Cord = line(!.Indent),
    Pos = !.Indent,
    MaxPos = !.Indent.
pretty(p_tabstop,         init, MaxPos, !Pos,   !Indent) :-
    MaxPos = !.Pos,
    ( if !.Indent < !.Pos then
        !:Indent = !.Pos
    else
        unexpected($file, $pred, "Tabstop before current indent")
    ).

:- func cord_string_len(cord(string)) = int.

cord_string_len(Cord) =
    foldl(func(S, L) = length(S) + L, Cord, 0).

%-----------------------------------------------------------------------%

    % Default indentation amount.
    %
:- func unit = int.
unit = 2.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
