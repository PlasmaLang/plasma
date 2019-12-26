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

:- func p_parens(pretty, pretty, list(pretty), list(pretty)) =
    pretty.

%:- func nl = pretty.
%
%:- func join(pretty, list(pretty)) = pretty.

:- func pretty(int, pretty) = cord(string).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
:- implementation.

:- import_module pretty_utils.
:- import_module util.

%-----------------------------------------------------------------------%

p_str(String) = p_unit(singleton(String)).
p_cord(Cord) = p_unit(Cord).

%-----------------------------------------------------------------------%

p_parens(Left, Right, D, Items) =
    p_group([Left | list_join(D, Items) ++ [Right]]).

%-----------------------------------------------------------------------%

pretty(_, p_unit(Cord)) = Cord.
pretty(Indent, p_group(Pretties)) =
    cord_list_to_cord(map(pretty(Indent+1), Pretties)).
pretty(_, p_spc) = singleton(" ").
pretty(_, p_nl_hard) = nl.
pretty(_, p_nl_soft) = singleton(" ").

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
