%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module util.pretty_old.
%
% Pretty printer utils.
%
% Copyright (C) Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
%-----------------------------------------------------------------------%
:- interface.

:- import_module cord.
:- import_module list.
%-----------------------------------------------------------------------%

:- func join(cord(T), list(cord(T))) = cord(T).

:- func nl = cord(string).

:- func spc = cord(string).

:- func semicolon = cord(string).

:- func colon = cord(string).

:- func comma = cord(string).

:- func period = cord(string).

:- func comma_spc = cord(string).

:- func bang = cord(string).

:- func open_curly = cord(string).

:- func close_curly = cord(string).

:- func open_paren = cord(string).

:- func close_paren = cord(string).

:- func equals = cord(string).

:- func indent(int) = cord(string).

:- func line(int) = cord(string).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
:- implementation.

:- import_module int.

:- import_module util.mercury.

%-----------------------------------------------------------------------%

join(J, Xs) = cord_list_to_cord(list_join([J], Xs)).

%-----------------------------------------------------------------------%

nl = singleton("\n").

spc = singleton(" ").

semicolon = singleton(";").

colon = singleton(":").

comma = singleton(",").

period = singleton(".").

comma_spc = comma ++ spc.

bang = singleton("!").

open_curly = singleton("{").

close_curly = singleton("}").

open_paren = singleton("(").

close_paren = singleton(")").

equals = singleton("=").

%-----------------------------------------------------------------------%

indent(N) =
    ( if N = 0 then
        init
    else
        singleton(" ") ++ indent(N-1)
    ).

line(N) = nl ++ indent(N).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
