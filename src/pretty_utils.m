%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module pretty_utils.
%
% Pretty printer utils.
%
% Copyright (C) 2016 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
%-----------------------------------------------------------------------%
:- interface.

:- import_module cord.
:- import_module int.
:- import_module list.
:- import_module set.
:- import_module string.

:- import_module context.
:- import_module common_types.
:- import_module q_name.
:- import_module varmap.

%-----------------------------------------------------------------------%

:- func join(cord(T), list(cord(T))) = cord(T).

:- func nl = cord(string).

:- func spc = cord(string).

:- func semicolon = cord(string).

:- func colon = cord(string).

:- func comma = cord(string).

:- func bang = cord(string).

:- func open_curly = cord(string).

:- func close_curly = cord(string).

:- func open_paren = cord(string).

:- func close_paren = cord(string).

:- func equals = cord(string).

:- func indent(int) = cord(string).

:- func line(int) = cord(string).

:- func comment_line(int) = cord(string).

%-----------------------------------------------------------------------%

:- func context_pretty(int, context) = cord(string).

:- func var_pretty(varmap, var) = cord(string).

:- func vars_pretty(varmap, set(var)) = cord(string).

:- type func_lookup == pred(func_id, q_name).
:- inst func_lookup == (pred(in, out) is det).

:- func const_pretty(func_lookup, const_type) = cord(string).
:- mode const_pretty(in(func_lookup), in) = (out) is det.

:- func func_name_pretty(func_lookup, func_id) = cord(string).
:- mode func_name_pretty(in(func_lookup), in) = (out) is det.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
:- implementation.

:- import_module string_utils.

%-----------------------------------------------------------------------%

join(_, []) = empty.
join(_, [X]) = X.
join(Join, [X1, X2 | Xs]) =
    X1 ++ Join ++ join(Join, [X2 | Xs]).

%-----------------------------------------------------------------------%

nl = singleton("\n").

spc = singleton(" ").

semicolon = singleton(";").

colon = singleton(":").

comma = singleton(",").

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

comment_line(N) = line(N) ++ singleton("// ").

%-----------------------------------------------------------------------%

context_pretty(Indent, Context) =
    comment_line(Indent) ++ singleton(context_string(Context)).

var_pretty(Varmap, Var) = singleton(get_var_name(Varmap, Var)).

vars_pretty(Varmap, Vars) =
    join(comma ++ spc, map(var_pretty(Varmap), set.to_sorted_list(Vars))).

const_pretty(_,          c_number(Int)) =    singleton(string(Int)).
const_pretty(_,          c_string(String)) = singleton(escape_string(String)).
const_pretty(FuncLookup, c_func(FuncId)) =   func_name_pretty(FuncLookup,
                                                              FuncId).

func_name_pretty(Lookup, FuncId) = singleton(String) :-
    Lookup(FuncId, Name),
    String = q_name_to_string(Name).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
