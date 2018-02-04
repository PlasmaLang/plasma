%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module pretty_utils.
%
% Pretty printer utils.
%
% Copyright (C) 2017-2018 Plasma Team
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

:- typeclass pretty(T) where [
    func pretty(T) = cord(string)
].

%-----------------------------------------------------------------------%

:- func join(cord(T), list(cord(T))) = cord(T).

:- func pretty_string(cord(string)) = string.

:- func pretty_args(func(X) = cord(string), list(X)) =
    cord(string).

:- func pretty_optional_args(func(X) = cord(string), list(X)) =
    cord(string).

:- func pretty_seperated(cord(string), func(X) = cord(string), list(X)) =
    cord(string).

    % maybe_pretty_args_maybe_prefix(Prefix, Func, Items) = Pretty.
    %
    % Print a list of items with a prefix (if there are any items) and
    % parens if there are more than one item.
    %
    % [] -> ""
    % [X] -> Prefix ++ X
    % Xs -> Prefix ++ "(" ++ pretty_args(Xs) ++ ")"
    %
:- func maybe_pretty_args_maybe_prefix(cord(string), func(T) = cord(string),
    list(T)) = cord(string).

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

:- func comment_line(int) = cord(string).

%-----------------------------------------------------------------------%

:- func context_pretty(int, context) = cord(string).

:- func var_pretty(varmap, var) = cord(string).

:- func var_or_wild_pretty(varmap, var_or_wildcard(var)) = cord(string).

:- func vars_pretty(varmap, set(var)) = cord(string).

:- type id_lookup(ID) == pred(ID, q_name).
:- inst id_lookup == (pred(in, out) is det).

:- func id_pretty(id_lookup(Id), Id) = cord(string).
:- mode id_pretty(in(id_lookup), in) = (out) is det.

:- func name_pretty(q_name) = cord(string).

:- func const_pretty(id_lookup(func_id), id_lookup(ctor_id), const_type) =
    cord(string).
:- mode const_pretty(in(id_lookup), in(id_lookup), in) = (out) is det.

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

pretty_string(Cord) = append_list(list(Cord)).

%-----------------------------------------------------------------------%

pretty_args(ItemPretty, Args) =
    open_paren ++ pretty_seperated(comma_spc, ItemPretty, Args) ++
        close_paren.

pretty_optional_args(_, []) = cord.init.
pretty_optional_args(ItemPretty, Args@[_ | _]) =
    pretty_args(ItemPretty, Args).

%-----------------------------------------------------------------------%

pretty_seperated(Sep, ItemPretty, Items) =
    join(Sep, map(ItemPretty, Items)).

%-----------------------------------------------------------------------%

maybe_pretty_args_maybe_prefix(_, _, []) = cord.init.
maybe_pretty_args_maybe_prefix(Prefix, Func, [Item]) = Prefix ++ Func(Item).
maybe_pretty_args_maybe_prefix(Prefix, Func, Items@[_, _ | _]) =
    Prefix ++ pretty_args(Func, Items).

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

comment_line(N) = line(N) ++ singleton("// ").

%-----------------------------------------------------------------------%

context_pretty(Indent, Context) =
    comment_line(Indent) ++ singleton(context_string(Context)).

var_pretty(Varmap, Var) = singleton(get_var_name(Varmap, Var)).

var_or_wild_pretty(Varmap, var(Var)) = var_pretty(Varmap, Var).
var_or_wild_pretty(_, wildcard) = singleton("_").

vars_pretty(Varmap, Vars) =
    join(comma ++ spc, map(var_pretty(Varmap), set.to_sorted_list(Vars))).

id_pretty(Lookup, Id) = name_pretty(Name) :-
    Lookup(Id, Name).

name_pretty(Name) = singleton(q_name_to_string(Name)).

const_pretty(_, _,          c_number(Int)) =    singleton(string(Int)).
const_pretty(_, _,          c_string(String)) =
    singleton(escape_string(String)).
const_pretty(FuncLookup, _, c_func(FuncId)) =   id_pretty(FuncLookup, FuncId).
const_pretty(_, CtorLookup, c_ctor(CtorId)) =   id_pretty(CtorLookup, CtorId).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
