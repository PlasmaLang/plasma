%-----------------------------------------------------------------------%
% Plasma AST Environment manipulation routines
% vim: ts=4 sw=4 et
%
% Copyright (C) 2015 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
% This module contains code to track the environment of a statement in the
% Plasma AST.
%
%-----------------------------------------------------------------------%
:- module ast.env.
%-----------------------------------------------------------------------%

:- interface.

:- import_module string.

:- import_module varmap.

%-----------------------------------------------------------------------%

:- type env.

:- func init = env.

:- pred env_add_var(string::in, var::out, env::in, env::out,
    varmap::in, varmap::out) is det.

:- pred env_add_func(q_name::in, func_id::in, env::in, env::out) is det.

:- pred env_import_star(q_name::in, env::in, env::out) is det.

:- type env_entry
    --->    ee_var(var)
    ;       ee_func(func_id).

:- pred env_search(env::in, q_name::in, env_entry::out) is semidet.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

:- import_module map.

%-----------------------------------------------------------------------%

    % TODO, use a radix structure.  Lookup errors can be more informative.
    %
:- type env
    --->    env(
                e_map           :: map(q_name, env_entry)
            ).

%-----------------------------------------------------------------------%

init = env(init).

env_add_var(Name, Var, !Env, !Varmap) :-
    add_or_get_var(Name, Var, !Varmap),
    det_insert(q_name(Name), ee_var(Var), !.Env ^ e_map, Map),
    !:Env = env(Map).

env_add_func(Name, Func, !Env) :-
    det_insert(Name, ee_func(Func), !.Env ^ e_map, Map),
    !:Env = env(Map).

env_import_star(Name, !Env) :-
    Map0 = !.Env ^ e_map,
    foldl(do_env_import_star(Name), Map0, Map0, Map),
    !Env ^ e_map := Map.

:- pred do_env_import_star(q_name::in, q_name::in, env_entry::in,
    map(q_name, env_entry)::in, map(q_name, env_entry)::out) is det.

do_env_import_star(Module, Name, Entry, !Map) :-
    ( if q_name_append(Module, UnqualName, Name) then
        det_insert(UnqualName, Entry, !Map)
    else
        true
    ).

env_search(Env, QName, Entry) :-
    search(Env ^ e_map, QName, Entry).

