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

%-----------------------------------------------------------------------%

:- type env.

:- func init = env.

:- pred env_has_var(env::in, string::in) is semidet.

:- pred env_add_var(string::in, env::in, env::out) is det.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

:- import_module set.

%-----------------------------------------------------------------------%

:- type env
    --->    env(set(string)).

%-----------------------------------------------------------------------%

init = env(init).

env_has_var(env(Set), Var) :- member(Var, Set).

env_add_var(Var, env(Set0), env(Set)) :-
    insert(Var, Set0, Set).

