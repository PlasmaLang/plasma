%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module varmap.
%
% Copyright (C) 2015-2016 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
% Plasma variable map data structure.
%
%-----------------------------------------------------------------------%

:- interface.

:- import_module set.
:- import_module string.

:- type var.

:- type var_or_wildcard(V)
    --->    var(V)
    ;       wildcard.

:- pred vow_is_var(var_or_wildcard(V)::in, V::out) is semidet.

    % A varmap provides name -> var and var -> name mappings.  Note that
    % multiple variables can share the same name, for example on seperate
    % execution branches.  In this way names are only a convenience to the
    % user.
    %
:- type varmap.

%-----------------------------------------------------------------------%

:- func init = varmap.

:- func get_var_name(varmap, var) = string.

%-----------------------------------------------------------------------%
%
% This interface is constrained to one name per variable.  It is used when
% first setting up the varmap.  These functions and predicates throw an
% exception of they find multiple variables with the same name.
%

:- pred add_unique_var(string::in, var::out, varmap::in, varmap::out) is det.

:- pred get_or_add_var(string::in, var::out, varmap::in, varmap::out)
    is det.

:- pred add_anon_var(var::out, varmap::in, varmap::out) is det.

:- pred search_var(varmap::in, string::in, var::out) is semidet.

%-----------------------------------------------------------------------%
%
% This interface allows multiple names per variable, it is used after
% variable renaming has occured.
%

:- pred search_vars(varmap::in, string::in, set(var)::out) is semidet.

%-----------------------------------------------------------------------%

:- pred var_or_make_var(var_or_wildcard(var)::in, var::out,
    varmap::in, varmap::out) is det.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module map.
:- import_module list.
:- import_module require.
:- import_module string.

%-----------------------------------------------------------------------%

:- type var == int.

%-----------------------------------------------------------------------%

vow_is_var(var(V), V).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- type varmap
    --->    varmap(
                vm_forward              :: map(var, string),
                vm_backward             :: map(string, set(var)),
                vm_next_var             :: var
            ).

%-----------------------------------------------------------------------%

init = varmap(init, init, 0).

get_var_name(Varmap, Var) = Name :-
    ( if search(Varmap ^ vm_forward, Var, Name0Prime) then
        Name0 = Name0Prime
    else
        Name0 = "v"
    ),
    Name = format("%s_%d", [s(Name0), i(Var)]).

%-----------------------------------------------------------------------%

add_unique_var(Name, Var, Varmap0, Varmap) :-
    Var = Varmap0 ^ vm_next_var,
    det_insert(Var, Name, Varmap0 ^ vm_forward, Forward),
    ( if search(Varmap0 ^ vm_backward, Name, _) then
        unexpected($file, $pred, "Variable already exists")
    else
        det_insert(Name, make_singleton_set(Var), Varmap0 ^ vm_backward,
            Backward)
    ),
    Varmap = varmap(Forward, Backward, Var+1).

get_or_add_var(Name, Var, !Varmap) :-
    ( if search_var(!.Varmap, Name, VarPrime) then
        Var = VarPrime
    else
        add_unique_var(Name, Var, !Varmap)
    ).

add_anon_var(Var, !Varmap) :-
    Var = !.Varmap ^ vm_next_var,
    !Varmap ^ vm_next_var := Var + 1.

search_var(Varmap, Name, Var) :-
    search_vars(Varmap, Name, Vars),
    ( if singleton_set(VarPrime, Vars) then
        Var = VarPrime
    else
        unexpected($file, $pred,
            format("%s is ambigious", [s(Name)]))
    ).

%-----------------------------------------------------------------------%

search_vars(Varmap, Name, Var) :-
    search(Varmap ^ vm_backward, Name, Var).

%-----------------------------------------------------------------------%

var_or_make_var(var(Var), Var, !Varmap).
var_or_make_var(wildcard, Var, !Varmap) :-
    add_anon_var(Var, !Varmap).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
