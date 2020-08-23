%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module varmap.
%
% Copyright (C) 2015-2016, 2019-2020 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
% Plasma variable map data structure.
%
%-----------------------------------------------------------------------%

:- interface.

:- import_module list.
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

:- func get_var_name_no_suffix(varmap, var) = string.

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

:- pred add_n_anon_vars(int::in, list(var)::out,
    varmap::in, varmap::out) is det.

:- pred search_var(varmap::in, string::in, var::out) is semidet.

%-----------------------------------------------------------------------%
%
% This interface allows multiple names per variable, it is used after
% variable renaming has occured.
%

:- pred add_fresh_var(string::in, var::out, varmap::in, varmap::out) is det.

:- pred search_vars(varmap::in, string::in, set(var)::out) is semidet.

%-----------------------------------------------------------------------%

:- pred var_or_make_var(var_or_wildcard(var)::in, var::out,
    varmap::in, varmap::out) is det.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module map.
:- import_module require.

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
    Name = format("%s_%d", [s(get_var_name_no_suffix(Varmap, Var)), i(Var)]).

get_var_name_no_suffix(Varmap, Var) = Name :-
    ( if search(Varmap ^ vm_forward, Var, Name0Prime) then
        Name = Name0Prime
    else
        Name = "v"
    ).

%-----------------------------------------------------------------------%

add_unique_var(Name, Var, !Varmap) :-
    add_anon_var(Var, !Varmap),
    add_forward_name(Name, Var, !Varmap),
    ( if search(!.Varmap ^ vm_backward, Name, _) then
        unexpected($file, $pred, "Variable already exists")
    else
        det_insert(Name, make_singleton_set(Var), !.Varmap ^ vm_backward,
            Backward),
        !Varmap ^ vm_backward := Backward
    ).

get_or_add_var(Name, Var, !Varmap) :-
    ( if search_var(!.Varmap, Name, VarPrime) then
        Var = VarPrime
    else
        add_unique_var(Name, Var, !Varmap)
    ).

add_anon_var(Var, !Varmap) :-
    Var = !.Varmap ^ vm_next_var,
    !Varmap ^ vm_next_var := Var + 1.

add_n_anon_vars(N, Vars, !Varmap) :-
    ( if N < 1 then
        Vars = []
    else
        add_n_anon_vars(N - 1, Vars0, !Varmap),
        add_anon_var(Var, !Varmap),
        Vars = [Var | Vars0]
    ).

search_var(Varmap, Name, Var) :-
    search_vars(Varmap, Name, Vars),
    ( if singleton_set(VarPrime, Vars) then
        Var = VarPrime
    else
        unexpected($file, $pred,
            format("%s is ambigious", [s(Name)]))
    ).

%-----------------------------------------------------------------------%

add_fresh_var(Name, Var, !Varmap) :-
    add_anon_var(Var, !Varmap),
    add_forward_name(Name, Var, !Varmap),
    ( if search(!.Varmap ^ vm_backward, Name, Vars0) then
        Vars = set.insert(Vars0, Var)
    else
        Vars = make_singleton_set(Var)
    ),
    map.set(Name, Vars, !.Varmap ^ vm_backward, Backward),
    !Varmap ^ vm_backward := Backward.

search_vars(Varmap, Name, Var) :-
    search(Varmap ^ vm_backward, Name, Var).

%-----------------------------------------------------------------------%

var_or_make_var(var(Var), Var, !Varmap).
var_or_make_var(wildcard, Var, !Varmap) :-
    add_anon_var(Var, !Varmap).

%-----------------------------------------------------------------------%

:- pred add_forward_name(string::in, var::in, varmap::in, varmap::out)
    is det.

add_forward_name(Name, Var, !Varmap) :-
    det_insert(Var, Name, !.Varmap ^ vm_forward, Forward),
    !Varmap ^ vm_forward := Forward.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
