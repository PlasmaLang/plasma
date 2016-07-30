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

    % A varmap provides name -> var and var -> name mappings.  Note that
    % multiple variables can share the same name, for example on seperate
    % execution branches.  In this way names are only a convenience to the
    % user.
    %
:- type varmap.

%-----------------------------------------------------------------------%

:- func init = varmap.

:- pred add_new_var(string::in, var::out, varmap::in, varmap::out) is det.

:- pred add_anon_var(var::out, varmap::in, varmap::out) is det.

:- pred search_var(varmap::in, string::in, set(var)::out) is semidet.

:- pred lookup_var(varmap::in, string::in, set(var)::out) is det.

:- func get_var_name(varmap, var) = string.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module map.
:- import_module list.
:- import_module require.
:- import_module string.

:- type var == int.

:- type varmap
    --->    varmap(
                vm_forward              :: map(var, string),
                vm_backward             :: map(string, set(var)),
                vm_next_var             :: var
            ).

%-----------------------------------------------------------------------%

init = varmap(init, init, 0).

%-----------------------------------------------------------------------%

add_new_var(Name, Var, Varmap0, Varmap) :-
    Var = Varmap0 ^ vm_next_var,
    det_insert(Var, Name, Varmap0 ^ vm_forward, Forward),
    ( if search(Varmap0 ^ vm_backward, Name, Vars0) then
        Vars = insert(Vars0, Var),
        det_update(Name, Vars, Varmap0 ^ vm_backward, Backward)
    else
        det_insert(Name, make_singleton_set(Var), Varmap0 ^ vm_backward,
            Backward)
    ),
    Varmap = varmap(Forward, Backward, Var+1).

add_anon_var(Var, !Varmap) :-
    Var = !.Varmap ^ vm_next_var,
    !Varmap ^ vm_next_var := Var + 1.

search_var(Varmap, Name, Var) :-
    search(Varmap ^ vm_backward, Name, Var).

lookup_var(Varmap, Name, Var) :-
    lookup(Varmap ^ vm_backward, Name, Var).

get_var_name(Varmap, Var) = Name :-
    ( if search(Varmap ^ vm_forward, Var, Name0Prime) then
        Name0 = Name0Prime
    else
        Name0 = "V"
    ),
    Name = format("%s_%d", [s(Name0), i(Var)]).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
