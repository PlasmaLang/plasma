%-----------------------------------------------------------------------%
% Plasma variable map data structure.
% vim: ts=4 sw=4 et
%
% Copyright (C) 2015-2016 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
% This program compiles plasma modules.
%
%-----------------------------------------------------------------------%
:- module varmap.
%-----------------------------------------------------------------------%

:- interface.

:- type var.

:- type varmap.

%-----------------------------------------------------------------------%

:- func init = varmap.

:- pred add_or_get_var(string::in, var::out, varmap::in, varmap::out) is det.

:- pred search_var(varmap::in, string::in, var::out) is semidet.

:- pred lookup_var(varmap::in, string::in, var::out) is det.

:- func get_var_name(varmap, var) = string.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module map.
:- import_module string.

:- type var == int.

:- type varmap
    --->    varmap(
                vm_forward              :: map(var, string),
                vm_backward             :: map(string, var),
                vm_next_var             :: var
            ).

%-----------------------------------------------------------------------%

init = varmap(init, init, 0).

%-----------------------------------------------------------------------%

add_or_get_var(Name, Var, Varmap0, Varmap) :-
    ( if search(Varmap0 ^ vm_backward, Name, VarPrime) then
        Var = VarPrime,
        Varmap = Varmap0
    else
        Var = Varmap0 ^ vm_next_var,
        det_insert(Var, Name, Varmap0 ^ vm_forward, Forward),
        det_insert(Name, Var, Varmap0 ^ vm_backward, Backward),
        Varmap = varmap(Forward, Backward, Var+1)
    ).

search_var(Varmap, Name, Var) :-
    search(Varmap ^ vm_backward, Name, Var).

lookup_var(Varmap, Name, Var) :-
    lookup(Varmap ^ vm_backward, Name, Var).

get_var_name(Varmap, Var) = Name :-
    lookup(Varmap ^ vm_forward, Var, Name).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
