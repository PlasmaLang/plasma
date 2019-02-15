%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module core_to_pz.locn.
%
% Copyright (C) 2015-2019 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
% Plasma core to pz conversion - value location information
%
%-----------------------------------------------------------------------%
:- interface.

:- import_module cord.

%-----------------------------------------------------------------------%

:- type var_locn_map.

    % The location of a variable.
    %
:- type var_locn
            % The variable is on the stack.
    --->    vl_stack(int).

%-----------------------------------------------------------------------%

:- func vl_init = var_locn_map.

:- pred vl_put_var(var::in, int::in, var_locn_map::in, var_locn_map::out)
    is det.

:- pred vl_put_vars(list(var)::in, int::in, varmap::in,
    cord(pz_instr_obj)::out, var_locn_map::in, var_locn_map::out) is det.

:- func vl_lookup(var_locn_map, var) = var_locn.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
:- implementation.

:- import_module int.

%-----------------------------------------------------------------------%

:- type var_locn_map == map(var, var_locn).

%-----------------------------------------------------------------------%

vl_init = map.init.

%-----------------------------------------------------------------------%

vl_put_var(Var, Depth, !Map) :-
    map.det_insert(Var, vl_stack(Depth), !Map).

%-----------------------------------------------------------------------%

vl_put_vars([], _, _, init, !Map).
vl_put_vars([Var | Vars], Depth0, Varmap, Comments, !Map) :-
    Depth = Depth0 + 1,
    vl_put_var(Var, Depth, !Map),
    Comment = pzio_comment(format("%s is at depth %d",
        [s(get_var_name(Varmap, Var)), i(Depth)])),
    vl_put_vars(Vars, Depth, Varmap, Comments0, !Map),
    Comments = cons(Comment, Comments0).

%-----------------------------------------------------------------------%

vl_lookup(Map, Var) = Locn :-
    map.lookup(Map, Var, Locn).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
