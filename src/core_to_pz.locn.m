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

%
% The location map information is divided into two halves, the static
% information which is static per PZ procedure.  And the dyanmic
% information, which changes with code generation (for example as values are
% pushed onto the stack).
%

:- type val_locn_map_static.

:- type var_locn_map.

    % The location of a variable.
    %
:- type var_locn
            % The variable is on the stack.
    --->    vl_stack(int).

    % Strings can only exist in a module's envrionment for now.
    %
:- type str_locn
    --->    sl_module_env(field_num).

%-----------------------------------------------------------------------%

:- func vl_init = var_locn_map.

:- pred vl_put_var(var::in, int::in, var_locn_map::in, var_locn_map::out)
    is det.

:- pred vl_put_vars(list(var)::in, int::in, varmap::in,
    cord(pz_instr_obj)::out, var_locn_map::in, var_locn_map::out) is det.

:- func vl_lookup_var(var_locn_map, var) = var_locn.

%-----------------------------------------------------------------------%

:- func sl_init = val_locn_map_static.

:- func sl_lookup(val_locn_map_static, string) = str_locn.

:- pred sl_search(val_locn_map_static::in, string::in, str_locn::out)
    is semidet.

:- pred sl_insert(string::in, field_num::in, val_locn_map_static::in,
    val_locn_map_static::out) is det.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
:- implementation.

:- import_module int.

%-----------------------------------------------------------------------%

:- type val_locn_map_static == map(const_data, str_locn).

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

vl_lookup_var(Map, Var) = Locn :-
    map.lookup(Map, Var, Locn).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

sl_init = map.init.

%-----------------------------------------------------------------------%

sl_lookup(Map, Str) = Locn :-
    map.lookup(Map, cd_string(Str), Locn).

%-----------------------------------------------------------------------%

sl_search(Map, Str, Locn) :-
    map.search(Map, cd_string(Str), Locn).

%-----------------------------------------------------------------------%

sl_insert(String, FieldNum, !Map) :-
    map.det_insert(cd_string(String), sl_module_env(FieldNum), !Map).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
