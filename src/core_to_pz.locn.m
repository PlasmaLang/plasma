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

    % The location of a variable.
    %
:- type var_locn
            % The variable is on the stack.
    --->    vl_stack(int).

:- type proc_locn
    --->    pl_instrs(list(pz_instr))
    ;       pl_static_proc(pzp_id)
    ;       pl_import(pzi_id, field_num).

    % Strings can only exist in a module's envrionment for now.
    %
:- type str_locn
    --->    sl_module_env(field_num).

%-----------------------------------------------------------------------%
%
% The location map information is divided into two halves, the static
% information which is static per PZ procedure.  And the dyanmic
% information, which changes with code generation (for example as values are
% pushed onto the stack).
%

:- type val_locn_map_static.

:- func vls_init = val_locn_map_static.

:- pred vls_set_proc(func_id::in, pzp_id::in,
    val_locn_map_static::in, val_locn_map_static::out) is det.

:- pred vls_set_proc_instrs(func_id::in, list(pz_instr)::in,
    val_locn_map_static::in, val_locn_map_static::out) is det.

:- pred vls_set_proc_imported(func_id::in, pzi_id::in, field_num::in,
    val_locn_map_static::in, val_locn_map_static::out) is det.

:- func vls_lookup_proc_id(val_locn_map_static, func_id) = pzp_id.

:- pred vls_has_str(val_locn_map_static::in, string::in) is semidet.

:- pred vls_insert_str(string::in, field_num::in, val_locn_map_static::in,
    val_locn_map_static::out) is det.

%-----------------------------------------------------------------------%

:- type val_locn_map.

:- pred vl_start_var_binding(val_locn_map_static::in, val_locn_map::out)
    is det.

:- pred vl_put_var(var::in, int::in, val_locn_map::in, val_locn_map::out)
    is det.

:- pred vl_put_vars(list(var)::in, int::in, varmap::in,
    cord(pz_instr_obj)::out, val_locn_map::in, val_locn_map::out) is det.

:- func vl_lookup_proc(val_locn_map, func_id) = proc_locn.

:- func vl_lookup_var(val_locn_map, var) = var_locn.

:- func vl_lookup_str(val_locn_map, string) = str_locn.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
:- implementation.

:- import_module int.

%-----------------------------------------------------------------------%

:- type val_locn_map_static
    --->    val_locn_map_static(
                vls_const_data          :: map(const_data, str_locn),
                vls_proc_id_map         :: map(func_id, proc_locn)
            ).

%-----------------------------------------------------------------------%

vls_init = val_locn_map_static(init, init).

%-----------------------------------------------------------------------%

vls_set_proc(FuncId, ProcId, !Map) :-
    vls_set_proc_1(FuncId, pl_static_proc(ProcId), !Map).

vls_set_proc_instrs(FuncId, Instrs, !Map) :-
    vls_set_proc_1(FuncId, pl_instrs(Instrs), !Map).

vls_set_proc_imported(FuncId, ImportId, FieldNum, !Map) :-
    vls_set_proc_1(FuncId, pl_import(ImportId, FieldNum), !Map).

:- pred vls_set_proc_1(func_id::in, proc_locn::in,
    val_locn_map_static::in, val_locn_map_static::out) is det.

vls_set_proc_1(FuncId, Locn, !Map) :-
    map.det_insert(FuncId, Locn, !.Map ^ vls_proc_id_map, ProcMap),
    !Map ^ vls_proc_id_map := ProcMap.

%-----------------------------------------------------------------------%

vls_lookup_proc_id(Map, FuncId) = ProcId :-
    map.lookup(Map ^ vls_proc_id_map, FuncId, Locn),
    ( Locn = pl_static_proc(ProcId)
    ;
        ( Locn = pl_instrs(_)
        ; Locn = pl_import(_, _)
        ),
        unexpected($file, $pred, "Non-static proc")
    ).

%-----------------------------------------------------------------------%

vls_has_str(Map, Str) :-
    map.contains(Map ^ vls_const_data, cd_string(Str)).

%-----------------------------------------------------------------------%

vls_insert_str(String, FieldNum, !Map) :-
    map.det_insert(cd_string(String), sl_module_env(FieldNum),
        !.Map ^ vls_const_data, ConstMap),
    !Map ^ vls_const_data := ConstMap.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- type val_locn_map
    --->    val_locn_map(
                vl_static               :: val_locn_map_static,
                vl_vars                 :: map(var, var_locn)
            ).

%-----------------------------------------------------------------------%

vl_start_var_binding(Static, val_locn_map(Static, map.init)).

%-----------------------------------------------------------------------%

vl_put_var(Var, Depth, !Map) :-
    map.det_insert(Var, vl_stack(Depth), !.Map ^ vl_vars, VarsMap),
    !Map ^ vl_vars := VarsMap.

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

vl_lookup_proc(Map, FuncId) = Locn :-
    map.lookup(Map ^ vl_static ^ vls_proc_id_map, FuncId, Locn).

%-----------------------------------------------------------------------%

vl_lookup_var(Map, Var) = Locn :-
    map.lookup(Map ^ vl_vars, Var, Locn).

%-----------------------------------------------------------------------%

vl_lookup_str(Map, Str) = Locn :-
    map.lookup(Map ^ vl_static ^ vls_const_data, cd_string(Str), Locn).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
