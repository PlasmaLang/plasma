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

    % The location of a value, this is made of two types.
    %
    % val_locn says where to start looking, either on the stack or with the
    % current environment.  Once you have then then...
    %
    % val_locn_next says what to do with the current location, if you're
    % done or weather you should follow some structure.
    %
:- type val_locn
            % The value is on the stack.
    --->    vl_stack(int, val_locn_next)
            % The value _is_ the current env.
    ;       vl_env(val_locn_next).

:- type val_locn_next
    --->    vln_done
            % The value is within some structure (like the environment).
    ;       vln_struct(pzs_id, field_num, pz_width, val_locn_next).

:- type proc_locn
    --->    pl_instrs(list(pz_instr))
    ;       pl_static_proc(pzp_id)
    ;       pl_import(pzi_id, field_num).

%-----------------------------------------------------------------------%
%
% The location map information is divided into two halves, the static
% information which is static per PZ procedure.  And the dynamic
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

:- pred vls_set_closure(func_id::in, pzs_id::in,
    val_locn_map_static::in, val_locn_map_static::out) is det.

:- func vls_lookup_closure(val_locn_map_static, func_id) = pzs_id.

:- pred vls_has_str(val_locn_map_static::in, string::in) is semidet.

:- pred vls_insert_str(string::in, pzs_id::in, field_num::in, pz_width::in,
    val_locn_map_static::in, val_locn_map_static::out) is det.

%-----------------------------------------------------------------------%

:- type val_locn_map.

:- pred vl_start_var_binding(val_locn_map_static::in, val_locn_map::out)
    is det.

:- pred vl_push_env(pzs_id::in, field_num::in,
    val_locn_map::in, val_locn_map::out) is det.

:- pred vl_put_var(var::in, int::in, val_locn_map::in, val_locn_map::out)
    is det.

:- pred vl_set_var_env(var::in, pzs_id::in, field_num::in, pz_width::in,
    val_locn_map::in, val_locn_map::out) is det.

:- pred vl_put_vars(list(var)::in, int::in, varmap::in,
    cord(pz_instr_obj)::out, val_locn_map::in, val_locn_map::out) is det.

:- func vl_lookup_proc(val_locn_map, func_id) = proc_locn.

:- func vl_lookup_proc_id(val_locn_map, func_id) = pzp_id.

:- func vl_lookup_closure(val_locn_map, func_id) = pzs_id.

:- func vl_lookup_var(val_locn_map, var) = val_locn.

:- func vl_lookup_str(val_locn_map, string) = val_locn.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
:- implementation.

:- import_module int.

%-----------------------------------------------------------------------%

:- type val_locn_map_static
    --->    val_locn_map_static(
                vls_const_data          :: map(const_data, val_locn),
                vls_proc_id_map         :: map(func_id, proc_locn),

                % Not exactly location data, but it is accessed and created
                % similarly.
                vls_closures            :: map(func_id, pzs_id)
            ).

%-----------------------------------------------------------------------%

vls_init = val_locn_map_static(init, init, init).

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

vls_set_closure(FuncId, EnvStructId, !Map) :-
    map.det_insert(FuncId, EnvStructId, !.Map ^ vls_closures, ClosuresMap),
    !Map ^ vls_closures := ClosuresMap.

vls_lookup_closure(Map, FuncId) = EnvStructId :-
    map.lookup(Map ^ vls_closures, FuncId, EnvStructId).

%-----------------------------------------------------------------------%

vls_has_str(Map, Str) :-
    map.contains(Map ^ vls_const_data, cd_string(Str)).

%-----------------------------------------------------------------------%

vls_insert_str(String, Struct, Field, Width, !Map) :-
    map.det_insert(cd_string(String),
        vl_env(vln_struct(Struct, Field, Width, vln_done)),
        !.Map ^ vls_const_data, ConstMap),
    !Map ^ vls_const_data := ConstMap.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- type val_locn_map
    --->    vlm_root(
                vlmr_static             :: val_locn_map_static,
                vlmr_vars               :: map(var, val_locn)
            )
    ;       vlm_env(
                vlme_parent             :: val_locn_map,
                vlme_vars               :: map(var, val_locn),
                vlme_struct             :: pzs_id,
                vlme_field              :: field_num,
                vlme_width              :: pz_width
            ).

%-----------------------------------------------------------------------%

vl_start_var_binding(Static, vlm_root(Static, map.init)).

%-----------------------------------------------------------------------%

vl_push_env(Struct, Field, LocnMap,
        vlm_env(LocnMap, init, Struct, Field, Width)) :-
    Width = pzw_ptr.

%-----------------------------------------------------------------------%

vl_put_var(Var, Depth, !Map) :-
    vl_set_var_1(Var, vl_stack(Depth, vln_done), !Map).

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

vl_set_var_env(Var, Struct, Field, Width, !Map) :-
    vl_set_var_1(Var, vl_env(vln_struct(Struct, Field, Width, vln_done)), !Map).

%-----------------------------------------------------------------------%

:- pred vl_set_var_1(var::in, val_locn::in,
    val_locn_map::in, val_locn_map::out) is det.

vl_set_var_1(Var, Locn,
        vlm_root(Static, !.VarsMap), vlm_root(Static, !:VarsMap)) :-
    map.det_insert(Var, Locn, !VarsMap).
vl_set_var_1(Var, Locn,
        vlm_env(P, !.VarsMap, S, F, W), vlm_env(P, !:VarsMap, S, F, W)) :-
    map.det_insert(Var, Locn, !VarsMap).

%-----------------------------------------------------------------------%

vl_lookup_proc(vlm_root(Static, _), FuncId) = Locn :-
    map.lookup(Static ^ vls_proc_id_map, FuncId, Locn).
vl_lookup_proc(vlm_env(Parent, _, _, _, _), FuncId) = Locn :-
    Locn = proc_maybe_in_struct(vl_lookup_proc(Parent, FuncId)).

vl_lookup_proc_id(vlm_root(Static, _), FuncId) =
    vls_lookup_proc_id(Static, FuncId).
vl_lookup_proc_id(vlm_env(Parent, _, _, _, _), FuncId) =
    vl_lookup_proc_id(Parent, FuncId).

%-----------------------------------------------------------------------%

vl_lookup_closure(vlm_root(Static, _), FuncId) = StructId :-
    map.lookup(Static ^ vls_closures, FuncId, StructId).
vl_lookup_closure(vlm_env(Parent, _, _, _, _), FuncId) =
    vl_lookup_closure(Parent, FuncId).

%-----------------------------------------------------------------------%

vl_lookup_var(vlm_root(_, VarsMap), Var) = Locn :-
    map.lookup(VarsMap, Var, Locn).
vl_lookup_var(vlm_env(Parent, VarsMap, Struct, Field, Width), Var) = Locn :-
    ( if map.search(VarsMap, Var, LocnPrime) then
        Locn = LocnPrime
    else
        Locn0 = vl_lookup_var(Parent, Var),
        Locn = val_maybe_in_struct(Struct, Field, Width, Locn0)
    ).

%-----------------------------------------------------------------------%

vl_lookup_str(vlm_root(Static, _), Str) = Locn :-
    map.lookup(Static ^ vls_const_data, cd_string(Str), Locn).
vl_lookup_str(vlm_env(Parent, _, _, _, _), Str) =
    vl_lookup_str(Parent, Str).

%-----------------------------------------------------------------------%

:- import_module util.

:- func proc_maybe_in_struct(proc_locn) = proc_locn.

proc_maybe_in_struct(P@pl_instrs(_)) = P.
proc_maybe_in_struct(P@pl_static_proc(_)) = P.
proc_maybe_in_struct(pl_import(_, _)) = _ :-
    util.sorry($file, $pred, "Import").

:- func val_maybe_in_struct(pzs_id, field_num, pz_width, val_locn) = val_locn.

val_maybe_in_struct(_, _, _,            Val@vl_stack(_, _)) = Val.
val_maybe_in_struct(StructId, Field, Width, vl_env(Next0)) =
    vl_env(vln_struct(StructId, Field, Width, Next0)).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
