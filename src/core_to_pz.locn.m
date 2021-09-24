%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module core_to_pz.locn.
%
% Copyright (C) 2015-2021 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
% Plasma core to pz conversion - value location information
%
%-----------------------------------------------------------------------%
:- interface.

:- import_module cord.
:- import_module maybe.

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
    ;       vl_env(val_locn_next)
            % The value needs to be computed using this expression.
    ;       vl_compute(expr).

:- type val_locn_next
    --->    vln_done
            % The value is within some structure (like the environment).
    ;       vln_struct(pzs_id, field_num, pz_width, val_locn_next).

:- type proc_locn
    --->    pl_instrs(list(pz_instr), maybe(pzp_id))
    ;       pl_static_proc(pzp_id)
    ;       pl_other(val_locn).

%-----------------------------------------------------------------------%
%
% The location map information is divided into two halves, the static
% information which is static per PZ procedure.  And the dynamic
% information, which changes with code generation (for example as values are
% pushed onto the stack).
%

:- type val_locn_map_static.

:- func vls_init(pzs_id) = val_locn_map_static.

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

    % vl_setup_closure(StructId, FieldNo, !LocnMap).
    %
    % The code using !:LocnMap map executes within a closure.  The root
    % (module) environment can be found by dereferencing the environment
    % using FieldNo of StructId.
:- pred vl_setup_closure(pzs_id::in, field_num::in,
    val_locn_map::in, val_locn_map::out) is det.

:- pred vl_put_var(var::in, int::in, val_locn_map::in, val_locn_map::out)
    is det.

:- pred vl_set_var_env(var::in, pzs_id::in, field_num::in, pz_width::in,
    val_locn_map::in, val_locn_map::out) is det.

:- pred vl_set_var_expr(var::in, expr::in,
    val_locn_map::in, val_locn_map::out) is det.

:- pred vl_put_vars(list(var)::in, int::in, varmap::in,
    cord(pz_instr_obj)::out, val_locn_map::in, val_locn_map::out) is det.

:- func vl_lookup_proc(val_locn_map, func_id) = proc_locn.

:- func vl_lookup_proc_id(val_locn_map, func_id) = pzp_id.

:- func vl_lookup_closure(val_locn_map, func_id) = pzs_id.

    % This is semidet so our caller can give a clearer exception if it
    % fails.
    %
:- pred vl_search_var(val_locn_map::in, var::in, val_locn::out) is semidet.

:- func vl_lookup_str(val_locn_map, string) = val_locn.

:- func vl_lookup_mod_env(val_locn_map) = val_locn.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
:- implementation.

:- import_module int.

%-----------------------------------------------------------------------%

:- type val_locn_map_static
    --->    val_locn_map_static(
                vls_mod_env             :: pzs_id,
                vls_const_data          :: map(const_data, val_locn),
                vls_proc_id_map         :: map(func_id, proc_locn_internal),

                % Not exactly location data, but it is accessed and created
                % similarly.
                vls_closures            :: map(func_id, pzs_id)
            ).

    % Used internally.  proc_locn is just how the the result is returned.
    %
:- type proc_locn_internal
    --->    pli_instrs(list(pz_instr), maybe(pzp_id))
    ;       pli_static_proc(pzp_id)
    ;       pli_import(pzi_id, field_num).

%-----------------------------------------------------------------------%

vls_init(ModEnvStruct) = val_locn_map_static(ModEnvStruct, init, init, init).

%-----------------------------------------------------------------------%

vls_set_proc(FuncId, ProcId, !Map) :-
    ( if search(!.Map ^ vls_proc_id_map, FuncId, Locn0) then
        (
            ( Locn0 = pli_static_proc(_)
            ; Locn0 = pli_import(_, _)
            ),
            unexpected($file, $pred, "Already set")
        ; Locn0 = pli_instrs(Instrs, MaybeProc),
            ( MaybeProc = yes(_),
                unexpected($file, $pred, "Already set")
            ; MaybeProc = no,
                Locn = pli_instrs(Instrs, yes(ProcId))
            )
        )
    else
        Locn = pli_static_proc(ProcId)
    ),
    map.set(FuncId, Locn, !.Map ^ vls_proc_id_map, ProcMap),
    !Map ^ vls_proc_id_map := ProcMap.

vls_set_proc_instrs(FuncId, Instrs, !Map) :-
    ( if search(!.Map ^ vls_proc_id_map, FuncId, Locn0) then
        (
            ( Locn0 = pli_instrs(_, _)
            ; Locn0 = pli_import(_, _)
            ),
            unexpected($file, $pred, "Already set")
        ; Locn0 = pli_static_proc(ProcId),
            Locn = pli_instrs(Instrs, yes(ProcId))
        )
    else
        Locn = pli_instrs(Instrs, no)
    ),
    map.set(FuncId, Locn, !.Map ^ vls_proc_id_map, ProcMap),
    !Map ^ vls_proc_id_map := ProcMap.

vls_set_proc_imported(FuncId, ImportId, FieldNum, !Map) :-
    map.det_insert(FuncId, pli_import(ImportId, FieldNum),
        !.Map ^ vls_proc_id_map, ProcMap),
    !Map ^ vls_proc_id_map := ProcMap.

%-----------------------------------------------------------------------%

vls_lookup_proc_id(Map, FuncId) = ProcId :-
    map.lookup(Map ^ vls_proc_id_map, FuncId, Locn),
    ( Locn = pli_static_proc(ProcId)
    ; Locn = pli_instrs(_, MaybeProcId),
        ( MaybeProcId = yes(ProcId)
        ; MaybeProcId = no,
            unexpected($file, $pred, "Builtin with no proc")
        )
    ; Locn = pli_import(_, _),
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
    ;       vlm_clos(
                vlme_static             :: val_locn_map_static,
                vlme_vars               :: map(var, val_locn),
                vlme_struct             :: pzs_id,
                vlme_field              :: field_num,
                vlme_width              :: pz_width
            ).

%-----------------------------------------------------------------------%

vl_start_var_binding(Static, vlm_root(Static, map.init)).

%-----------------------------------------------------------------------%

vl_setup_closure(Struct, Field, vlm_root(Static, Vars),
    vlm_clos(Static, Vars, Struct, Field, pzw_ptr)).
vl_setup_closure(_, _, vlm_clos(_, _, _, _, _), _) :-
    unexpected($file, $pred, "Closures must be flat").

%-----------------------------------------------------------------------%

vl_put_var(Var, Depth, !Map) :-
    vl_set_var_1(Var, vl_stack(Depth, vln_done), !Map).

vl_set_var_env(Var, Struct, Field, Width, !Map) :-
    vl_set_var_1(Var, vl_env(vln_struct(Struct, Field, Width, vln_done)), !Map).

vl_set_var_expr(Var, Expr, !Map) :-
    vl_set_var_1(Var, vl_compute(Expr), !Map).

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

:- pred vl_set_var_1(var::in, val_locn::in,
    val_locn_map::in, val_locn_map::out) is det.

vl_set_var_1(Var, Locn,
        vlm_root(Static, !.VarsMap), vlm_root(Static, !:VarsMap)) :-
    map.det_insert(Var, Locn, !VarsMap).
vl_set_var_1(Var, Locn,
        vlm_clos(Sta, !.VarsMap, Str, F, W),
        vlm_clos(Sta, !:VarsMap, Str, F, W)) :-
    map.det_insert(Var, Locn, !VarsMap).

%-----------------------------------------------------------------------%

vl_lookup_proc(vlm_root(Static, _), FuncId) = Locn :-
    map.lookup(Static ^ vls_proc_id_map, FuncId, Locn0),
    Locn = proc_locn_from_internal(Static ^ vls_mod_env, Locn0).
vl_lookup_proc(vlm_clos(Static, _, Struct, Field, Width), FuncId) = Locn :-
    map.lookup(Static ^ vls_proc_id_map, FuncId, Locn0),
    Locn1 = proc_locn_from_internal(Static ^ vls_mod_env, Locn0),
    Locn = proc_maybe_in_struct(Struct, Field, Width, Locn1).

vl_lookup_proc_id(LocnMap, FuncId) =
    vls_lookup_proc_id(vl_static(LocnMap), FuncId).

%-----------------------------------------------------------------------%

vl_lookup_closure(LocnMap, FuncId) = StructId :-
    map.lookup(vl_static(LocnMap) ^ vls_closures, FuncId, StructId).

%-----------------------------------------------------------------------%

vl_search_var(vlm_root(_, VarsMap), Var, Locn) :-
    map.search(VarsMap, Var, Locn).
vl_search_var(vlm_clos(_, VarsMap, _, _, _), Var, Locn) :-
    map.search(VarsMap, Var, Locn).

%-----------------------------------------------------------------------%

vl_lookup_str(vlm_root(Static, _), Str) = Locn :-
    map.lookup(Static ^ vls_const_data, cd_string(Str), Locn).
vl_lookup_str(vlm_clos(Static, _, Struct, Field, Width), Str) =
        val_maybe_in_struct(Struct, Field, Width, Locn0) :-
    map.lookup(Static ^ vls_const_data, cd_string(Str), Locn0).

%-----------------------------------------------------------------------%

vl_lookup_mod_env(vlm_root(_, _)) = vl_env(vln_done).
vl_lookup_mod_env(vlm_clos(_, _, S, F, W)) =
    vl_env(vln_struct(S, F, W, vln_done)).

%-----------------------------------------------------------------------%

:- func proc_locn_from_internal(pzs_id, proc_locn_internal) = proc_locn.

proc_locn_from_internal(_, pli_instrs(Instrs, MbProcId)) =
    pl_instrs(Instrs, MbProcId).
proc_locn_from_internal(_, pli_static_proc(ProcId)) = pl_static_proc(ProcId).
proc_locn_from_internal(ModEnvStruct, pli_import(_, FieldNum)) =
    pl_other(vl_env(vln_struct(ModEnvStruct, FieldNum, pzw_ptr, vln_done))).

:- func proc_maybe_in_struct(pzs_id, field_num, pz_width, proc_locn) =
    proc_locn.

proc_maybe_in_struct(Struct, Field, Width, Locn0) = Locn :-
    (
        ( Locn0 = pl_instrs(_, _)
        ; Locn0 = pl_static_proc(_)
        ),
        Locn = Locn0
    ; Locn0 = pl_other(ValLocn0),
        Locn = pl_other(val_maybe_in_struct(Struct, Field, Width, ValLocn0))
    ).

:- func val_maybe_in_struct(pzs_id, field_num, pz_width, val_locn) = val_locn.

val_maybe_in_struct(_, _, _,            Val@vl_stack(_, _)) = Val.
val_maybe_in_struct(_, _, _,            Val@vl_compute(_)) = Val.
val_maybe_in_struct(StructId, Field, Width, vl_env(Next0)) =
    vl_env(vln_struct(StructId, Field, Width, Next0)).

:- func vl_static(val_locn_map) = val_locn_map_static.

vl_static(vlm_root(Static, _)) = Static.
vl_static(vlm_clos(Static, _, _, _, _)) = Static.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
