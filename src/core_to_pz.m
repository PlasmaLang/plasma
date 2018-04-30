%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module core_to_pz.
%
% Copyright (C) 2015-2017 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
% Plasma core to pz conversion
%
%-----------------------------------------------------------------------%

:- interface.

:- import_module map.

:- import_module builtins.
:- import_module core.
:- import_module pz.
:- import_module q_name.

%-----------------------------------------------------------------------%

:- pred core_to_pz(core::in, pz::out) is det.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module string.

:- import_module common_types.
:- import_module core.function.
:- import_module core.types.
:- import_module pz.code.
:- import_module varmap.

:- include_module core_to_pz.code.
:- include_module core_to_pz.data.
:- import_module core_to_pz.code.
:- import_module core_to_pz.data.

%-----------------------------------------------------------------------%

core_to_pz(Core, !:PZ) :-
    !:PZ = init_pz,
    FuncIds = core_all_functions(Core),

    % Generate constants.
    foldl2(gen_const_data(Core), FuncIds, init, DataMap, !PZ),

    % Get ProcIds for builtin procedures.
    setup_pz_builtin_procs(BuiltinProcs, !PZ),

    % Make decisions about how data should be stored in memory.
    % This covers what tag values to use for each constructor and the IDs of
    % each structure.
    gen_constructor_data(Core, BuiltinProcs, TypeTagMap, TypeCtorTagMap, !PZ),

    % Generate functions.
    foldl3(make_proc_id_map(Core), FuncIds,
        init, ProcIdMap, init, OpIdMap, !PZ),
    map(gen_proc(Core, OpIdMap, ProcIdMap, BuiltinProcs, TypeTagMap,
            TypeCtorTagMap, DataMap),
        keys(ProcIdMap), Procs),
    foldl((pred((PID - P)::in, PZ0::in, PZ::out) is det :-
            pz_add_proc(PID, P, PZ0, PZ)
        ), Procs, !PZ),
    set_entry_function(Core, ProcIdMap, !PZ).

:- pred set_entry_function(core::in, map(func_id, pzp_id)::in,
    pz::in, pz::out) is det.

set_entry_function(Core, ProcIdMap, !PZ) :-
    ( if core_entry_function(Core, FuncId) then
        lookup(ProcIdMap, FuncId, PID),
        pz_set_entry_proc(PID, !PZ)
    else
        true
    ).

%-----------------------------------------------------------------------%

:- pred make_proc_id_map(core::in, func_id::in,
    map(func_id, pzp_id)::in, map(func_id, pzp_id)::out,
    map(func_id, list(pz_instr))::in, map(func_id, list(pz_instr))::out,
    pz::in, pz::out) is det.

make_proc_id_map(Core, FuncId, !ProcMap, !OpMap, !PZ) :-
    core_get_function_det(Core, FuncId, Function),
    Name = q_name_to_string(func_get_name(Function)),
    ( if func_builtin_type(Function, BuiltinType) then
        ( BuiltinType = bit_core,
            make_proc_id_core_or_rts(FuncId, Function, !ProcMap, !PZ),
            ( if func_get_body(Function, _, _, _) then
                true
            else
                unexpected($file, $pred,
                    format("Builtin core function ('%s') has no body",
                        [s(Name)]))
            )
        ; BuiltinType = bit_inline_pz,
            ( if func_builtin_inline_pz(Function, PZInstrs) then
                det_insert(FuncId, PZInstrs, !OpMap)
            else
                unexpected($file, $pred, format(
                    "Inline PZ builtin ('%s') without list of instructions",
                    [s(Name)]))
            )
        ; BuiltinType = bit_rts,
            make_proc_id_core_or_rts(FuncId, Function, !ProcMap, !PZ),
            ( if
                not func_builtin_inline_pz(Function, _),
                not func_get_body(Function, _, _, _)
            then
                true
            else
                unexpected($file, $pred,
                    format("RTS builtin ('%s') with a body",
                        [s(Name)]))
            )
        )
    else
        make_proc_id_core_or_rts(FuncId, Function, !ProcMap, !PZ),
        ( if func_get_body(Function, _, _, _) then
            true
        else
            unexpected($file, $pred,
                format("Non builtin function ('%s') has no body", [s(Name)]))
        )
    ).

:- pred make_proc_id_core_or_rts(func_id::in, function::in,
    map(func_id, pzp_id)::in, map(func_id, pzp_id)::out,
    pz::in, pz::out) is det.

make_proc_id_core_or_rts(FuncId, Function, !Map, !PZ) :-
    Imported = func_get_imported(Function),
    pz_new_proc_id(Imported, ProcId, !PZ),
    det_insert(FuncId, ProcId, !Map).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
