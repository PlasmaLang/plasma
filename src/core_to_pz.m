%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module core_to_pz.
%
% Copyright (C) 2015-2018 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
% Plasma core to pz conversion
%
%-----------------------------------------------------------------------%

:- interface.

:- import_module map.

:- import_module builtins.
:- import_module core.
:- import_module options.
:- import_module pz.
:- import_module q_name.

%-----------------------------------------------------------------------%

:- pred core_to_pz(compile_options::in, core::in, pz::out) is det.

%-----------------------------------------------------------------------%

:- func bool_width = pz_width.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module string.

:- import_module context.
:- import_module common_types.
:- import_module core.code.
:- import_module core.function.
:- import_module core.types.
:- import_module core.util.
:- import_module pz.code.
:- import_module varmap.

:- include_module core_to_pz.code.
:- include_module core_to_pz.closure.
:- include_module core_to_pz.data.
:- include_module core_to_pz.util.
:- import_module core_to_pz.code.
:- import_module core_to_pz.closure.
:- import_module core_to_pz.data.
:- import_module core_to_pz.util.

%-----------------------------------------------------------------------%

core_to_pz(CompileOpts, !.Core, !:PZ) :-
    !:PZ = init_pz,

    % Get ImportIds for builtin procedures.
    setup_pz_builtin_procs(BuiltinProcs, !PZ),

    % Make decisions about how data should be stored in memory.
    % This covers what tag values to use for each constructor and the IDs of
    % each structure.
    pz_new_struct_id(EnvStructId, !PZ),
    gen_constructor_data(!.Core, BuiltinProcs, EnvStructId, TypeTagMap,
        TypeCtorTagMap, !PZ),

    some [!ModuleClo] (
        !:ModuleClo = closure_builder_init,

        % Generate constants.
        gen_const_data(!.Core, DataMap, !ModuleClo, !PZ),
        closure_finalize_data(!.ModuleClo, EnvStructId, EnvDataId, !PZ),

        % Generate functions.
        FuncIds = core_all_functions(!.Core),
        foldl3(make_proc_id_map(!.Core), FuncIds,
            init, ProcIdMap, init, OpIdMap, !PZ),
        foldl(gen_func(CompileOpts, !.Core, OpIdMap, ProcIdMap, BuiltinProcs,
                TypeTagMap, TypeCtorTagMap, DataMap, EnvStructId),
            keys(ProcIdMap), !PZ),

        % Finalize the module closure.
        set_entrypoint(!.Core, ProcIdMap, EnvDataId, !PZ)
    ).

:- pred set_entrypoint(core::in, map(func_id, pz_proc_or_import)::in,
    pzd_id::in, pz::in, pz::out) is det.

set_entrypoint(Core, ProcIdMap, ModuleDataId, !PZ) :-
    ( if core_entry_function(Core, FuncId) then
        lookup(ProcIdMap, FuncId, ProcOrImport),
        ( ProcOrImport = pzp(ProcId)
        ; ProcOrImport = pzi(_),
            unexpected($file, $pred, "Imported procedure")
        ),

        % Make a closure for the entry function and register it as the
        % entrypoint, this is temporary while we convert to the knew module
        % structures.
        pz_new_closure_id(ClosureId, !PZ),
        pz_add_closure(ClosureId, pz_closure(ProcId, ModuleDataId), !PZ),
        pz_set_entry_closure(ClosureId, !PZ)
    else
        true
    ).

%-----------------------------------------------------------------------%

:- pred make_proc_id_map(core::in, func_id::in,
    map(func_id, pz_proc_or_import)::in, map(func_id, pz_proc_or_import)::out,
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
    map(func_id, pz_proc_or_import)::in, map(func_id, pz_proc_or_import)::out,
    pz::in, pz::out) is det.

make_proc_id_core_or_rts(FuncId, Function, !Map, !PZ) :-
    Imported = func_get_imported(Function),
    ( Imported = i_local,
        pz_new_proc_id(ProcId, !PZ),
        ProcOrImport = pzp(ProcId)
    ; Imported = i_imported,
        pz_new_import(ImportId, func_get_name(Function), !PZ),
        ProcOrImport = pzi(ImportId)
    ),
    det_insert(FuncId, ProcOrImport, !Map).

%-----------------------------------------------------------------------%

bool_width = data.bool_width.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
