%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module core_to_pz.
%
% Copyright (C) 2015-2019 Plasma Team
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
:- include_module core_to_pz.locn.
:- import_module core_to_pz.code.
:- import_module core_to_pz.closure.
:- import_module core_to_pz.data.
:- import_module core_to_pz.locn.

%-----------------------------------------------------------------------%

core_to_pz(CompileOpts, !.Core, !:PZ) :-
    !:PZ = init_pz,

    % Get ImportIds for builtin procedures.
    setup_pz_builtin_procs(BuiltinProcs, !PZ),

    % Make decisions about how data should be stored in memory.
    % This covers what tag values to use for each constructor and the IDs of
    % each structure.
    pz_new_struct_id(EnvStructId, !PZ),
    gen_constructor_data(!.Core, BuiltinProcs, TypeTagMap, TypeCtorTagMap,
        !PZ),

    some [!ModuleClo, !LocnMap] (
        !:ModuleClo = closure_builder_init(EnvStructId),
        !:LocnMap = vls_init(EnvStructId),

        % Generate constants.
        gen_const_data(!.Core, !LocnMap, !ModuleClo, !PZ),

        % Generate functions.
        FuncIds = core_all_functions(!.Core),
        foldl3(make_proc_and_struct_ids(!.Core), FuncIds, !LocnMap,
            !ModuleClo, !PZ),
        foldl(gen_func(CompileOpts, !.Core, !.LocnMap, BuiltinProcs,
                TypeTagMap, TypeCtorTagMap, EnvStructId),
            FuncIds, !PZ),

        % Finalize the module closure.
        closure_finalize_data(!.ModuleClo, EnvDataId, !PZ),
        set_entrypoint(!.Core, !.LocnMap, EnvDataId, !PZ)
    ).

:- pred set_entrypoint(core::in, val_locn_map_static::in,
    pzd_id::in, pz::in, pz::out) is det.

set_entrypoint(Core, LocnMap, ModuleDataId, !PZ) :-
    ( if core_entry_function(Core, FuncId) then
        ProcId = vls_lookup_proc_id(LocnMap, FuncId),
        pz_new_closure_id(ClosureId, !PZ),
        pz_add_closure(ClosureId, pz_closure(ProcId, ModuleDataId), !PZ),
        pz_set_entry_closure(ClosureId, !PZ)
    else
        true
    ).

%-----------------------------------------------------------------------%

    % Create proc and struct IDs for functions and any closure environments
    % they require, add these to maps and return them.
    %
:- pred make_proc_and_struct_ids(core::in, func_id::in,
    val_locn_map_static::in, val_locn_map_static::out,
    closure_builder::in, closure_builder::out, pz::in, pz::out) is det.

make_proc_and_struct_ids(Core, FuncId, !LocnMap, !BuildModClosure, !PZ) :-
    core_get_function_det(Core, FuncId, Function),
    Name = q_name_to_string(func_get_name(Function)),
    ( if func_builtin_type(Function, BuiltinType) then
        ( BuiltinType = bit_core,
            make_proc_id_core_or_rts(FuncId, Function, !LocnMap,
                !BuildModClosure, !PZ),
            ( if func_get_body(Function, _, _, _, _) then
                true
            else
                unexpected($file, $pred,
                    format("Builtin core function ('%s') has no body",
                        [s(Name)]))
            )
        ; BuiltinType = bit_inline_pz,
            ( if func_builtin_inline_pz(Function, PZInstrs) then
                vls_set_proc_instrs(FuncId, PZInstrs, !LocnMap)
            else
                unexpected($file, $pred, format(
                    "Inline PZ builtin ('%s') without list of instructions",
                    [s(Name)]))
            )
        ; BuiltinType = bit_rts,
            make_proc_id_core_or_rts(FuncId, Function, !LocnMap,
                !BuildModClosure, !PZ),
            ( if
                not func_builtin_inline_pz(Function, _),
                not func_get_body(Function, _, _, _, _)
            then
                true
            else
                unexpected($file, $pred,
                    format("RTS builtin ('%s') with a body",
                        [s(Name)]))
            )
        )
    else
        make_proc_id_core_or_rts(FuncId, Function, !LocnMap,
            !BuildModClosure, !PZ),
        ( if func_get_body(Function, _, _, _, _) then
            true
        else
            unexpected($file, $pred,
                format("Non builtin function ('%s') has no body", [s(Name)]))
        )
    ),
    Captured = func_get_captured_vars_types(Function),
    ( Captured = []
    ; Captured = [_ | _],
        pz_new_struct_id(EnvStructId, !PZ),
        vls_set_closure(FuncId, EnvStructId, !LocnMap),
        EnvStruct = pz_struct([pzw_ptr | map(type_to_pz_width, Captured)]),
        pz_add_struct(EnvStructId, EnvStruct, !PZ)
    ).

:- pred make_proc_id_core_or_rts(func_id::in, function::in,
    val_locn_map_static::in, val_locn_map_static::out,
    closure_builder::in, closure_builder::out, pz::in, pz::out) is det.

make_proc_id_core_or_rts(FuncId, Function, !LocnMap, !BuildModClosure, !PZ) :-
    Imported = func_get_imported(Function),
    ( Imported = i_local,
        pz_new_proc_id(ProcId, !PZ),
        vls_set_proc(FuncId, ProcId, !LocnMap)
    ; Imported = i_imported,
        pz_new_import(ImportId, func_get_name(Function), !PZ),
        closure_add_field(pzv_import(ImportId), FieldNum, !BuildModClosure),
        vls_set_proc_imported(FuncId, ImportId, FieldNum, !LocnMap)
    ).

%-----------------------------------------------------------------------%

bool_width = data.bool_width.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
