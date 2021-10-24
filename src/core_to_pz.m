%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module core_to_pz.
%
% Copyright (C) 2015-2021 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
% Plasma core to pz conversion
%
%-----------------------------------------------------------------------%

:- interface.

:- include_module core_to_pz.data.

:- import_module io.

:- import_module core.
:- import_module core_to_pz.data.
:- import_module options.
:- import_module pz.
:- import_module pz.pz_ds.
:- import_module util.
:- import_module util.log.

%-----------------------------------------------------------------------%

:- pred core_to_pz(log_config::in, compile_options::in, core::in, pz::out,
    type_tag_map::out, constructor_data_map::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------%

:- func bool_width = pz_width.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

:- import_module cord.
:- import_module list.
:- import_module map.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module string.

:- import_module builtins.
:- import_module common_types.
:- import_module core.code.
:- import_module core.function.
:- import_module core.types.
:- import_module pz.code.
:- import_module q_name.
:- import_module util.mercury.
:- import_module util.pretty.
:- import_module varmap.

:- include_module core_to_pz.code.
:- include_module core_to_pz.closure.
:- include_module core_to_pz.locn.
:- import_module core_to_pz.code.
:- import_module core_to_pz.closure.
:- import_module core_to_pz.locn.

%-----------------------------------------------------------------------%

core_to_pz(Verbose, CompileOpts, !.Core, !:PZ, TypeTagMap, TypeCtorTagMap,
        !IO) :-
    !:PZ = init_pz([module_name(!.Core)], pzft_object),

    % Get ImportIds for builtin procedures.
    setup_pz_builtin_procs(BuiltinProcs, !PZ),

    % Make decisions about how data should be stored in memory.
    % This covers what tag values to use for each constructor and the IDs of
    % each structure.
    pz_new_struct_id(EnvStructId, "Module struct", !PZ),
    verbose_output(Verbose, "Generating type layout (constructor tags)\n",
        !IO),
    gen_constructor_data(!.Core, BuiltinProcs, TypeTagMap, TypeCtorTagMap,
        !PZ),

    some [!ModuleClo, !LocnMap, !FilenameDataMap] (
        !:ModuleClo = closure_builder_init(EnvStructId),
        !:LocnMap = vls_init(EnvStructId),
        !:FilenameDataMap = map.init,

        % Generate constants.
        verbose_output(Verbose, "Generating constants\n", !IO),
        gen_const_data(!.Core, !LocnMap, !ModuleClo, !FilenameDataMap, !PZ),

        % Generate functions.
        Funcs = core_all_functions(!.Core),
        foldl3(make_proc_and_struct_ids, Funcs, !LocnMap, !ModuleClo, !PZ),
        DefinedFuncs = core_all_defined_functions(!.Core),
        verbose_output(Verbose,
            format("Generating %d functions\n", [i(length(DefinedFuncs))]),
            !IO),
        foldl(gen_func(CompileOpts, !.Core, !.LocnMap, BuiltinProcs,
                !.FilenameDataMap, TypeTagMap, TypeCtorTagMap, EnvStructId),
            DefinedFuncs, !PZ),

        % Finalize the module closure.
        verbose_output(Verbose, "Generating module closure\n", !IO),
        closure_finalize_data(!.ModuleClo, EnvDataId, !PZ),
        ExportFuncs0 = core_all_exported_functions(!.Core),

        % Export and mark the entrypoint.
        verbose_output(Verbose, "Generating entrypoint and exports\n", !IO),
        Candidates = core_entry_candidates(!.Core),
        set.fold(create_entry_candidate(!.Core, !.LocnMap, EnvDataId),
            Candidates, !PZ),
        CandidateIDs = map(entry_get_func_id, Candidates),
        ExportFuncs = filter(
            pred(Id - _::in) is semidet :- not member(Id, CandidateIDs),
            ExportFuncs0),

        % Export the other exported functions.
        map_foldl(create_export(!.LocnMap, EnvDataId), ExportFuncs, _, !PZ)
    ).

:- func entry_get_func_id(core_entrypoint) = func_id.

entry_get_func_id(entry_plain(FuncId)) = FuncId.
entry_get_func_id(entry_argv(FuncId)) = FuncId.

:- pred create_entry_candidate(core::in, val_locn_map_static::in,
    pzd_id::in, core_entrypoint::in, pz::in, pz::out) is det.

create_entry_candidate(Core, LocnMap, EnvDataId, Entrypoint, !PZ) :-
    ( Entrypoint = entry_plain(EntryFuncId),
        Signature = pz_es_plain
    ; Entrypoint = entry_argv(EntryFuncId),
        Signature = pz_es_args
    ),
    core_get_function_det(Core, EntryFuncId, EntryFunc),
    create_export(LocnMap, EnvDataId,
        EntryFuncId - EntryFunc, EntryClo, !PZ),
    pz_add_entry_candidate(EntryClo, Signature, !PZ).

:- pred create_export(val_locn_map_static::in, pzd_id::in,
    pair(func_id, function)::in, pzc_id::out, pz::in, pz::out) is det.

create_export(LocnMap, ModuleDataId, FuncId - Function, ClosureId, !PZ) :-
    ProcId = vls_lookup_proc_id(LocnMap, FuncId),
    pz_new_closure_id(ClosureId, !PZ),
    pz_add_closure(ClosureId, pz_closure(ProcId, ModuleDataId), !PZ),
    pz_export_closure(ClosureId, func_get_name(Function), !PZ).

%-----------------------------------------------------------------------%

    % Create proc and struct IDs for functions and any closure environments
    % they require, add these to maps and return them.
    %
:- pred make_proc_and_struct_ids(pair(func_id, function)::in,
    val_locn_map_static::in, val_locn_map_static::out,
    closure_builder::in, closure_builder::out, pz::in, pz::out) is det.

make_proc_and_struct_ids(FuncId - Function, !LocnMap, !BuildModClosure, !PZ) :-
    Name = q_name_to_string(func_get_name(Function)),
    ShouldGenerate = should_generate(Function),
    ( ShouldGenerate = need_codegen,
        assert_has_body(Function),
        make_proc_id_core_or_rts(FuncId, Function, !LocnMap,
            !BuildModClosure, !PZ)
    ; ShouldGenerate = need_inline_pz_and_codegen,
        ( if func_builtin_inline_pz(Function, PZInstrs) then
            vls_set_proc_instrs(FuncId, PZInstrs, !LocnMap)
        else
            unexpected($file, $pred, format(
                "Inline PZ builtin ('%s') without list of instructions",
                [s(Name)]))
        ),

        assert_has_body(Function),
        make_proc_id_core_or_rts(FuncId, Function, !LocnMap,
            !BuildModClosure, !PZ)
    ; ShouldGenerate = need_extern,
        assert_has_no_body(Function),
        make_proc_id_core_or_rts(FuncId, Function, !LocnMap,
            !BuildModClosure, !PZ)
    ; ShouldGenerate = dead_code
    ),

    Captured = func_get_captured_vars_types(Function),
    ( Captured = []
    ; Captured = [_ | _],
        pz_new_struct_id(EnvStructId, "Closure of " ++ Name, !PZ),
        vls_set_closure(FuncId, EnvStructId, !LocnMap),
        EnvStruct = pz_struct([pzw_ptr | map(type_to_pz_width, Captured)]),
        pz_add_struct(EnvStructId, EnvStruct, !PZ)
    ).

:- type generate
    --->    need_codegen
    ;       need_inline_pz_and_codegen
    ;       need_extern
    ;       dead_code.

:- func should_generate(function) = generate.

should_generate(Function) = Generate :-
    ( if func_builtin_type(Function, BuiltinType) then
        IsUsed = func_get_used(Function),
        ( IsUsed = used_probably,
            ( BuiltinType = bit_core,
                Generate = need_codegen
            ; BuiltinType = bit_inline_pz,
                % Everything with inline PZ also gets codegen.  TODO we
                % should check if address is taken to skip that most of the
                % time.
                Generate = need_inline_pz_and_codegen
            ; BuiltinType = bit_rts,
                Generate = need_extern
            )
        ; IsUsed = unused,
            Generate = dead_code
        )
    else
        Imported = func_get_imported(Function),
        ( Imported = i_local,
            Generate = need_codegen
        ; Imported = i_imported,
            IsUsed = func_get_used(Function),
            ( IsUsed = used_probably,
                Generate = need_extern
            ; IsUsed = unused,
                Generate = dead_code
            )
        )
    ).

:- pred assert_has_body(function::in) is det.

assert_has_body(Function) :-
    ( if func_get_body(Function, _, _, _, _) then
        true
    else
        Name = q_name_to_string(func_get_name(Function)),
        unexpected($file, $pred,
            format("Function ('%s') has no body",
                [s(Name)]))
    ).

:- pred assert_has_no_body(function::in) is det.

assert_has_no_body(Function) :-
    ( if
        not func_builtin_inline_pz(Function, _),
        not func_get_body(Function, _, _, _, _)
    then
        true
    else
        Name = q_name_to_string(func_get_name(Function)),
        unexpected($file, $pred,
            format("Function ('%s') doesn't have a body",
                [s(Name)]))
    ).

:- pred make_proc_id_core_or_rts(func_id::in, function::in,
    val_locn_map_static::in, val_locn_map_static::out,
    closure_builder::in, closure_builder::out, pz::in, pz::out) is det.

make_proc_id_core_or_rts(FuncId, Function, !LocnMap, !BuildModClosure, !PZ) :-
    ( if func_get_body(Function, _, _, _, _) then
        pz_new_proc_id(ProcId, !PZ),
        vls_set_proc(FuncId, ProcId, !LocnMap)
    else
        pz_new_import(ImportId, func_get_name(Function), !PZ),
        closure_add_field(pzv_import(ImportId), FieldNum, !BuildModClosure),
        vls_set_proc_imported(FuncId, ImportId, FieldNum, !LocnMap)
    ).

%-----------------------------------------------------------------------%

bool_width = data.bool_width.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
