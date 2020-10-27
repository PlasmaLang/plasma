%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module pz.pz_ds.
%
% Low level plasma data structure.
%
% Copyright (C) 2015-2020 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
%-----------------------------------------------------------------------%
:- interface.

:- import_module assoc_list.
:- import_module map.
:- import_module maybe.
:- import_module set.

:- import_module pz.code.
:- import_module q_name.

%-----------------------------------------------------------------------%

    % Structure ID
    %
:- type pzs_id.

:- func pzs_id_get_num(pzs_id) = uint32.

:- pred pzs_id_from_num(pz::in, uint32::in, pzs_id::out) is semidet.

    % Imported procedure ID
    %
:- type pzi_id.

:- func pzi_id_get_num(pzi_id) = uint32.

:- pred pzi_id_from_num(pz::in, uint32::in, pzi_id::out) is semidet.

    % Procedure ID
    %
:- type pzp_id.

:- func pzp_id_get_num(pzp_id) = uint32.

:- pred pzp_id_from_num(pz::in, uint32::in, pzp_id::out) is semidet.

    % Data ID
    %
:- type pzd_id.

:- func pzd_id_get_num(pzd_id) = uint32.
:- pred pzd_id_from_num(pz::in, uint32::in, pzd_id::out) is semidet.

    % Closure ID
    %
:- type pzc_id.

:- func pzc_id_get_num(pzc_id) = uint32.

:- pred pzc_id_from_num(pz::in, uint32::in, pzc_id::out) is semidet.

%-----------------------------------------------------------------------%

:- type pz.

%-----------------------------------------------------------------------%

    % init_pz(ModuleName, FileType)
    % init_pz(ModuleName, FileType, NumImports, NumStructs, NumProcs, NumDatas,
    %   NumClosures).
    %
:- func init_pz(q_name, pz_file_type) = pz.
:- func init_pz(q_name, pz_file_type, uint32, uint32, uint32, uint32, uint32)
    = pz.

%-----------------------------------------------------------------------%

:- func pz_get_module_name(pz) = q_name.

:- func pz_get_file_type(pz) = pz_file_type.

%-----------------------------------------------------------------------%

:- type pz_entrypoint
    --->    pz_entrypoint(
                pz_ep_closure       :: pzc_id,
                pz_ep_signature     :: pz_entry_signature,
                pz_ep_name          :: q_name
            ).

:- pred pz_set_entry_closure(pzc_id::in, pz_entry_signature::in,
    pz::in, pz::out) is det.

:- func pz_get_maybe_entry_closure(pz) = maybe(pz_entrypoint).

:- pred pz_add_entry_candidate(pzc_id::in, pz_entry_signature::in,
    pz::in, pz::out) is det.

:- func pz_get_entry_candidates(pz) = set(pz_entrypoint).

%-----------------------------------------------------------------------%

:- func pz_get_structs(pz) = assoc_list(pzs_id, pz_named_struct).

:- func pz_get_num_structs(pz) = uint32.

:- func pz_get_struct_names_map(pz) = map(pzs_id, string).

:- func pz_lookup_struct(pz, pzs_id) = pz_struct.

:- pred pz_new_struct_id(pzs_id::out, string::in, pz::in, pz::out) is det.

:- pred pz_add_struct(pzs_id::in, pz_struct::in, pz::in, pz::out) is det.
:- pred pz_add_struct(pzs_id::in, string::in, pz_struct::in, pz::in, pz::out)
    is det.

%-----------------------------------------------------------------------%

:- func pz_get_imports(pz) = assoc_list(pzi_id, q_name).

:- func pz_get_num_imports(pz) = uint32.

:- func pz_lookup_import(pz, pzi_id) = q_name.

:- pred pz_new_import(pzi_id::out, q_name::in, pz::in, pz::out) is det.

:- pred pz_add_import(pzi_id::in, q_name::in, pz::in, pz::out) is det.

%-----------------------------------------------------------------------%

:- pred pz_new_proc_id(pzp_id::out, pz::in, pz::out) is det.

:- pred pz_add_proc(pzp_id::in, pz_proc::in, pz::in, pz::out) is det.

:- func pz_get_procs(pz) = assoc_list(pzp_id, pz_proc).

:- func pz_lookup_proc(pz, pzp_id) = pz_proc.

:- func pz_get_num_procs(pz) = uint32.

%-----------------------------------------------------------------------%

:- pred pz_new_data_id(pzd_id::out, pz::in, pz::out) is det.

:- pred pz_add_data(pzd_id::in, pz_data::in, pz::in, pz::out) is det.

:- func pz_lookup_data(pz, pzd_id) = pz_data.

:- func pz_get_data_items(pz) = assoc_list(pzd_id, pz_data).

:- func pz_get_num_datas(pz) = uint32.

%-----------------------------------------------------------------------%

:- pred pz_new_closure_id(pzc_id::out, pz::in, pz::out) is det.

:- pred pz_add_closure(pzc_id::in, pz_closure::in, pz::in, pz::out) is det.

:- func pz_get_closures(pz) = assoc_list(pzc_id, pz_closure).

:- func pz_get_num_closures(pz) = uint32.

%-----------------------------------------------------------------------%

:- func pz_get_exports(pz) = assoc_list(q_name, pzc_id) is det.

:- pred pz_export_closure(pzc_id::in, q_name::in, pz::in, pz::out) is det.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
:- implementation.

:- import_module pair.
:- import_module require.
:- import_module uint32.

%-----------------------------------------------------------------------%

:- type pzs_id
    ---> pzs_id(pzs_id_num  :: uint32).

pzs_id_get_num(pzs_id(Num)) = Num.

pzs_id_from_num(PZ, Num, pzs_id(Num)) :-
    Num < PZ ^ pz_next_struct_id ^ pzs_id_num.

%-----------------------------------------------------------------------%

:- type pzi_id
    ---> pzi_id(pzi_id_num  :: uint32).

pzi_id_get_num(pzi_id(Num)) = Num.

pzi_id_from_num(PZ, Num, pzi_id(Num)) :-
    Num < PZ ^ pz_next_import_id ^ pzi_id_num.

%-----------------------------------------------------------------------%

:- type pzp_id
    ---> pzp_id(pzp_id_num :: uint32).

pzp_id_get_num(pzp_id(Num)) = Num.

pzp_id_from_num(PZ, Num, pzp_id(Num)) :-
    Num < PZ ^ pz_next_proc_id ^ pzp_id_num.

%-----------------------------------------------------------------------%

:- type pzd_id
    ---> pzd_id(pzd_id_num  :: uint32).

pzd_id_get_num(pzd_id(Num)) = Num.

pzd_id_from_num(PZ, Num, pzd_id(Num)) :-
    Num < PZ ^ pz_next_data_id ^ pzd_id_num.

%-----------------------------------------------------------------------%

:- type pzc_id
    ---> pzc_id(pzc_id_num  :: uint32).

pzc_id_get_num(pzc_id(Num)) = Num.

pzc_id_from_num(PZ, Num, pzc_id(Num)) :-
    Num < PZ ^ pz_next_closure_id ^ pzc_id_num.

%-----------------------------------------------------------------------%

:- type pz
    ---> pz(
        pz_module_name              :: q_name,
        pz_file_type                :: pz_file_type,

        pz_structs                  :: map(pzs_id, {string, maybe(pz_struct)}),
        pz_next_struct_id           :: pzs_id,

        pz_imports                  :: map(pzi_id, q_name),
        pz_next_import_id           :: pzi_id,

        pz_procs                    :: map(pzp_id, pz_proc),
        pz_next_proc_id             :: pzp_id,

        pz_data                     :: map(pzd_id, pz_data),
        pz_next_data_id             :: pzd_id,

        pz_closures                 :: map(pzc_id, pz_closure_maybe_export),
        pz_next_closure_id          :: pzc_id,
        pz_maybe_entry              :: maybe(pz_entrypoint_internal),
        pz_entry_candidates         :: set(pz_entrypoint_internal)
    ).

:- type pz_closure_maybe_export
    --->    pz_closure(pz_closure)
    ;       pz_exported_closure(q_name, pz_closure).

:- type pz_entrypoint_internal
    --->    pz_entrypoint_internal(
                pz_epi_closure          :: pzc_id,
                pz_epi_signature        :: pz_entry_signature
            ).

%-----------------------------------------------------------------------%

init_pz(ModuleName, FileType) = pz(ModuleName, FileType,
    init, pzs_id(0u32),
    init, pzi_id(0u32),
    init, pzp_id(0u32),
    init, pzd_id(0u32),
    init, pzc_id(0u32),
    no, init).

init_pz(ModuleName, FileType, NumImports, NumStructs, NumDatas, NumProcs,
        NumClosures) =
    pz( ModuleName, FileType,
        init, pzs_id(NumStructs),
        init, pzi_id(NumImports),
        init, pzp_id(NumProcs),
        init, pzd_id(NumDatas),
        init, pzc_id(NumClosures),
        no, init).

%-----------------------------------------------------------------------%

pz_get_module_name(PZ) = PZ ^ pz_module_name.

pz_get_file_type(PZ) = PZ ^ pz_file_type.

%-----------------------------------------------------------------------%

pz_set_entry_closure(Clo, Sig, !PZ) :-
    Entry = pz_entrypoint_internal(Clo, Sig),
    expect(unify(no, !.PZ ^ pz_maybe_entry), $file, $pred,
        "Entry must be unset"),
    expect(entry_is_exported(!.PZ, Entry), $file, $pred,
        "Entry must be exported"),
    !PZ ^ pz_maybe_entry := yes(Entry).

pz_get_maybe_entry_closure(PZ) =
    map_maybe(entrypoint_add_name(PZ), PZ ^ pz_maybe_entry).

pz_add_entry_candidate(Closure, Signature, !PZ) :-
    Entry = pz_entrypoint_internal(Closure, Signature),
    expect(entry_is_exported(!.PZ, Entry), $file, $pred,
        "Entry must be exported"),
    !PZ ^ pz_entry_candidates := insert(!.PZ ^ pz_entry_candidates, Entry).

pz_get_entry_candidates(PZ) =
    map(entrypoint_add_name(PZ), PZ ^ pz_entry_candidates).

:- func get_name_of_export(pz, pzc_id) = q_name.

get_name_of_export(PZ, Clo) = Name :-
    Exports = reverse_members(pz_get_exports(PZ)),
    lookup(Exports, Clo, Name).

:- func entrypoint_add_name(pz, pz_entrypoint_internal) = pz_entrypoint.

entrypoint_add_name(PZ, pz_entrypoint_internal(Clo, Sig)) =
    pz_entrypoint(Clo, Sig, get_name_of_export(PZ, Clo)).

:- pred entry_is_exported(pz::in, pz_entrypoint_internal::in) is semidet.

entry_is_exported(PZ, Entry) :-
    Closures = map(snd, pz_get_exports(PZ)),
    member(Entry ^ pz_epi_closure, Closures).

%-----------------------------------------------------------------------%

pz_get_structs(PZ) = Structs :-
    filter_map(pred((K - {N, yes(S)})::in, (K - pz_named_struct(N, S))::out)
            is semidet,
        to_assoc_list(PZ ^ pz_structs), Structs).

pz_get_num_structs(PZ) = pzs_id_get_num(PZ ^ pz_next_struct_id).

pz_get_struct_names_map(PZ) = map_values(func(_, {N, _}) = N,
    PZ ^ pz_structs).

pz_lookup_struct(PZ, PZSId) = Struct :-
    {_, MaybeStruct} = map.lookup(PZ ^ pz_structs, PZSId),
    ( MaybeStruct = no,
        unexpected($file, $pred, "Struct not found")
    ; MaybeStruct = yes(Struct)
    ).

pz_new_struct_id(StructId, Name, !PZ) :-
    StructId = !.PZ ^ pz_next_struct_id,
    !PZ ^ pz_next_struct_id := pzs_id(StructId ^ pzs_id_num + 1u32),
    !PZ ^ pz_structs := det_insert(!.PZ ^ pz_structs, StructId, {Name, no}).

pz_add_struct(StructId, Struct, !PZ) :-
    Structs0 = !.PZ ^ pz_structs,
    ( if search(Structs0, StructId, {N, _}) then
        det_update(StructId, {N, yes(Struct)}, Structs0, Structs)
    else
        det_insert(StructId, {string(StructId), yes(Struct)}, Structs0, Structs)
    ),
    !PZ ^ pz_structs := Structs.

pz_add_struct(StructId, Name, Struct, !PZ) :-
    Structs0 = !.PZ ^ pz_structs,
    map.set(StructId, {Name, yes(Struct)}, Structs0, Structs),
    !PZ ^ pz_structs := Structs.

%-----------------------------------------------------------------------%

pz_get_imports(PZ) = to_assoc_list(PZ ^ pz_imports).

pz_get_num_imports(PZ) = pzi_id_get_num(PZ ^ pz_next_import_id).

pz_lookup_import(PZ, ImportId) = lookup(PZ ^ pz_imports, ImportId).

pz_new_import(ImportId, Name, !PZ) :-
    ImportId = !.PZ ^ pz_next_import_id,
    !PZ ^ pz_next_import_id := pzi_id(ImportId ^ pzi_id_num + 1u32),
    pz_add_import(ImportId, Name, !PZ).

pz_add_import(ImportId, Name, !PZ) :-
    Imports0 = !.PZ ^ pz_imports,
    map.det_insert(ImportId, Name, Imports0, Imports),
    !PZ ^ pz_imports := Imports.

%-----------------------------------------------------------------------%

pz_new_proc_id(ProcId, !PZ) :-
    ProcId = !.PZ ^ pz_next_proc_id,
    !PZ ^ pz_next_proc_id := pzp_id(ProcId ^ pzp_id_num + 1u32).

pz_add_proc(ProcID, Proc, !PZ) :-
    Procs0 = !.PZ ^ pz_procs,
    map.det_insert(ProcID, Proc, Procs0, Procs),
    !PZ ^ pz_procs := Procs.

pz_get_procs(PZ) = to_assoc_list(PZ ^ pz_procs).

pz_lookup_proc(PZ, PID) = map.lookup(PZ ^ pz_procs, PID).

pz_get_num_procs(PZ) = pzp_id_num(PZ ^ pz_next_proc_id).

%-----------------------------------------------------------------------%

pz_new_data_id(NewID, !PZ) :-
    NewID = !.PZ ^ pz_next_data_id,
    !PZ ^ pz_next_data_id := pzd_id(NewID ^ pzd_id_num + 1u32).

pz_add_data(DataID, Data, !PZ) :-
    Datas0 = !.PZ ^ pz_data,
    map.det_insert(DataID, Data, Datas0, Datas),
    !PZ ^ pz_data := Datas.

pz_lookup_data(PZ, DataId) = Data :-
    lookup(PZ ^ pz_data, DataId, Data).

pz_get_data_items(PZ) = to_assoc_list(PZ ^ pz_data).

pz_get_num_datas(PZ) = pzd_id_num(PZ ^ pz_next_data_id).

%-----------------------------------------------------------------------%

pz_new_closure_id(NewID, !PZ) :-
    NewID = !.PZ ^ pz_next_closure_id,
    !PZ ^ pz_next_closure_id := pzc_id(NewID ^ pzc_id_num + 1u32).

pz_add_closure(ClosureID, Closure, !PZ) :-
    Closures0 = !.PZ ^ pz_closures,
    map.det_insert(ClosureID, pz_closure(Closure), Closures0, Closures),
    !PZ ^ pz_closures := Closures.

pz_get_closures(PZ) =
    map((func(Id - CloEx) = Id - Clo :-
            ( CloEx = pz_closure(Clo)
            ; CloEx = pz_exported_closure(_, Clo)
            )
        ), to_assoc_list(PZ ^ pz_closures)).

pz_get_num_closures(PZ) = pzc_id_num(PZ ^ pz_next_closure_id).

%-----------------------------------------------------------------------%

pz_get_exports(PZ) = Exports :-
    filter_map(is_export_closure, to_assoc_list(PZ ^ pz_closures), Exports).

pz_export_closure(Id, Name, !PZ) :-
    lookup(!.PZ ^ pz_closures, Id, ClosureExport0),
    ( ClosureExport0 = pz_closure(Closure),
        ClosureExport = pz_exported_closure(Name, Closure)
    ; ClosureExport0 = pz_exported_closure(_, _),
        unexpected($file, $pred, "This closure is already exported")
    ),
    set(Id, ClosureExport, !.PZ ^ pz_closures, Closures),
    !PZ ^ pz_closures := Closures.

:- pred is_export_closure(pair(pzc_id, pz_closure_maybe_export)::in,
    pair(q_name, pzc_id)::out) is semidet.

is_export_closure(Id - pz_exported_closure(Name, _), Name - Id).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
