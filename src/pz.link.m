%-----------------------------------------------------------------------%
% Plasma linking code
% vim: ts=4 sw=4 et
%
% Copyright (C) 2020 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
% This program links the pz intermediate representation.
%
%-----------------------------------------------------------------------%
:- module pz.link.
%-----------------------------------------------------------------------%
:- interface.

:- import_module maybe.

:- import_module q_name.
:- import_module util.
:- import_module util.result.

:- type link_error == string.

:- pred do_link(nq_name::in, maybe(q_name)::in, list(pz)::in,
    result(pz, link_error)::out) is det.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
:- implementation.

:- import_module array.
:- import_module cord.
:- import_module int.
:- import_module map.
:- import_module pair.
:- import_module require.
:- import_module uint32.

:- import_module context.
:- import_module pz.code.
:- import_module pz.bytecode.
:- import_module pz.pz_ds.
:- import_module util.exception.

%-----------------------------------------------------------------------%

do_link(Name, MaybeEntry, Inputs, Result) :-
    some [!PZ, !Errors] (
        !:Errors = init,

        % Calculate the IDs of all the entries in the new PZ file.  Also build a
        % map from module names to modules and count the various entries.
        build_input_maps(Inputs, IdMap, ModNameMap, NumStructs, NumDatas,
            NumProcs, NumClosures),

        !:PZ = init_pz(q_name(Name), 0u32, NumStructs, NumDatas, NumProcs,
            NumClosures),

        % Build a map of exports. This will be used to determine what can be
        % linked too.
        foldl2(build_export_map(!.PZ, IdMap), Inputs, 0, _, init, ExportMap),

        % Link the files by each entry type at a time, eg: all the structs for
        % all the inputs, then all the datas for all the inputs.

        foldl2(link_structs(IdMap), Inputs, 0, _, !PZ),
        % Process imports, those found in ExportMap will be linked and the
        % others will become imports in !PZ.
        foldl3(link_imports(ExportMap), Inputs, 0, _, !PZ, init, CloLinkMap),
        foldl2(link_datas(IdMap, CloLinkMap), Inputs, 0, _, !PZ),
        foldl2(link_procs(IdMap, CloLinkMap), Inputs, 0, _, !PZ),
        foldl2(link_closures(IdMap), Inputs, 0, _, !PZ),

        find_entrypoint(Inputs, ModNameMap, MaybeEntry, MaybeEntryRes),
        ( MaybeEntryRes = ok(no)
        ; MaybeEntryRes = ok(yes(Entry)),
            pz_set_entry_closure(Entry, !PZ)
        ; MaybeEntryRes = errors(Errors),
            add_errors(Errors, !Errors)
        ),

        ( if is_empty(!.Errors) then
            Result = ok(!.PZ)
        else
            Result = errors(!.Errors)
        )
    ).

:- pred build_export_map(pz::in, id_map::in,
    pz::in, int::in, int::out, export_map::in, export_map::out) is det.

build_export_map(PZ, IdMap, Input, InputNum, InputNum+1, !Exports) :-
    Exports = from_assoc_list(map((func(Name - Id0) = Name - Id :-
            Id = transform_closure_id(PZ, IdMap, InputNum, Id0)
        ), pz_get_exports(Input))),
    det_insert(pz_get_module_name(Input), Exports, !Exports).

:- pred link_imports(export_map::in, pz::in, int::in, int::out,
    pz::in, pz::out, link_map::in, link_map::out) is det.

link_imports(ModuleMap, Input, InputNum, InputNum+1, !PZ, !LinkMap) :-
    Imports = pz_get_imports(Input),
    foldl2(link_imports_2(ModuleMap, InputNum), Imports, !PZ, !LinkMap).

:- pred link_imports_2(export_map::in, int::in, pair(pzi_id, q_name)::in,
    pz::in, pz::out, link_map::in, link_map::out) is det.

link_imports_2(ExportMap0, InputNum, ImportId - Name, !PZ, !LinkMap) :-
    ( if
        q_name_parts(Name, yes(Module), Symbol),
        search(ExportMap0, Module, ExportMap),
        ( if search(ExportMap, Symbol, ClosureId0) then
            ClosureId = ClosureId0
        else
            % This could almost be a compilation error, it shouldn't be
            % possible though but we could reconsider it.
            unexpected($file, $pred,
                format("Unknown symbol `%s`\n", [s(q_name_to_string(Name))]))
        )
    then
        det_insert({InputNum, ImportId}, link_to(ClosureId), !LinkMap)
    else
        pz_new_import(NewImportId, Name, !PZ),
        det_insert({InputNum, ImportId}, link_external(NewImportId),
            !LinkMap)
    ).

:- pred link_structs(id_map::in, pz::in, int::in, int::out,
    pz::in, pz::out) is det.

link_structs(IdMap, Input, InputNum, InputNum+1, !PZ) :-
    Structs = pz_get_structs(Input),
    foldl(link_structs_2(IdMap, InputNum), Structs, !PZ).

:- pred link_structs_2(id_map::in, int::in, pair(pzs_id, pz_named_struct)::in,
    pz::in, pz::out) is det.

link_structs_2(IdMap, InputNum, SId0 - pz_named_struct(Name, Struct), !PZ) :-
    SId = transform_struct_id(!.PZ, IdMap, InputNum, SId0),
    pz_add_struct(SId, Name, Struct, !PZ).

:- pred link_datas(id_map::in, link_map::in, pz::in, int::in, int::out,
    pz::in, pz::out) is det.

link_datas(IdMap, LinkMap, Input, InputNum, InputNum+1, !PZ) :-
    Datas = pz_get_data_items(Input),
    foldl(link_datas_2(IdMap, LinkMap, InputNum), Datas, !PZ).

:- pred link_datas_2(id_map::in, link_map::in, int::in,
    pair(pzd_id, pz_data)::in, pz::in, pz::out) is det.

link_datas_2(IdMap, LinkMap, InputNum, DataId0 - Data0, !PZ) :-
    Data0 = pz_data(Type0, Values0),
    ( Type0 = type_array(_, _),
        Type = Type0
    ; Type0 = type_struct(OldId),
        NewId = transform_struct_id(!.PZ, IdMap, InputNum, OldId),
        Type = type_struct(NewId)
    ),
    Values = map(transform_value(!.PZ, IdMap, LinkMap, InputNum), Values0),
    Data = pz_data(Type, Values),

    DataId = transform_data_id(!.PZ, IdMap, InputNum, DataId0),
    pz_add_data(DataId, Data, !PZ).

:- pred link_procs(id_map::in, link_map::in, pz::in, int::in, int::out,
    pz::in, pz::out) is det.

link_procs(IdMap, LinkMap, Input, InputNum, InputNum+1, !PZ) :-
    Procs = pz_get_procs(Input),
    foldl(link_proc(IdMap, LinkMap, InputNum), Procs, !PZ).

:- pred link_proc(id_map::in, link_map::in, int::in,
    pair(pzp_id, pz_proc)::in, pz::in, pz::out) is det.

link_proc(IdMap, LinkMap, Input, ProcId0 - Proc0, !PZ) :-
    pz_proc(Name, Signature, MaybeBlocks0) = Proc0,
    ( MaybeBlocks0 = no,
        MaybeBlocks = no
    ; MaybeBlocks0 = yes(Blocks0),
        Blocks = map(link_block(!.PZ, IdMap, LinkMap, Input), Blocks0),
        MaybeBlocks = yes(Blocks)
    ),
    Proc = pz_proc(Name, Signature, MaybeBlocks),
    ProcId = transform_proc_id(!.PZ, IdMap, Input, ProcId0),
    pz_add_proc(ProcId, Proc, !PZ).

:- func link_block(pz, id_map, link_map, int, pz_block) = pz_block.

link_block(PZ, IdMap, LinkMap, Input, pz_block(Instrs)) =
    pz_block(map(link_instr_obj(PZ, IdMap, LinkMap, Input), Instrs)).

:- func link_instr_obj(pz, id_map, link_map, int, pz_instr_obj) = pz_instr_obj.

link_instr_obj(PZ, IdMap, LinkMap, Input, pzio_instr(Instr)) =
    pzio_instr(link_instr(PZ, IdMap, LinkMap, Input, Instr)).
link_instr_obj(PZ, IdMap, _,       Input, pzio_context(Context)) =
    pzio_context(link_context(PZ, IdMap, Input, Context)).
link_instr_obj(_,  _,     _,       _,     pzio_comment(Comment)) =
    pzio_comment(Comment).

:- func link_instr(pz, id_map, link_map, int, pz_instr) = pz_instr.

link_instr(PZ, IdMap, LinkMap, Input, Instr0) = Instr :-
    instruction(Instr0, Opcode, Width, MaybeImm0),
    ( MaybeImm0 = no,
        MaybeImm = no
    ; MaybeImm0 = yes(Imm0),
        (
            ( Imm0 = pz_im_i8(_)
            ; Imm0 = pz_im_u8(_)
            ; Imm0 = pz_im_i16(_)
            ; Imm0 = pz_im_u16(_)
            ; Imm0 = pz_im_i32(_)
            ; Imm0 = pz_im_u32(_)
            ; Imm0 = pz_im_i64(_)
            ; Imm0 = pz_im_u64(_)
            ; Imm0 = pz_im_label(_)
            ; Imm0 = pz_im_depth(_)
            ),
            Imm = Imm0
        ; Imm0 = pz_im_closure(CloId),
            Imm = pz_im_closure(transform_closure_id(PZ, IdMap, Input, CloId))
        ; Imm0 = pz_im_proc(ProcId),
            Imm = pz_im_proc(transform_proc_id(PZ, IdMap, Input, ProcId))
        ; Imm0 = pz_im_import(ImportId),
            LinkDest = transform_import_id(LinkMap, Input, ImportId),
            ( LinkDest = link_to(ClosureId),
                Imm = pz_im_closure(ClosureId)
            ; LinkDest = link_external(NewImportId),
                Imm = pz_im_import(NewImportId)
            )
        ; Imm0 = pz_im_struct(StructId),
            Imm = pz_im_struct(transform_struct_id(PZ, IdMap, Input, StructId))
        ; Imm0 = pz_im_struct_field(StructId, FieldNo),
            Imm = pz_im_struct_field(
                transform_struct_id(PZ, IdMap, Input, StructId),
                FieldNo)
        ),
        MaybeImm = yes(Imm)
    ),
    ( if instruction(InstrPrime, Opcode, Width, MaybeImm) then
        Instr = InstrPrime
    else
        unexpected($file, $pred, "Instruction encoding bug")
    ).

:- func link_context(pz, id_map, int, pz_context) = pz_context.

link_context(PZ, IdMap, InputNum, pz_context(OrigContext, FileData)) =
    pz_context(OrigContext, transform_data_id(PZ, IdMap, InputNum, FileData)).
link_context(_,  _,     _,        pz_context_short(Line)) =
    pz_context_short(Line).
link_context(_,  _,     _,        pz_nil_context) =
    pz_nil_context.

:- pred link_closures(id_map::in, pz::in, int::in, int::out, pz::in, pz::out)
    is det.

link_closures(IdMap, Input, InputNum, InputNum+1, !PZ) :-
    Closures = pz_get_closures(Input),
    foldl(link_closure(IdMap, InputNum), Closures, !PZ).

:- pred link_closure(id_map::in, int::in, pair(pzc_id, pz_closure)::in,
    pz::in, pz::out) is det.

link_closure(IdMap, InputNum, CID0 - pz_closure(Proc, Data), !PZ) :-
    Closure = pz_closure(
        transform_proc_id(!.PZ, IdMap, InputNum, Proc),
        transform_data_id(!.PZ, IdMap, InputNum, Data)),
    CID = transform_closure_id(!.PZ, IdMap, InputNum, CID0),
    pz_add_closure(CID, Closure, !PZ).

:- pred find_entrypoint(list(pz)::in, map(q_name, pz)::in,
    maybe(q_name)::in, result(maybe(pz_entrypoint), link_error)::out) is det.

find_entrypoint(_, ModNameMap, yes(EntryName), Result) :-
    ( if
        map.search(ModNameMap, EntryName, Module),
        % XXX: Do we need to translate this closure ID?
        yes(Entry) = pz_get_maybe_entry_closure(Module)
    then
        Result = ok(yes(Entry))
    else
        Result = return_error(context("command line", 0, 0),
            "Cannot find entrypoint symbol")
    ).
find_entrypoint(Inputs, _, no, Result) :-
    ( if
        Inputs = [Only],
        yes(Entry) = pz_get_maybe_entry_closure(Only)
    then
        Result = ok(yes(Entry))
    else
        Result = ok(no)
    ).

%-----------------------------------------------------------------------%

:- func transform_value(pz, id_map, link_map, int, pz_data_value) =
    pz_data_value.

transform_value(_,  _,     _,       _,     pzv_num(Num)) =
        pzv_num(Num).
transform_value(PZ, IdMap, _,       Input, pzv_data(OldId)) =
        pzv_data(NewId) :-
    NewId = transform_data_id(PZ, IdMap, Input, OldId).
transform_value(_,  _,     LinkMap, Input, pzv_import(OldId)) =
        Value :-
    LinkDest = transform_import_id(LinkMap, Input, OldId),
    ( LinkDest = link_to(ClosureId),
        Value = pzv_closure(ClosureId)
    ; LinkDest = link_external(NewImportId),
        Value = pzv_import(NewImportId)
    ).
transform_value(PZ, IdMap, _,       Input, pzv_closure(OldId)) =
        pzv_closure(NewId) :-
    NewId = transform_closure_id(PZ, IdMap, Input, OldId).

%-----------------------------------------------------------------------%

:- type link_map ==
    map({int, pzi_id}, link_dest).

:- type link_dest
    --->    link_to(pzc_id)
    ;       link_external(pzi_id).

:- type id_map
    --->    id_map(
                idm_struct_offsets  :: array(uint32),
                idm_data_offsets    :: array(uint32),
                idm_proc_offsets    :: array(uint32),
                idm_closure_offsets :: array(uint32)
            ).

:- type export_map == map(q_name, map(nq_name, pzc_id)).

:- pred build_input_maps(list(pz)::in, id_map::out, map(q_name, pz)::out,
    uint32::out, uint32::out, uint32::out, uint32::out) is det.

build_input_maps(Inputs, IdMap, NameMap, NumStructs, NumDatas, NumProcs,
        NumClosures) :-
    calculate_offsets_and_build_maps(Inputs,
        0u32, NumStructs, [], StructOffsetsList,
        0u32, NumDatas, [], DataOffsetsList,
        0u32, NumProcs, [], ProcOffsetsList,
        0u32, NumClosures, [], ClosureOffsetsList,
        init, NameMap),
    StructOffsets = array(StructOffsetsList),
    DataOffsets = array(DataOffsetsList),
    ProcOffsets = array(ProcOffsetsList),
    ClosureOffsets = array(ClosureOffsetsList),

    IdMap = id_map(StructOffsets, DataOffsets, ProcOffsets, ClosureOffsets).

:- pred calculate_offsets_and_build_maps(list(pz)::in,
    uint32::in, uint32::out, list(uint32)::in, list(uint32)::out,
    uint32::in, uint32::out, list(uint32)::in, list(uint32)::out,
    uint32::in, uint32::out, list(uint32)::in, list(uint32)::out,
    uint32::in, uint32::out, list(uint32)::in, list(uint32)::out,
    map(q_name, pz)::in, map(q_name, pz)::out) is det.

calculate_offsets_and_build_maps([],
        !NumStructs, !StructOffsets,
        !NumDatas, !DataOffsets,
        !NumProcs, !ProcOffsets,
        !NumClosures, !ClosureOffsets,
        !NameMap) :-
    reverse(!StructOffsets),
    reverse(!DataOffsets),
    reverse(!ProcOffsets),
    reverse(!ClosureOffsets).
calculate_offsets_and_build_maps([Input | Inputs],
        !StructOffset, !StructOffsets,
        !DataOffset, !DataOffsets,
        !ProcOffset, !ProcOffsets,
        !ClosureOffset, !ClosureOffsets,
        !NameMap) :-

    !:StructOffset = !.StructOffset + pz_get_num_structs(Input),
    !:StructOffsets = [!.StructOffset | !.StructOffsets],

    !:DataOffset = !.DataOffset + pz_get_num_datas(Input),
    !:DataOffsets = [!.DataOffset | !.DataOffsets],

    !:ProcOffset = !.ProcOffset + pz_get_num_procs(Input),
    !:ProcOffsets = [!.ProcOffset | !.ProcOffsets],

    !:ClosureOffset = !.ClosureOffset + pz_get_num_closures(Input),
    !:ClosureOffsets = [!.ClosureOffset | !.ClosureOffsets],

    ( if insert(pz_get_module_name(Input), Input, !NameMap) then
        true
    else
        compile_error($file, $pred,
            "Cannot link two modules containing the same module")
    ),

    calculate_offsets_and_build_maps(Inputs,
        !StructOffset, !StructOffsets,
        !DataOffset, !DataOffsets,
        !ProcOffset, !ProcOffsets,
        !ClosureOffset, !ClosureOffsets,
        !NameMap).

%-----------------------------------------------------------------------%

:- func transform_import_id(link_map, int, pzi_id) = link_dest.

transform_import_id(LinkMap, InputNum, OldId) = LinkDest :-
    lookup(LinkMap, {InputNum, OldId}, LinkDest).

:- func transform_struct_id(pz, id_map, int, pzs_id) = pzs_id.

transform_struct_id(PZ, IdMap, InputNum, OldId) =
    transform_id(pzs_id_get_num, pzs_id_from_num(PZ),
        IdMap ^ idm_struct_offsets, InputNum, OldId).

:- func transform_data_id(pz, id_map, int, pzd_id) = pzd_id.

transform_data_id(PZ, IdMap, InputNum, OldId) =
    transform_id(pzd_id_get_num, pzd_id_from_num(PZ),
        IdMap ^ idm_data_offsets, InputNum, OldId).

:- func transform_proc_id(pz, id_map, int, pzp_id) = pzp_id.

transform_proc_id(PZ, IdMap, InputNum, ProcId) =
    transform_id(pzp_id_get_num, pzp_id_from_num(PZ),
        IdMap ^ idm_proc_offsets, InputNum, ProcId).

:- func transform_closure_id(pz, id_map, int, pzc_id) = pzc_id.

transform_closure_id(PZ, IdMap, InputNum, ClosureId) =
    transform_id(pzc_id_get_num, pzc_id_from_num(PZ),
        IdMap ^ idm_closure_offsets, InputNum, ClosureId).

:- func transform_id(func(Id) = uint32, pred(uint32, Id),
    array(uint32), int, Id) = Id.
:- mode transform_id(in, pred(in, out) is semidet,
    in, in, in) = (out) is det.

transform_id(GetNum, FromNum, Offsets, InputNum, OldId) = NewId :-
    ( if InputNum > 0 then
        OldIdNum = GetNum(OldId),
        NewIdNum = OldIdNum + Offsets ^ elem(InputNum - 1),
        ( if FromNum(NewIdNum, NewIdPrime) then
            NewId = NewIdPrime
        else
            unexpected($file, $pred, "Bad id")
        )
    else
        NewId = OldId
    ).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
