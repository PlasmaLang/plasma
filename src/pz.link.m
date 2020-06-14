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

:- pred do_link(nq_name::in, maybe(q_name)::in, list(pz)::in, pz::out) is det.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
:- implementation.

:- import_module array.
:- import_module int.
:- import_module pair.
:- import_module require.
:- import_module uint32.

:- import_module pz.bytecode.
:- import_module pz.pz_ds.
:- import_module util.

%-----------------------------------------------------------------------%

do_link(Name, MaybeEntry, Inputs, !:PZ) :-
    !:PZ = init_pz(nq_to_q_name(Name)),

    build_input_maps(Inputs, IdMap, ModNameMap),

    % Link the files by each entry type at a time, eg: all the structs for
    % all the inputs, then all the datas for all the inputs.

    foldl(link_structs, Inputs, !PZ),
    foldl(link_imports, Inputs, !PZ),
    foldl2(link_datas(IdMap), Inputs, 0, _, !PZ),
    foldl2(link_procs(IdMap), Inputs, 0, _, !PZ),
    foldl2(link_closures(IdMap), Inputs, 0, _, !PZ),

    ( if
        MaybeEntry = yes(EntryName)
    then
        ( if
            map.search(ModNameMap, EntryName, Module),
            yes(Entry) = pz_get_maybe_entry_closure(Module)
        then
            pz_set_entry_closure(Entry, !PZ)
        else
            compile_error($file, $pred, "Cannot find entrypoint symbol")
        )
    else if
        Inputs = [Only],
        yes(Entry) = pz_get_maybe_entry_closure(Only)
    then
        pz_set_entry_closure(Entry, !PZ)
    else
        true
    ).

:- pred link_imports(pz::in, pz::in, pz::out) is det.

link_imports(Input, !PZ) :-
    Imports = pz_get_imports(Input),
    foldl(link_imports_2, Imports, !PZ).

:- pred link_imports_2(pair(pzi_id, q_name)::in, pz::in, pz::out) is det.

link_imports_2(_ - Name, !PZ) :-
    pz_new_import(_, Name, !PZ).

:- pred link_structs(pz::in, pz::in, pz::out) is det.

link_structs(Input, !PZ) :-
    Structs = pz_get_structs(Input),
    foldl(link_structs_2, Structs, !PZ).

:- pred link_structs_2(pair(T, pz_named_struct)::in, pz::in, pz::out) is det.

link_structs_2(_ - pz_named_struct(Name, Struct), !PZ) :-
    pz_new_struct_id(SId, Name, !PZ),
    pz_add_struct(SId, Struct, !PZ).

:- pred link_datas(id_map::in, pz::in, int::in, int::out,
    pz::in, pz::out) is det.

link_datas(IdMap, Input, InputNum, InputNum+1, !PZ) :-
    Datas = pz_get_data_items(Input),
    foldl(link_datas_2(IdMap, InputNum), Datas, !PZ).

:- pred link_datas_2(id_map::in, int::in, pair(T, pz_data)::in,
    pz::in, pz::out) is det.

link_datas_2(IdMap, InputNum, _ - Data0, !PZ) :-
    Data0 = pz_data(Type0, Values0),
    ( Type0 = type_array(_, _),
        Type = Type0
    ; Type0 = type_struct(OldId),
        NewId = transform_struct_id(!.PZ, IdMap, InputNum, OldId),
        Type = type_struct(NewId)
    ),
    Values = map(transform_value(!.PZ, IdMap, InputNum), Values0),
    Data = pz_data(Type, Values),

    pz_new_data_id(DataId, !PZ),
    pz_add_data(DataId, Data, !PZ).

:- pred link_procs(id_map::in, pz::in, int::in, int::out, pz::in, pz::out)
    is det.

link_procs(IdMap, Input, InputNum, InputNum+1, !PZ) :-
    Procs = pz_get_procs(Input),
    foldl(link_proc(IdMap, InputNum), Procs, !PZ).

:- pred link_proc(id_map::in, int::in, pair(T, pz_proc)::in,
    pz::in, pz::out) is det.

link_proc(IdMap, Input, _ - Proc0, !PZ) :-
    pz_proc(Name, Signature, MaybeBlocks0) = Proc0,
    ( MaybeBlocks0 = no,
        MaybeBlocks = no
    ; MaybeBlocks0 = yes(Blocks0),
        Blocks = map(link_block(!.PZ, IdMap, Input), Blocks0),
        MaybeBlocks = yes(Blocks)
    ),
    Proc = pz_proc(Name, Signature, MaybeBlocks),
    pz_new_proc_id(ProcId, !PZ),
    pz_add_proc(ProcId, Proc, !PZ).

:- func link_block(pz, id_map, int, pz_block) = pz_block.

link_block(PZ, IdMap, Input, pz_block(Instrs)) =
    pz_block(map(link_instr_obj(PZ, IdMap, Input), Instrs)).

:- func link_instr_obj(pz, id_map, int, pz_instr_obj) = pz_instr_obj.

link_instr_obj(PZ, IdMap, Input, pzio_instr(Instr)) =
    pzio_instr(link_instr(PZ, IdMap, Input, Instr)).
link_instr_obj(_,  _,     _,     pzio_context(Context)) =
    pzio_context(Context).
link_instr_obj(_,  _,     _,     pzio_comment(Comment)) =
    pzio_comment(Comment).

:- func link_instr(pz, id_map, int, pz_instr) = pz_instr.

link_instr(PZ, IdMap, Input, Instr0) = Instr :-
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
            Imm = pz_im_import(transform_import_id(PZ, IdMap, Input, ImportId))
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

:- pred link_closures(id_map::in, pz::in, int::in, int::out, pz::in, pz::out)
    is det.

link_closures(IdMap, Input, InputNum, InputNum+1, !PZ) :-
    Closures = pz_get_closures(Input),
    foldl(link_closure(IdMap, InputNum), Closures, !PZ).

:- pred link_closure(id_map::in, int::in, pair(T, pz_closure)::in,
    pz::in, pz::out) is det.

link_closure(IdMap, InputNum, _ - pz_closure(Proc, Data), !PZ) :-
    Closure = pz_closure(
        transform_proc_id(!.PZ, IdMap, InputNum, Proc),
        transform_data_id(!.PZ, IdMap, InputNum, Data)),
    pz_new_closure_id(CID, !PZ),
    pz_add_closure(CID, Closure, !PZ).

%-----------------------------------------------------------------------%

:- func transform_value(pz, id_map, int, pz_data_value) = pz_data_value.

transform_value(_,  _,     _,     pzv_num(Num)) =       pzv_num(Num).
transform_value(PZ, IdMap, Input, pzv_data(OldId)) =    pzv_data(NewId) :-
    NewId = transform_data_id(PZ, IdMap, Input, OldId).
transform_value(PZ, IdMap, Input, pzv_import(OldId)) =  pzv_import(NewId) :-
    NewId = transform_import_id(PZ, IdMap, Input, OldId).
transform_value(PZ, IdMap, Input, pzv_closure(OldId)) = pzv_closure(NewId) :-
    NewId = transform_closure_id(PZ, IdMap, Input, OldId).

%-----------------------------------------------------------------------%

:- type id_map
    --->    id_map(
                idm_import_offsets  :: array(uint32),
                idm_struct_offsets  :: array(uint32),
                idm_data_offsets    :: array(uint32),
                idm_proc_offsets    :: array(uint32),
                idm_closure_offsets :: array(uint32)
            ).

:- pred build_input_maps(list(pz)::in, id_map::out, map(q_name, pz)::out)
    is det.

build_input_maps(Inputs, IdMap, NameMap) :-
    calculate_offsets_and_build_maps(Inputs,
        0u32, [], ImportOffsetsList,
        0u32, [], StructOffsetsList,
        0u32, [], DataOffsetsList,
        0u32, [], ProcOffsetsList,
        0u32, [], ClosureOffsetsList,
        init, NameMap),
    ImportOffsets = array(ImportOffsetsList),
    StructOffsets = array(StructOffsetsList),
    DataOffsets = array(DataOffsetsList),
    ProcOffsets = array(ProcOffsetsList),
    ClosureOffsets = array(ClosureOffsetsList),

    IdMap = id_map(ImportOffsets, StructOffsets, DataOffsets, ProcOffsets,
        ClosureOffsets).

:- pred calculate_offsets_and_build_maps(list(pz)::in,
    uint32::in, list(uint32)::in, list(uint32)::out,
    uint32::in, list(uint32)::in, list(uint32)::out,
    uint32::in, list(uint32)::in, list(uint32)::out,
    uint32::in, list(uint32)::in, list(uint32)::out,
    uint32::in, list(uint32)::in, list(uint32)::out,
    map(q_name, pz)::in, map(q_name, pz)::out) is det.

calculate_offsets_and_build_maps([],
        _, !ImportOffsets,
        _, !StructOffsets,
        _, !DataOffsets,
        _, !ProcOffsets,
        _, !ClosureOffsets,
        !NameMap) :-
    reverse(!ImportOffsets),
    reverse(!StructOffsets),
    reverse(!DataOffsets),
    reverse(!ProcOffsets),
    reverse(!ClosureOffsets).
calculate_offsets_and_build_maps([Input | Inputs],
        PrevImportOffset, !ImportOffsets,
        PrevStructOffset, !StructOffsets,
        PrevDataOffset, !DataOffsets,
        PrevProcOffset, !ProcOffsets,
        PrevClosureOffset, !ClosureOffsets,
        !NameMap) :-
    CurImportOffset = PrevImportOffset + pz_get_num_imports(Input),
    !:ImportOffsets = [CurImportOffset | !.ImportOffsets],

    CurStructOffset = PrevStructOffset + pz_get_num_structs(Input),
    !:StructOffsets = [CurStructOffset | !.StructOffsets],

    CurDataOffset = PrevDataOffset + pz_get_num_datas(Input),
    !:DataOffsets = [CurDataOffset | !.DataOffsets],

    CurProcOffset = PrevProcOffset + pz_get_num_procs(Input),
    !:ProcOffsets = [CurProcOffset | !.ProcOffsets],

    CurClosureOffset = PrevClosureOffset + pz_get_num_closures(Input),
    !:ClosureOffsets = [CurClosureOffset | !.ClosureOffsets],

    det_insert(pz_get_module_name(Input), Input, !NameMap),

    calculate_offsets_and_build_maps(Inputs,
        CurImportOffset, !ImportOffsets,
        CurStructOffset, !StructOffsets,
        CurDataOffset, !DataOffsets,
        CurProcOffset, !ProcOffsets,
        CurClosureOffset, !ClosureOffsets,
        !NameMap).

:- func transform_import_id(pz, id_map, int, pzi_id) = pzi_id.

transform_import_id(PZ, IdMap, InputNum, OldId) =
    transform_id(pzi_id_get_num, pzi_id_from_num(PZ),
        IdMap ^ idm_import_offsets, InputNum, OldId).

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
