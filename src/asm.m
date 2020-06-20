%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module asm.
%
% Assemble a PZ bytecode file.
%
% Copyright (C) 2015-2020 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
%-----------------------------------------------------------------------%

:- interface.

:- import_module asm_ast.
:- import_module asm_error.
:- import_module pz.
:- import_module pz.pz_ds.
:- import_module result.

%-----------------------------------------------------------------------%

:- pred assemble(asm::in, result(pz, asm_error)::out) is det.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

:- import_module bimap.
:- import_module cord.
:- import_module digraph.
:- import_module int.
:- import_module int32.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module uint32.

:- import_module constant.
:- import_module context.
:- import_module common_types.
:- import_module pz.code.
:- import_module q_name.
:- import_module util.
:- import_module util.path.

%-----------------------------------------------------------------------%

assemble(PZT, MaybePZ) :-
    some [!PZ, !Errors] (
        filename_extension(constant.pz_text_extension, PZT ^ asm_filename,
            ModuleNameStr),
        ModuleName = q_name([], ModuleNameStr),
        !:PZ = init_pz(ModuleName),
        Items = PZT ^ asm_items,

        % Add a data item to store the source file name.
        pz_new_data_id(CtxtFileDataId, !PZ),
        pz_add_data(CtxtFileDataId, pz_encode_string(PZT ^ asm_filename), !PZ),

        prepare_map(Items, SymbolMap, StructMap, !PZ),
        !:Errors = init,
        foldl2(build_items(SymbolMap, StructMap, CtxtFileDataId), Items,
            !PZ, !Errors),
        ( is_empty(!.Errors) ->
            MaybePZ = ok(!.PZ)
        ;
            MaybePZ = errors(!.Errors)
        )
    ).

:- pred prepare_map(list(asm_item)::in, bimap(q_name, pz_item_id)::out,
    map(string, pzs_id)::out, pz::in, pz::out) is det.

prepare_map(Items, !:SymbolMap, StructMap, !PZ) :-
    some [!Graph] (
        digraph.init(!:Graph),
        filter_map((pred(Item::in, Data::out) is semidet :-
                Item = asm_item(Name, Context, asm_data(Type, Value)),
                Data = asm_item_data(Name, Context, Type, Value)
            ), Items, DataItems),

        DataNames = list_to_set(map(func(Data) = Data ^ aid_name, DataItems)),
        foldl(build_data_graph(DataNames), DataItems, !Graph),
        !:SymbolMap = bimap.init,
        ( if return_vertices_in_to_from_order(!.Graph, NamesOrdered) then
            foldl2((pred(Name::in, S0::in, S::out, PZ0::in, PZ::out) is det :-
                    pz_new_data_id(DID, PZ0, PZ),
                    ID = pzii_data(DID),
                    ( if insert(Name, ID, S0, S1) then
                        S = S1
                    else
                        compile_error($file, $pred, "Duplicate data name")
                    )
                ), NamesOrdered, !SymbolMap, !PZ)
        else
            compile_error($file, $pred, "Data contains cycles")
        ),

        foldl3(prepare_map_2, Items, !SymbolMap, map.init, StructMap,
            !PZ)
    ).

:- type asm_item_data
    --->    asm_item_data(
                aid_name        :: q_name,
                aid_context     :: context,
                aid_type        :: asm_data_type,
                aid_value       :: list(asm_data_value)
            ).

:- pred build_data_graph(set(q_name)::in, asm_item_data::in,
    digraph(q_name)::in, digraph(q_name)::out) is det.

build_data_graph(DataNames, Data, !Graph) :-
    Name = Data ^ aid_name,
    add_vertex(Name, NameKey, !Graph),
    foldl((pred(Item::in, G0::in, G::out) is det :-
            ( Item = asm_dvalue_num(_),
                G = G0
            ; Item = asm_dvalue_name(Ref),
                ( if member(Ref, DataNames) then
                    % Only add this edge if the referred-to thing is itself
                    % data.
                    add_vertex(Ref, RefKey, G0, G1),
                    add_edge(NameKey, RefKey, G1, G)
                else
                    G = G0
                )
            )
        ), Data ^ aid_value, !Graph).

:- pred prepare_map_2(asm_item::in, bimap(q_name, pz_item_id)::in,
    bimap(q_name, pz_item_id)::out,
    map(string, pzs_id)::in, map(string, pzs_id)::out,
    pz::in, pz::out) is det.

prepare_map_2(asm_item(QName, Context, Type), !SymMap, !StructMap, !PZ) :-
    (
        ( Type = asm_proc(_, _),
            pz_new_proc_id(PID, !PZ),
            ID = pzii_proc(PID)
        ; Type = asm_closure(_, _, _),
            pz_new_closure_id(CID, !PZ),
            ID = pzii_closure(CID)
        ; Type = asm_import(_),
            pz_new_import(IID, QName, !PZ),
            ID = pzii_import(IID)
        ),
        ( if insert(QName, ID, !SymMap) then
            true
        else
            compile_error($file, $pred, Context, "Duplicate name")
        )
    ; Type = asm_struct(Fields),
        ( if q_name_parts(QName, [], Name) then
            pz_new_struct_id(SID, Name, !PZ),
            pz_add_struct(SID, pz_struct(Fields), !PZ),
            ( if insert(Name, SID, !StructMap) then
                true
            else
                compile_error($file, $pred, Context, "Duplicate struct name")
            )
        else
            compile_error($file, $pred, Context, "Qualified struct name")
        )
    ; Type = asm_data(_, _)
        % Already handled above.
    ).
prepare_map_2(asm_entrypoint(_, _), !SymMap, !StructMap, !PZ).

:- pred build_items(bimap(q_name, pz_item_id)::in, map(string, pzs_id)::in,
    pzd_id::in, asm_item::in, pz::in, pz::out,
    errors(asm_error)::in, errors(asm_error)::out) is det.

build_items(SymbolMap, StructMap, CtxtStrData, asm_item(Name, Context, Type),
        !PZ, !Errors) :-
    (
        ( Type = asm_proc(_, _)
        ; Type = asm_data(_, _)
        ; Type = asm_closure(_, _, _)
        ),
        bimap.lookup(SymbolMap, Name, ID),
        ( Type = asm_proc(Signature, Blocks0),
            PID = item_expect_proc($file, $pred, ID),
            list.foldl3(build_block_map, Blocks0, 0u32, _, map.init, BlockMap,
                init, BlockErrors),
            Info = asm_info(SymbolMap, BlockMap, StructMap, CtxtStrData),
            ( is_empty(BlockErrors) ->
                map(build_block(Info), Blocks0, MaybeBlocks0),
                result_list_to_result(MaybeBlocks0, MaybeBlocks)
            ;
                MaybeBlocks = errors(BlockErrors)
            ),
            ( MaybeBlocks = ok(Blocks),
                pz_add_proc(PID, pz_proc(Name, Signature, yes(Blocks)),
                    !PZ)
            ; MaybeBlocks = errors(Errors),
                add_errors(Errors, !Errors)
            )
        ; Type = asm_data(ASMDType, ASMValues),
            DID = item_expect_data($file, $pred, ID),
            DType = build_data_type(StructMap, ASMDType, ASMValues),
            ( DType = type_struct(PZSId),
                pz_lookup_struct(!.PZ, PZSId) = pz_struct(Widths),
                ( if length(Widths) = length(ASMValues) `with_type` int then
                    true
                else
                    compile_error($file, $pred, Context,
                        "Data length doesn't match struct length")
                )
            ; DType = type_array(_, _)
            ),
            Values = map(build_data_value(SymbolMap), ASMValues),
            pz_add_data(DID, pz_data(DType, Values), !PZ)
        ; Type = asm_closure(ProcName, DataName, Sharing),
            CID = item_expect_closure($file, $pred, ID),
            Closure = build_closure(SymbolMap, ProcName, DataName),
            pz_add_closure(CID, Closure, !PZ),
            ( Sharing = s_public,
                q_name_parts(Name, ModuleParts, NameStr),
                ModuleName = pz_get_module_name(!.PZ),
                ( if
                    ModuleParts = []
                  ;
                    ModuleName = q_name_from_list(ModuleParts)
                then
                    pz_export_closure(CID, nq_name_det(NameStr), !PZ)
                else
                    util.sorry($file, $pred,
                        "Module can't yet export other modules' symbols")
                )
            ; Sharing = s_private
            )
        )
    ; Type = asm_struct(_)
    ; Type = asm_import(_)
    ).
build_items(Map, _StructMap, _, asm_entrypoint(_, Name), !PZ, !Errors) :-
    lookup(Map, Name, ID),
    CID = item_expect_closure($file, $pred, ID),
    pz_set_entry_closure(CID, !PZ).

:- pred build_block_map(pzt_block::in, pzb_id::in, pzb_id::out,
    map(string, pzb_id)::in, map(string, pzb_id)::out,
    errors(asm_error)::in, errors(asm_error)::out) is det.

build_block_map(pzt_block(Name, _, Context), !Num, !Map, !Errors) :-
    ( map.insert(Name, !.Num, !Map) ->
        true
    ;
        add_error(Context, e_name_already_defined(Name), !Errors)
    ),
    !:Num = !.Num + 1u32.

:- type asm_info
    --->    asm_info(
                ai_symbols          :: bimap(q_name, pz_item_id),
                ai_blocks           :: map(string, pzb_id),
                ai_structs          :: map(string, pzs_id),

                % The string data for the filename part of context
                % information.
                ai_context_string   :: pzd_id
            ).

:- pred build_block(asm_info::in, pzt_block::in,
    result(pz_block, asm_error)::out) is det.

build_block(Info, pzt_block(_, Instrs0, _), MaybeBlock) :-
    map(build_instruction(Info), Instrs0, MaybeInstrs0),
    result_list_to_result(MaybeInstrs0, MaybeInstrs1),
    MaybeInstrs = result_map(condense, MaybeInstrs1),
    MaybeBlock = result_map((func(X) = pz_block(X)), MaybeInstrs).

:- pred build_instruction(asm_info::in, pzt_instruction::in,
    result(list(pz_instr_obj), asm_error)::out) is det.

build_instruction(Info, pzt_instruction(Instr, Widths0, Context),
        MaybeInstrs) :-
    default_widths(Widths0, Width1, Width2),
    build_instruction(Info, Context, Instr, Width1, Width2, MaybeInstr),
    ( if is_nil_context(Context) then
        PZContext = pz_nil_context
    else
        PZContext = pz_context(Context, Info ^ ai_context_string)
    ),
    MaybeInstrs = result_map(
        func(X) = [pzio_context(PZContext), pzio_instr(X)],
            MaybeInstr).

:- pred default_widths(pzt_instruction_widths::in, pz_width::out,
    pz_width::out) is det.

default_widths(no, pzw_fast, pzw_fast).
default_widths(one_width(Width), Width, pzw_fast).
default_widths(two_widths(Width1, Width2), Width1, Width2).

:- pred build_instruction(asm_info::in, context::in, pzt_instruction_code::in,
    pz_width::in, pz_width::in, result(pz_instr, asm_error)::out) is det.

build_instruction(Info, Context, PInstr,
        Width1, Width2, MaybeInstr) :-
    ( PInstr = pzti_load_immediate(N),
        % TODO: Encode the immediate value with a more suitable width.
        MaybeInstr = ok(pzi_load_immediate(Width1, im_i32(det_from_int(N))))
    ; PInstr = pzti_word(Name),
        ( if
            builtin_instr(Name, Width1, Width2, Instr)
        then
            MaybeInstr = ok(Instr)
        else
            MaybeInstr = return_error(Context, e_no_such_instruction(Name))
        )
    ; PInstr = pzti_jmp(Name),
        ( search(Info ^ ai_blocks, Name, Num) ->
            MaybeInstr = ok(pzi_jmp(Num))
        ;
            MaybeInstr = return_error(Context, e_block_not_found(Name))
        )
    ; PInstr = pzti_cjmp(Name),
        ( search(Info ^ ai_blocks, Name, Num) ->
            MaybeInstr = ok(pzi_cjmp(Num, Width1))
        ;
            MaybeInstr = return_error(Context, e_block_not_found(Name))
        )
    ; PInstr = pzti_call(QName),
        ( if
            search(Info ^ ai_symbols, QName, Entry),
            ( Entry = pzii_closure(CID),
                Callee = pzc_closure(CID)
            ; Entry = pzii_proc(PID),
                Callee = pzc_proc_opt(PID)
            ; Entry = pzii_import(ImportId),
                Callee = pzc_import(ImportId)
            )
        then
            MaybeInstr = ok(pzi_call(Callee))
        else
            MaybeInstr = return_error(Context, e_symbol_not_found(QName))
        )
    ; PInstr = pzti_tcall(QName),
        ( if
            search(Info ^ ai_symbols, QName, Entry),
            ( Entry = pzii_proc(PID),
                Callee = pzc_proc_opt(PID)
            ; Entry = pzii_closure(CID),
                Callee = pzc_closure(CID)
            )
        then
            MaybeInstr = ok(pzi_tcall(Callee))
        else
            MaybeInstr = return_error(Context, e_symbol_not_found(QName))
        )
    ;
        ( PInstr = pzti_roll(Depth)
        ; PInstr = pzti_pick(Depth)
        ),
        ( Depth =< 255 ->
            ( PInstr = pzti_roll(_),
                Instr = pzi_roll(Depth)
            ; PInstr = pzti_pick(_),
                Instr = pzi_pick(Depth)
            ),
            MaybeInstr = ok(Instr)
        ;
            MaybeInstr = return_error(Context, e_stack_depth)
        )
    ;
        ( PInstr = pzti_alloc(Name)
        ; PInstr = pzti_load(Name, _)
        ; PInstr = pzti_store(Name, _)
        ),
        ( if search(Info ^ ai_structs, Name, StructId) then
            ( PInstr = pzti_alloc(_),
                MaybeInstr = ok(pzi_alloc(StructId))
            ; PInstr = pzti_load(_, Field),
                % TODO: Use the width from the structure and don't allow a
                % custom one.
                MaybeInstr = ok(pzi_load(StructId, Field, Width1))
            ; PInstr = pzti_store(_, Field),
                MaybeInstr = ok(pzi_store(StructId, Field, Width1))
            )
        else
            MaybeInstr = return_error(Context, e_struct_not_found(Name))
        )
    ; PInstr = pzti_load_named(QName),
        ( if search(Info ^ ai_symbols, QName, Entry) then
            ImportId = item_expect_import($file, $pred, Entry),
            MaybeInstr = ok(pzi_load_named(ImportId, Width1))
        else
            MaybeInstr = return_error(Context, e_import_not_found(QName))
        )
    ; PInstr = pzti_make_closure(QName),
        ( if
            search(Info ^ ai_symbols, QName, Entry),
            Entry = pzii_proc(PID)
        then
            MaybeInstr = ok(pzi_make_closure(PID))
        else
            MaybeInstr = return_error(Context, e_symbol_not_found(QName))
        )
    ).

    % Identifiers that are builtin instructions.
    %
:- pred builtin_instr(string::in, pz_width::in, pz_width::in,
    pz_instr::out) is semidet.

builtin_instr("ze",         W1, W2, pzi_ze(W1, W2)).
builtin_instr("se",         W1, W2, pzi_se(W1, W2)).
builtin_instr("trunc",      W1, W2, pzi_trunc(W1, W2)).
builtin_instr("add",        W1, _,  pzi_add(W1)).
builtin_instr("sub",        W1, _,  pzi_sub(W1)).
builtin_instr("mul",        W1, _,  pzi_mul(W1)).
builtin_instr("div",        W1, _,  pzi_div(W1)).
builtin_instr("and",        W1, _,  pzi_and(W1)).
builtin_instr("or",         W1, _,  pzi_or(W1)).
builtin_instr("xor",        W1, _,  pzi_xor(W1)).
builtin_instr("dup",        _,  _,  pzi_dup).
builtin_instr("drop",       _,  _,  pzi_drop).
builtin_instr("swap",       _,  _,  pzi_swap).
builtin_instr("lt_u",       W1, _,  pzi_lt_u(W1)).
builtin_instr("lt_s",       W1, _,  pzi_lt_s(W1)).
builtin_instr("gt_u",       W1, _,  pzi_gt_u(W1)).
builtin_instr("gt_s",       W1, _,  pzi_gt_s(W1)).
builtin_instr("eq",         W1, _,  pzi_eq(W1)).
builtin_instr("not",        W1, _,  pzi_not(W1)).
builtin_instr("ret",        _,  _,  pzi_ret).
builtin_instr("call_ind",   _,  _,  pzi_call_ind).
builtin_instr("tcall_ind",  _,  _,  pzi_tcall_ind).
builtin_instr("get_env",    _,  _,  pzi_get_env).

%-----------------------------------------------------------------------%

:- func build_data_type(map(string, pzs_id), asm_data_type, list(T)) =
    pz_data_type.

build_data_type(_,   asm_dtype_array(Width), Values) =
    type_array(Width, length(Values)).
build_data_type(Map, asm_dtype_struct(Name), _) = type_struct(ID) :-
    ( if map.search(Map, Name, IDPrime) then
        ID = IDPrime
    else
        compile_error($file, $pred,
            format("Unknown data type: '%s'", [s(Name)]))
    ).

:- func build_data_value(bimap(q_name, pz_item_id), asm_data_value) =
    pz_data_value.

build_data_value(_, asm_dvalue_num(Num)) = pzv_num(Num).
build_data_value(Map, asm_dvalue_name(Name)) = Value :-
    ( if search(Map, Name, ID) then
        ( ID = pzii_proc(_),
            compile_error($file, $pred,
                "Can't store proc references in data yet")
        ; ID = pzii_data(DID),
            Value = pzv_data(DID)
        ; ID = pzii_closure(CID),
            Value = pzv_closure(CID)
        ; ID = pzii_import(IID),
            Value = pzv_import(IID)
        )
    else
        compile_error($file, $pred,
            format("Unknown data name: '%s'", [s(q_name_to_string(Name))]))
    ).

:- func build_closure(bimap(q_name, pz_item_id), string, string) = pz_closure.

build_closure(Map, ProcName, DataName) = Closure :-
    ( if
        search(Map, q_name(ProcName), ProcEntry),
        ProcEntry = pzii_proc(ProcPrime)
    then
        Proc = ProcPrime
    else
        compile_error($file, $pred,
            format("Unknown procedure name: '%s'", [s(ProcName)]))
    ),
    ( if
        search(Map, q_name(DataName), DataEntry),
        DataEntry = pzii_data(DataPrime)
    then
        Data = DataPrime
    else
        util.compile_error($file, $pred,
            format("Unknown data name: '%s'", [s(DataName)]))
    ),
    Closure = pz_closure(Proc, Data).

%-----------------------------------------------------------------------%

:- type pz_item_id
    --->    pzii_proc(pzp_id)
    ;       pzii_data(pzd_id)
    ;       pzii_closure(pzc_id)
    ;       pzii_import(pzi_id).

:- func item_expect_proc(string, string, pz_item_id) = pzp_id.

item_expect_proc(File, Pred, ID) =
    ( if ID = pzii_proc(Proc) then
        Proc
    else
        unexpected(File, Pred, "Expected proc")
    ).

:- func item_expect_data(string, string, pz_item_id) = pzd_id.

item_expect_data(File, Pred, ID) =
    ( if ID = pzii_data(Data) then
        Data
    else
        unexpected(File, Pred, "Expected data")
    ).

:- func item_expect_closure(string, string, pz_item_id) = pzc_id.

item_expect_closure(File, Pred, ID) =
    ( if ID = pzii_closure(Closure) then
        Closure
    else
        unexpected(File, Pred, "Expected closure")
    ).

:- func item_expect_import(string, string, pz_item_id) = pzi_id.

item_expect_import(File, Pred, ID) =
    ( if ID = pzii_import(Import) then
        Import
    else
        unexpected(File, Pred, "Expected import")
    ).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
