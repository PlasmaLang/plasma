%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module asm.
%
% Assemble a PZ bytecode file.
%
% Copyright (C) 2015-2018 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
%-----------------------------------------------------------------------%

:- interface.

:- import_module asm_ast.
:- import_module asm_error.
:- import_module pz.
:- import_module result.

%-----------------------------------------------------------------------%

:- pred assemble(asm::in, result(pz, asm_error)::out) is det.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

:- import_module bimap.
:- import_module cord.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module require.
:- import_module string.

:- import_module context.
:- import_module common_types.
:- import_module pz.code.
:- import_module q_name.
:- import_module util.

%-----------------------------------------------------------------------%

assemble(PZT, MaybePZ) :-
    some [!PZ] (
        !:PZ = init_pz,
        Items = PZT ^ asm_items,
        foldl3(prepare_map, Items, init, SymbolMap, init, StructMap, !PZ),
        foldl(build_items(SymbolMap, StructMap), Items, !PZ),
        Errors = pz_get_errors(!.PZ),
        ( is_empty(Errors) ->
            MaybePZ = ok(!.PZ)
        ;
            MaybePZ = errors(Errors)
        )
    ).

:- type pz_item_id
    --->    pzii_proc(pzp_id)
    ;       pzii_imported_proc(pzi_id)
    ;       pzii_data(pzd_id)
    ;       pzii_closure(pzc_id).

:- pred prepare_map(asm_item::in, bimap(q_name, pz_item_id)::in,
    bimap(q_name, pz_item_id)::out,
    map(string, pzs_id)::in, map(string, pzs_id)::out,
    pz::in, pz::out) is det.

prepare_map(asm_item(QName, Context, Type), !SymMap, !StructMap, !PZ) :-
    (
        ( Type = asm_proc(_, _),
            pz_new_proc_id(PID, !PZ),
            ID = pzii_proc(PID)
        ; Type = asm_import(_),
            pz_new_import(IID, QName, !PZ),
            ID = pzii_imported_proc(IID)
        ; Type = asm_data(_, _),
            pz_new_data_id(DID, !PZ),
            ID = pzii_data(DID)
        ; Type = asm_closure(_, _),
            pz_new_closure_id(CID, !PZ),
            ID = pzii_closure(CID)
        ),
        ( if insert(QName, ID, !SymMap) then
            true
        else
            compile_error($file, $pred, Context, "Duplicate name")
        )
    ; Type = asm_struct(Fields),
        pz_new_struct_id(SID, !PZ),
        pz_add_struct(SID, pz_struct(Fields), !PZ),
        ( if q_name_parts(QName, [], Name) then
            ( if insert(Name, SID, !StructMap) then
                true
            else
                compile_error($file, $pred, Context, "Duplicate struct name")
            )
        else
            compile_error($file, $pred, Context, "Qualified struct name")
        )
    ).
prepare_map(asm_entrypoint(_, _), !SymMap, !StructMap, !PZ).

:- pred build_items(bimap(q_name, pz_item_id)::in, map(string, pzs_id)::in,
    asm_item::in, pz::in, pz::out) is det.

build_items(Map, StructMap, asm_item(Name, _, Type), !PZ) :-
    (
        ( Type = asm_proc(_, _)
        ; Type = asm_import(_)
        ; Type = asm_data(_, _)
        ; Type = asm_closure(_, _)
        ),
        lookup(Map, Name, ID),
        ( Type = asm_proc(Signature, Blocks0),
            ( ID = pzii_proc(PID),
                list.foldl3(build_block_map, Blocks0, 0, _, init, BlockMap,
                    init, BlockErrors),
                ( is_empty(BlockErrors) ->
                    map(build_block(Map, BlockMap, StructMap),
                        Blocks0, MaybeBlocks0),
                    result_list_to_result(MaybeBlocks0, MaybeBlocks)
                ;
                    MaybeBlocks = errors(BlockErrors)
                ),
                ( MaybeBlocks = ok(Blocks),
                    pz_add_proc(PID, pz_proc(Name, Signature, yes(Blocks)),
                        !PZ)
                ; MaybeBlocks = errors(Errors),
                    pz_add_errors(Errors, !PZ)
                )
            ;
                ( ID = pzii_data(_)
                ; ID = pzii_closure(_)
                ; ID = pzii_imported_proc(_)
                ),
                unexpected($file, $pred, "Not a procedure")
            )
        ; Type = asm_import(Signature),
            ( ID = pzii_proc(PID),
                pz_add_proc(PID, pz_proc(Name, Signature, no), !PZ)
            ; ID = pzii_imported_proc(_)
            ;
                ( ID = pzii_data(_)
                ; ID = pzii_closure(_)
                ),
                unexpected($file, $pred, "Not a procedure")
            )
        ; Type = asm_data(ASMDType, ASMValues),
            (
                ( ID = pzii_proc(_)
                ; ID = pzii_imported_proc(_)
                ; ID = pzii_closure(_)
                ),
                unexpected($file, $pred, "Not a data value")
            ; ID = pzii_data(DID),
                DType = build_data_type(StructMap, ASMDType),
                Values = map(build_data_value(Map), ASMValues),
                pz_add_data(DID, pz_data(DType, Values), !PZ)
            )
        ; Type = asm_closure(ProcName, DataName),
            (
                ( ID = pzii_proc(_)
                ; ID = pzii_imported_proc(_)
                ; ID = pzii_data(_)
                ),
                unexpected($file, $pred, "Not a closure")
            ; ID = pzii_closure(CID),
                Closure = build_closure(Map, ProcName, DataName),
                pz_add_closure(CID, Closure, !PZ)
            )
        )
    ; Type = asm_struct(_)
    ).
build_items(Map, _StructMap, asm_entrypoint(_, Name), !PZ) :-
    lookup(Map, Name, ID),
    (
        ( ID = pzii_proc(_)
        ; ID = pzii_imported_proc(_)
        ; ID = pzii_data(_)
        ),
        unexpected($file, $pred, "Not a closure")
    ; ID = pzii_closure(CID),
        pz_set_entry_closure(CID, !PZ)
    ).

:- pred build_block_map(pzt_block::in, int::in, int::out,
    map(string, int)::in, map(string, int)::out,
    errors(asm_error)::in, errors(asm_error)::out) is det.

build_block_map(pzt_block(Name, _, Context), !Num, !Map, !Errors) :-
    ( map.insert(Name, !.Num, !Map) ->
        true
    ;
        add_error(Context, e_name_already_defined(Name), !Errors)
    ),
    !:Num = !.Num + 1.

:- pred build_block(bimap(q_name, pz_item_id)::in, map(string, int)::in,
    map(string, pzs_id)::in, pzt_block::in,
    result(pz_block, asm_error)::out) is det.

build_block(Map, BlockMap, StructMap, pzt_block(_, Instrs0, _), MaybeBlock) :-
    map(build_instruction(Map, BlockMap, StructMap), Instrs0, MaybeInstrs0),
    result_list_to_result(MaybeInstrs0, MaybeInstrs),
    MaybeBlock = result_map((func(X) = pz_block(
        list.map((func(Y) = pzio_instr(Y)), X))), MaybeInstrs).

:- pred build_instruction(bimap(q_name, pz_item_id)::in,
    map(string, int)::in, map(string, pzs_id)::in, pzt_instruction::in,
    result(pz_instr, asm_error)::out) is det.

build_instruction(Map, BlockMap, StructMap,
        pzt_instruction(Instr, Widths0, Context), MaybeInstr) :-
    default_widths(Widths0, Width1, Width2),
    build_instruction(Map, BlockMap, StructMap, Context, Instr,
        Width1, Width2, MaybeInstr).

:- pred default_widths(pzt_instruction_widths::in, pz_width::out,
    pz_width::out) is det.

default_widths(no, pzw_fast, pzw_fast).
default_widths(one_width(Width), Width, pzw_fast).
default_widths(two_widths(Width1, Width2), Width1, Width2).

:- pred build_instruction(bimap(q_name, pz_item_id)::in, map(string, int)::in,
    map(string, pzs_id)::in, context::in, pzt_instruction_code::in,
    pz_width::in, pz_width::in, result(pz_instr, asm_error)::out) is det.

build_instruction(Map, BlockMap, StructMap, Context, PInstr, Width1, Width2,
        MaybeInstr) :-
    ( PInstr = pzti_load_immediate(N),
        % TODO: Encode the immediate value with a more suitable width.
        MaybeInstr = ok(pzi_load_immediate(Width1, immediate32(N)))
    ; PInstr = pzti_word(QName),
        ( if
            q_name_parts(QName, [], Name),
            builtin_instr(Name, Width1, Width2, Instr)
        then
            MaybeInstr = ok(Instr)
        else
            ( if search(Map, QName, Entry) then
                ( Entry = pzii_proc(PID),
                    Instr = pzi_call(pzp(PID))
                ; Entry = pzii_imported_proc(IID),
                    Instr = pzi_call(pzi(IID))
                ; Entry = pzii_data(_),
                    util.sorry($file, $pred, "Feature removed, load data")
                ; Entry = pzii_closure(_),
                    util.sorry($file, $pred, "Load closure")
                ),
                MaybeInstr = ok(Instr)
            else
                MaybeInstr = return_error(Context, e_symbol_not_found(QName))
            )
        )
    ; PInstr = pzti_jmp(Name),
        ( search(BlockMap, Name, Num) ->
            MaybeInstr = ok(pzi_jmp(Num))
        ;
            MaybeInstr = return_error(Context, e_block_not_found(Name))
        )
    ; PInstr = pzti_cjmp(Name),
        ( search(BlockMap, Name, Num) ->
            MaybeInstr = ok(pzi_cjmp(Num, Width1))
        ;
            MaybeInstr = return_error(Context, e_block_not_found(Name))
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
        ( if search(StructMap, Name, StructId) then
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
    ; PInstr = pzti_make_closure(QName),
        ( if
            search(Map, QName, Entry),
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
builtin_instr("get_env",    _,  _,  pzi_get_env).

%-----------------------------------------------------------------------%

:- func build_data_type(map(string, pzs_id), asm_data_type) =
    pz_data_type.

build_data_type(_,   asm_dtype_array(Width)) = type_array(Width).
build_data_type(Map, asm_dtype_struct(Name)) = type_struct(ID) :-
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
        (
            ( ID = pzii_proc(_)
            ; ID = pzii_imported_proc(_)
            ),
            util.sorry($file, $pred, "Can't store proc references in data yet")
        ; ID = pzii_data(DID),
            Value = pzv_data(DID)
        ; ID = pzii_closure(_),
            util.sorry($file, $pred,
                "Can't store closure references in data yet")
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
%-----------------------------------------------------------------------%
