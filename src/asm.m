%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module asm.
%
% Assemble a PZ bytecode file.
%
% Copyright (C) 2015 Paul Bone
% Distributed under the terms of the GPLv2 see ../LICENSE.tools
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

:- import_module cord.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module require.

:- import_module context.
:- import_module pz.code.
:- import_module symtab.

%-----------------------------------------------------------------------%

assemble(PZT, MaybePZ) :-
    some [!PZ] (
        !:PZ = init_pz,
        Entries = PZT ^ asm_entries,
        foldl2(prepare_map, Entries, init, SymbolMap, !PZ),
        foldl(build_entries(SymbolMap), Entries, !PZ),
        Errors = pz_get_errors(!.PZ),
        ( is_empty(Errors) ->
            MaybePZ = ok(!.PZ)
        ;
            MaybePZ = errors(Errors)
        )
    ).

:- type pz_entry_id
    --->    pzei_proc(pzp_id)
    ;       pzei_data(pzd_id).

:- pred prepare_map(asm_entry::in, symtab(pz_entry_id)::in,
    symtab(pz_entry_id)::out, pz::in, pz::out) is det.

prepare_map(Entry, !Map, !PZ) :-
    Entry = asm_entry(Name, _, Type),
    ( Type = asm_proc(_, _),
        pz_new_proc_id(l_local, PID, !PZ),
        ID = pzei_proc(PID)
    ; Type = asm_proc_decl(_),
        pz_new_proc_id(l_imported, PID, !PZ),
        ID = pzei_proc(PID)
    ; Type = asm_data(_, _),
        pz_new_data_id(DID, !PZ),
        ID = pzei_data(DID)
    ),
    det_insert(Name, ID, !Map).

:- pred build_entries(symtab(pz_entry_id)::in, asm_entry::in,
    pz::in, pz::out) is det.

build_entries(Map, Entry, !PZ) :-
    Entry = asm_entry(Name, _, Type),
    ( Type = asm_proc(Signature, Blocks0),
        lookup(Map, Name, ID),
        ( ID = pzei_proc(PID),
            list.foldl3(build_block_map, Blocks0, 0, _, init, BlockMap,
                init, BlockErrors),
            ( is_empty(BlockErrors) ->
                map(build_block(Map, BlockMap), Blocks0, MaybeBlocks0),
                result_list_to_result(MaybeBlocks0, MaybeBlocks)
            ;
                MaybeBlocks = errors(BlockErrors)
            ),
            ( MaybeBlocks = ok(Blocks),
                pz_add_proc(PID, pz_proc(Name, Signature, yes(Blocks)), !PZ),
                ( symbol_has_name(Name, "main") ->
                    pz_set_entry_proc(PID, !PZ)
                ;
                    true
                )
            ; MaybeBlocks = errors(Errors),
                pz_add_errors(Errors, !PZ)
            )
        ; ID = pzei_data(_),
            unexpected($file, $pred, "Not a procedure")
        )
    ; Type = asm_proc_decl(Signature),
        lookup(Map, Name, ID),
        ( ID = pzei_proc(PID),
            pz_add_proc(PID, pz_proc(Name, Signature, no), !PZ)
        ; ID = pzei_data(_),
            unexpected($file, $pred, "Not a procedure")
        )
    ; Type = asm_data(DType, Value),
        lookup(Map, Name, ID),
        ( ID = pzei_proc(_),
            unexpected($file, $pred, "Not a data value")
        ; ID = pzei_data(DID),
            pz_add_data(DID, pz_data(DType, Value), !PZ)
        )
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

:- pred build_block(symtab(pz_entry_id)::in, map(string, int)::in,
    pzt_block::in, result(pz_block, asm_error)::out) is det.

build_block(Map, BlockMap, pzt_block(_, Instrs0, _), MaybeBlock) :-
    map(build_instruction(Map, BlockMap), Instrs0, MaybeInstrs0),
    result_list_to_result(MaybeInstrs0, MaybeInstrs),
    MaybeBlock = result_map((func(X) = pz_block(X)), MaybeInstrs).

:- pred build_instruction(symtab(pz_entry_id)::in, map(string, int)::in,
    pzt_instruction::in, result(pz_instr, asm_error)::out) is det.

build_instruction(Map, BlockMap, pzt_instruction(Instr, Context),
        MaybeInstr) :-
    build_instruction(Map, BlockMap, Context, Instr, MaybeInstr).

:- pred build_instruction(symtab(pz_entry_id)::in, map(string, int)::in,
    context::in, pzt_instruction_code::in, result(pz_instr, asm_error)::out)
    is det.

build_instruction(_, _, _, pzti_load_immediate(N),
    ok(pzi_load_immediate(pzow_fast, immediate32(N)))).
build_instruction(_, _, _, pzti_add,    ok(pzi_add(pzow_fast))).
build_instruction(_, _, _, pzti_sub,    ok(pzi_sub(pzow_fast))).
build_instruction(_, _, _, pzti_mul,    ok(pzi_mul(pzow_fast))).
build_instruction(_, _, _, pzti_div,    ok(pzi_div(pzow_fast))).
build_instruction(_, _, _, pzti_lt_s,   ok(pzi_lt_s(pzow_fast))).
build_instruction(_, _, _, pzti_lt_u,   ok(pzi_lt_u(pzow_fast))).
build_instruction(_, _, _, pzti_gt_s,   ok(pzi_gt_s(pzow_fast))).
build_instruction(_, _, _, pzti_gt_u,   ok(pzi_gt_u(pzow_fast))).
build_instruction(_, _, _, pzti_dup,    ok(pzi_dup(pzow_fast))).
build_instruction(_, _, _, pzti_drop,   ok(pzi_drop(pzow_fast))).
build_instruction(_, _, _, pzti_swap,   ok(pzi_swap(pzow_fast, pzow_fast))).
build_instruction(_, _, _, pzti_ret,    ok(pzi_ret)).
build_instruction(Map, _, Context, pzti_word(Name), MaybeInstr) :-
    ( search(Map, Name, Entry) ->
        ( Entry = pzei_proc(PID),
            Instr = pzi_call(PID)
        ; Entry = pzei_data(DID),
            Instr = pzi_load_immediate(pzow_ptr, immediate_data(DID))
        ),
        MaybeInstr = ok(Instr)
    ;
        MaybeInstr = return_error(Context, e_symbol_not_found(Name))
    ).
build_instruction(_, BlockMap, Context, pzti_cjmp(Name), MaybeInstr) :-
    ( search(BlockMap, Name, Num) ->
        MaybeInstr = ok(pzi_cjmp(Num, pzow_fast))
    ;
        MaybeInstr = return_error(Context, e_block_not_found(Name))
    ).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
