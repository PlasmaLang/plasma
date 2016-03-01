%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module asm.
%
% Assemble a PZ bytecode file.
%
% Copyright (C) 2015-2016 Plasma Team
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

:- import_module cord.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module require.
:- import_module set.

:- import_module context.
:- import_module common_types.
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
        pz_new_proc_id(i_local, PID, !PZ),
        ID = pzei_proc(PID)
    ; Type = asm_proc_decl(_),
        pz_new_proc_id(i_imported, PID, !PZ),
        ID = pzei_proc(PID)
    ; Type = asm_data(_, _),
        pz_new_data_id(DID, !PZ),
        ID = pzei_data(DID)
    ),
    ( if insert(Name, ID, !Map) then
        true
    else
        unexpected($file, $pred, "Duplicate name")
    ).

:- pred build_entries(symtab(pz_entry_id)::in, asm_entry::in,
    pz::in, pz::out) is det.

build_entries(Map, Entry, !PZ) :-
    Entry = asm_entry(Name, _, Type),
    det_search_exact(Map, Name, ID),
    ( Type = asm_proc(Signature, Blocks0),
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
        ( ID = pzei_proc(PID),
            pz_add_proc(PID, pz_proc(Name, Signature, no), !PZ)
        ; ID = pzei_data(_),
            unexpected($file, $pred, "Not a procedure")
        )
    ; Type = asm_data(DType, Value),
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

build_instruction(Map, BlockMap, Context, PInstr, MaybeInstr) :-
    ( PInstr = pzti_load_immediate(N),
        MaybeInstr = ok(pzi_load_immediate(pzow_fast, immediate32(N)))
    ; PInstr = pzti_word(Symbol),
        ( if
            symbol_parts(Symbol, [], Name),
            builtin_instr(Name, Instr)
        then
            MaybeInstr = ok(Instr)
        else
            search(Map, Symbol, Entries),
            ( if singleton_set(Entry, Entries) then
                ( Entry = pzei_proc(PID),
                    Instr = pzi_call(PID)
                ; Entry = pzei_data(DID),
                    Instr = pzi_load_immediate(pzow_ptr, immediate_data(DID))
                ),
                MaybeInstr = ok(Instr)
            else if is_empty(Entries) then
                MaybeInstr = return_error(Context, e_symbol_not_found(Symbol))
            else
                MaybeInstr = return_error(Context, e_symbol_ambigious(Symbol))
            )
        )
    ; PInstr = pzti_cjmp(Name),
        ( search(BlockMap, Name, Num) ->
            MaybeInstr = ok(pzi_cjmp(Num, pzow_fast))
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
    ).

    % Identifiers that are builtin instructions.
    %
:- pred builtin_instr(string::in, pz_instr::out) is semidet.

builtin_instr("ze",     pzi_ze(pzow_16, pzow_32)).
builtin_instr("se",     pzi_se(pzow_16, pzow_32)).
builtin_instr("trunc",  pzi_trunc(pzow_32, pzow_16)).
builtin_instr("add",    pzi_add(pzow_fast)).
builtin_instr("sub",    pzi_sub(pzow_fast)).
builtin_instr("mul",    pzi_mul(pzow_fast)).
builtin_instr("div",    pzi_div(pzow_fast)).
builtin_instr("and",    pzi_and(pzow_fast)).
builtin_instr("or",     pzi_or(pzow_fast)).
builtin_instr("xor",    pzi_xor(pzow_fast)).
builtin_instr("dup",    pzi_dup).
builtin_instr("drop",   pzi_drop).
builtin_instr("swap",   pzi_swap).
builtin_instr("lt_u",   pzi_lt_u(pzow_fast)).
builtin_instr("lt_s",   pzi_lt_s(pzow_fast)).
builtin_instr("gt_u",   pzi_gt_u(pzow_fast)).
builtin_instr("gt_s",   pzi_gt_s(pzow_fast)).
builtin_instr("ret",    pzi_ret).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
