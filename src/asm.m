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

:- import_module context.
:- import_module common_types.
:- import_module pz.code.
:- import_module q_name.
:- import_module util.

%-----------------------------------------------------------------------%

assemble(PZT, MaybePZ) :-
    some [!PZ] (
        !:PZ = init_pz,
        Entries = PZT ^ asm_entries,
        foldl3(prepare_map, Entries, init, SymbolMap, init, StructMap, !PZ),
        foldl(build_entries(SymbolMap, StructMap), Entries, !PZ),
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

:- pred prepare_map(asm_entry::in, bimap(q_name, pz_entry_id)::in,
    bimap(q_name, pz_entry_id)::out,
    map(string, pzs_id)::in, map(string, pzs_id)::out,
    pz::in, pz::out) is det.

prepare_map(Entry, !SymMap, !StructMap, !PZ) :-
    Entry = asm_entry(QName, Context, Type),
    (
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

:- pred build_entries(bimap(q_name, pz_entry_id)::in, map(string, pzs_id)::in,
    asm_entry::in, pz::in, pz::out) is det.

build_entries(Map, StructMap, Entry, !PZ) :-
    Entry = asm_entry(Name, _, Type),
    (
        ( Type = asm_proc(_, _)
        ; Type = asm_proc_decl(_)
        ; Type = asm_data(_, _)
        ),
        lookup(Map, Name, ID),
        ( Type = asm_proc(Signature, Blocks0),
            ( ID = pzei_proc(PID),
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
                        !PZ),
                    ( q_name_has_name(Name, "main") ->
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
        )
    ; Type = asm_struct(_)
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

:- pred build_block(bimap(q_name, pz_entry_id)::in, map(string, int)::in,
    map(string, pzs_id)::in, pzt_block::in,
    result(pz_block, asm_error)::out) is det.

build_block(Map, BlockMap, StructMap, pzt_block(_, Instrs0, _), MaybeBlock) :-
    map(build_instruction(Map, BlockMap, StructMap), Instrs0, MaybeInstrs0),
    result_list_to_result(MaybeInstrs0, MaybeInstrs),
    MaybeBlock = result_map((func(X) = pz_block(
        list.map((func(Y) = pzio_instr(Y)), X))), MaybeInstrs).

:- pred build_instruction(bimap(q_name, pz_entry_id)::in,
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

:- pred build_instruction(bimap(q_name, pz_entry_id)::in, map(string, int)::in,
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
                ( Entry = pzei_proc(PID),
                    Instr = pzi_call(PID)
                ; Entry = pzei_data(DID),
                    Instr = pzi_load_immediate(pzw_ptr, immediate_data(DID))
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
            Entry = pzei_proc(PID)
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
%-----------------------------------------------------------------------%
