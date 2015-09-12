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
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module require.
:- import_module symtab.

:- import_module context.
:- import_module pz.code.

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
    ( Type = asm_proc(Signature, Instrs0),
        lookup(Map, Name, ID),
        ( ID = pzei_proc(PID),
            map(build_instruction(Map), Instrs0, MaybeInstrs0),
            result_list_to_result(MaybeInstrs0, MaybeInstrs),
            ( MaybeInstrs = ok(Instrs),
                pz_add_proc(PID, pz_proc(Name, Signature, yes(Instrs)), !PZ),
                ( symbol_has_name(Name, "main") ->
                    pz_set_entry_proc(PID, !PZ)
                ;
                    true
                )
            ; MaybeInstrs = errors(Errors),
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

:- pred build_instruction(symtab(pz_entry_id)::in,
    pzt_instruction::in, result(pz_instr, asm_error)::out) is det.

build_instruction(Map, pzt_instruction(Instr, Context), MaybeInstr) :-
    build_instruction(Map, Context, Instr, MaybeInstr).

:- pred build_instruction(symtab(pz_entry_id)::in, context::in,
    pzt_instruction_code::in, result(pz_instr, asm_error)::out) is det.

build_instruction(_, _, pzti_load_immediate(N), ok(pzi_load_immediate_32(N))).
build_instruction(Map, Context, pzti_word(Name), MaybeInstr) :-
    ( search(Map, Name, Entry) ->
        ( Entry = pzei_proc(PID),
            Instr = pzi_call(PID)
        ; Entry = pzei_data(DID),
            Instr = pzi_load_data_ref(DID)
        ),
        MaybeInstr = ok(Instr)
    ;
        MaybeInstr = return_error(Context, e_symbol_not_found(Name))
    ).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
