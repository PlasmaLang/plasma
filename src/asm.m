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

:- import_module list.
:- import_module map.
:- import_module require.

:- import_module pz.code.

%-----------------------------------------------------------------------%

assemble(PZT, MaybePZ) :-
    some [!PZ] (
        !:PZ = init_pz,
        Entries = PZT ^ asm_entries,
        foldl2(prepare_map, Entries, init, SymbolMap, !PZ),
        foldl(build_entries(SymbolMap), Entries, !PZ),
        MaybePZ = ok(!.PZ)
    ).

:- type pz_entry_id
    --->    pzei_proc(pzp_id)
    ;       pzei_data(pzd_id).

:- pred prepare_map(asm_entry::in, map(string, pz_entry_id)::in,
    map(string, pz_entry_id)::out, pz::in, pz::out) is det.

prepare_map(Entry, !Map, !PZ) :-
    Entry = asm_entry(Name, _, Type),
    ( Type = asm_proc(_, _),
        pz_new_proc_id(PID, !PZ),
        ID = pzei_proc(PID)
    ; Type = asm_data(_, _),
        pz_new_data_id(DID, !PZ),
        ID = pzei_data(DID)
    ),
    det_insert(Name, ID, !Map).

:- pred build_entries(map(string, pz_entry_id)::in, asm_entry::in,
    pz::in, pz::out) is det.

build_entries(Map, Entry, !PZ) :-
    Entry = asm_entry(Name, _, Type),
    ( Type = asm_proc(Signature, Instrs0),
        lookup(Map, Name, ID),
        ( ID = pzei_proc(PID),
            Signature = pzt_signature(Before, After),
            map(build_instruction(Map), Instrs0, Instrs),
            pz_add_proc(PID, pz_proc(Before, After, Instrs), !PZ)
        ; ID = pzei_data(_),
            unexpected($file, $pred, "Not a procedure")
        )
    ; Type = asm_data(_, _)
    ).

:- pred build_instruction(map(string, pz_entry_id)::in,
    pzt_instruction::in, pz_instr::out) is det.

build_instruction(_,   pzti_load_immediate(N), pzi_load_immediate_ptr(N)).
build_instruction(Map, pzti_word(Name), Instr) :-
    ( Name = [NamePart] ->
        lookup(Map, NamePart, Entry),
        ( Entry = pzei_proc(PID),
            Instr = pzi_call(PID)
        ; Entry = pzei_data(DID),
            Instr = pzi_load_data(DID)
        )
    ;
        unexpected($file, $pred, "foreign name")
    ).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
