%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module pz.write.
%
% Write the PZ bytecode.
%
% Copyright (C) 2015 Paul Bone
% Distributed under the terms of the GPLv2 see ../LICENSE.tools
%
%-----------------------------------------------------------------------%

:- interface.

:- import_module io.
:- import_module maybe.
:- import_module string.

%-----------------------------------------------------------------------%

:- pred write_pz(string::in, pz::in, maybe_error::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module list.
:- import_module pair.

:- import_module pz.bytecode.
:- import_module symtab.

:- import_module io_utils.

%-----------------------------------------------------------------------%

write_pz(Filename, PZ, Result, !IO) :-
    io.open_binary_output(Filename, MaybeFile, !IO),
    ( MaybeFile = ok(File),
        write_int16(File, pzf_magic, !IO),
        write_len_string(File, pzf_id_string, !IO),
        write_int16(File, pzf_version, !IO),
        write_pz_options(File, PZ, !IO),
        write_pz_entries(File, PZ, !IO),
        Result = ok
    ; MaybeFile = error(Error),
        Result =
            error(format("%s: %s", [s(Filename), s(error_message(Error))]))
    ).

%-----------------------------------------------------------------------%

:- import_module require.

:- pred write_pz_options(io.binary_output_stream::in, pz::in,
    io::di, io::uo) is det.

write_pz_options(File, PZ, !IO) :-
    MaybeEntryProc = pz_get_maybe_entry_proc(PZ),
    ( MaybeEntryProc = yes(EntryPID),
        write_int16(File, 1, !IO),
        write_int16(File, pzf_opt_entry_proc, !IO),
        write_int16(File, 4, !IO),
        write_int32(File, pzp_id_get_num(PZ, EntryPID), !IO)
    ; MaybeEntryProc = no,
        write_int16(File, 0, !IO)
    ).

:- pred write_pz_entries(io.binary_output_stream::in, pz::in, io::di, io::uo)
    is det.

write_pz_entries(File, PZ, !IO) :-
    % Currently data items are never imported.
    write_int32(File, 0, !IO),

    ImportedProcs = sort(pz_get_imported_procs(PZ)),
    write_int32(File, length(ImportedProcs), !IO),
    foldl(write_imported_proc(File), ImportedProcs, !IO),

    Datas = sort(pz_get_data_items(PZ)),
    write_int32(File, length(Datas), !IO),
    foldl(write_data(File), Datas, !IO),

    Procs = sort(pz_get_local_procs(PZ)),
    write_int32(File, length(Procs), !IO),
    foldl(write_proc(File, PZ), Procs, !IO).

%-----------------------------------------------------------------------%

:- pred write_imported_proc(io.binary_output_stream::in,
    pair(T, pz_proc)::in, io::di, io::uo) is det.

write_imported_proc(File, _ - Proc, !IO) :-
    symbol_names(Proc ^ pzp_name, MaybeModuleName, ProcName),
    ( MaybeModuleName = yes(ModuleName)
    ; MaybeModuleName = no,
        unexpected($file, $pred, "Unqualified procedure name")
    ),
    write_len_string(File, ModuleName, !IO),
    write_len_string(File, ProcName, !IO).

:- pred write_data(io.binary_output_stream::in, pair(T, pz_data)::in,
    io::di, io::uo) is det.

write_data(File, _ - pz_data(Type, Value), !IO) :-
    write_data_type(File, Type, Value, !IO),
    write_data_value(File, Type, Value, !IO),
    write_data_references(File, Value, !IO).

:- pred write_data_type(io.binary_output_stream::in, pz_data_type::in,
    pz_data_value::in, io::di, io::uo) is det.

write_data_type(File, type_basic(Width), _, !IO) :-
    write_int8(File, pzf_data_basic, !IO),
    write_width(File, Width, !IO).
write_data_type(File, type_array(Width), Value, !IO) :-
    write_int8(File, pzf_data_array, !IO),
    ( Value = pzv_sequence(Nums),
        write_int16(File, length(Nums), !IO)
    ;
        ( Value = pzv_num(_)
        ; Value = pzv_data(_)
        ),
        unexpected($file, $pred, "Expected sequence of data")
    ),
    write_width(File, Width, !IO).
write_data_type(File, type_struct(Widths), _, !IO) :-
    sorry($file, $pred, "Unimplemented"),
    write_int8(File, pzf_data_struct, !IO),
    write_int16(File, length(Widths), !IO),
    foldl(write_width(File), Widths, !IO).

:- pred write_width(io.binary_output_stream::in, pz_data_width::in,
    io::di, io::uo) is det.

write_width(File, Width, !IO) :-
    width_int(Width, Int),
    write_int8(File, Int, !IO).

:- pred width_int(pz_data_width::in, int::out) is det.

width_int(w8,  1).
width_int(w16, 2).
width_int(w32, 4).
width_int(w64, 8).
width_int(ptr, 0).

:- pred write_data_value(io.binary_output_stream::in, pz_data_type::in,
    pz_data_value::in, io::di, io::uo) is det.

write_data_value(File, Type, pzv_num(Num), !IO) :-
    ( Type = type_basic(Width),
        write_value(File, Width, Num, !IO)
    ;
        ( Type = type_array(_)
        ; Type = type_struct(_)
        ),
        unexpected($file, $pred,
            "Type and Value do not match, expected nonscalar value.")
    ).
write_data_value(File, Type, pzv_sequence(Nums), !IO) :-
    ( Type = type_array(Width),
        foldl(write_value(File, Width), Nums, !IO)
    ; Type = type_struct(Widths),
        foldl_corresponding(write_value(File), Widths, Nums, !IO)
    ; Type = type_basic(_),
        unexpected($file, $pred,
            "Type and Value do not match, expected scalar value.")
    ).
write_data_value(_, _, pzv_data(_), !IO).

:- pred write_value(io.binary_output_stream::in, pz_data_width::in, int::in,
    io::di, io::uo) is det.

write_value(File, Width, Value, !IO) :-
    ( Width = w8,
        write_int8(File, Value, !IO)
    ; Width = w16,
        write_int16(File, Value, !IO)
    ; Width = w32,
        write_int32(File, Value, !IO)
    ; Width = w64,
        write_int64(File, Value, !IO)
    ; Width = ptr,
        expect(unify(0, Value), $file, $pred,
            "Pointers must be the null pointer")
    ).

:- pred write_data_references(io.binary_output_stream::in,
    pz_data_value::in, io::di, io::uo) is det.

write_data_references(_, pzv_num(_), !IO).
write_data_references(_, pzv_sequence(_), !IO).
write_data_references(File, pzv_data(DID), !IO) :-
    write_int32(File, DID ^ pzd_id_num, !IO).

%-----------------------------------------------------------------------%

:- pred write_proc(binary_output_stream::in, pz::in, pair(T, pz_proc)::in,
    io::di, io::uo) is det.

write_proc(File, PZ, _ - Proc, !IO) :-
    MaybeInstrs = Proc ^ pzp_instrs,
    ( MaybeInstrs = yes(Instrs),
        write_int32(File, length(Instrs), !IO),
        foldl(write_instr(File, PZ), Instrs, !IO)
    ; MaybeInstrs = no,
        unexpected($file, $pred, "Missing definition")
    ).

:- pred write_instr(binary_output_stream::in, pz::in, pz_instr::in,
    io::di, io::uo) is det.

write_instr(File, PZ, Instr, !IO) :-
    instr_opcode(Instr, Opcode),
    instr_immediate(PZ, Instr, MaybeImmediate),
    write_int8(File, Opcode, !IO),
    ( MaybeImmediate = no_immediate
    ; MaybeImmediate = immediate8(Int),
        write_int8(File, Int, !IO)
    ; MaybeImmediate = immediate16(Int),
        write_int16(File, Int, !IO)
    ; MaybeImmediate = immediate32(Int),
        write_int32(File, Int, !IO)
    ; MaybeImmediate = immediate64(Int),
        write_int64(File, Int, !IO)
    ).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
