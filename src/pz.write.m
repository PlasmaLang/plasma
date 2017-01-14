%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module pz.write.
%
% Write the PZ bytecode.
%
% Copyright (C) 2015-2017 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
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
:- import_module require.

:- import_module io_utils.
:- import_module pz.bytecode.
:- import_module q_name.
:- import_module util.

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
    % Write counts of each entry type
    % Currently data items are never imported.
    write_int32(File, 0, !IO),
    ImportedProcs = sort(pz_get_imported_procs(PZ)),
    write_int32(File, length(ImportedProcs), !IO),
    Structs = sort(pz_get_structs(PZ)),
    write_int32(File, length(Structs), !IO),
    Datas = sort(pz_get_data_items(PZ)),
    write_int32(File, length(Datas), !IO),
    Procs = sort(pz_get_local_procs(PZ)),
    write_int32(File, length(Procs), !IO),

    % Write the actual entries.
    foldl(write_imported_proc(File), ImportedProcs, !IO),
    % TODO Write imported data.
    foldl(write_struct(File), Structs, !IO),
    foldl(write_data(File, PZ), Datas, !IO),
    foldl(write_proc(File, PZ), Procs, !IO).

%-----------------------------------------------------------------------%

:- pred write_imported_proc(io.binary_output_stream::in,
    pair(T, pz_proc)::in, io::di, io::uo) is det.

write_imported_proc(File, _ - Proc, !IO) :-
    q_name_parts(Proc ^ pzp_name, Qualifiers, ProcName),
    ModuleName = join_list(".", Qualifiers),
    ( if ModuleName = "" then
        unexpected($file, $pred, "Unqualified procedure name")
    else
        true
    ),
    write_len_string(File, ModuleName, !IO),
    write_len_string(File, ProcName, !IO).

%-----------------------------------------------------------------------%

:- pred write_struct(io.binary_output_stream::in,
    pair(T, pz_struct)::in, io::di, io::uo) is det.

write_struct(File, _ - pz_struct(Widths), !IO) :-
    write_int32(File, length(Widths), !IO),
    foldl(write_width(File), Widths, !IO).

:- pred write_width(io.binary_output_stream::in, pz_width::in,
    io::di, io::uo) is det.

write_width(File, Width, !IO) :-
    pz_width_byte(Width, Int),
    write_int8(File, Int, !IO).

%-----------------------------------------------------------------------%

:- pred write_data(io.binary_output_stream::in, pz::in,
    pair(T, pz_data)::in, io::di, io::uo) is det.

write_data(File, PZ, _ - pz_data(Type, Value), !IO) :-
    write_data_type(File, PZ, Type, Value, !IO),
    write_data_value(File, PZ, Type, Value, !IO).

:- pred write_data_type(io.binary_output_stream::in, pz::in,
    pz_data_type::in, pz_data_value::in, io::di, io::uo) is det.

write_data_type(File, _PZ, type_basic(Width), _, !IO) :-
    write_int8(File, pzf_data_basic, !IO),
    write_width(File, Width, !IO).
write_data_type(File, _PZ, type_array(Width), Value, !IO) :-
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
write_data_type(File, PZ, type_struct(PZSId), _, !IO) :-
    write_int8(File, pzf_data_struct, !IO),
    write_int32(File, pzs_id_get_num(PZ, PZSId), !IO).

:- pred write_data_value(io.binary_output_stream::in, pz::in, pz_data_type::in,
    pz_data_value::in, io::di, io::uo) is det.

write_data_value(File, _PZ, Type, pzv_num(Num), !IO) :-
    ( Type = type_basic(Width),
        write_value(File, Width, Num, !IO)
    ;
        ( Type = type_array(_)
        ; Type = type_struct(_)
        ),
        unexpected($file, $pred,
            "Type and Value do not match, expected nonscalar value.")
    ).
write_data_value(File, PZ, Type, pzv_sequence(Nums), !IO) :-
    ( Type = type_array(Width),
        foldl(write_value(File, Width), Nums, !IO)
    ; Type = type_struct(PZSId),
        pz_lookup_struct(PZ, PZSId) = pz_struct(Widths),
        foldl_corresponding(write_value(File), Widths, Nums, !IO)
    ; Type = type_basic(_),
        unexpected($file, $pred,
            "Type and Value do not match, expected scalar value.")
    ).
write_data_value(File, _, _, pzv_data(DID), !IO) :-
    write_int32(File, DID ^ pzd_id_num, !IO).

:- pred write_value(io.binary_output_stream::in, pz_width::in, int::in,
    io::di, io::uo) is det.

write_value(File, Width, Value, !IO) :-
    ( Width = pzw_8,
        pz_enc_byte(t_normal, 1, EncByte),
        write_int8(File, EncByte, !IO),
        write_int8(File, Value, !IO)
    ; Width = pzw_16,
        pz_enc_byte(t_normal, 2, EncByte),
        write_int8(File, EncByte, !IO),
        write_int16(File, Value, !IO)
    ; Width = pzw_32,
        pz_enc_byte(t_normal, 4, EncByte),
        write_int8(File, EncByte, !IO),
        write_int32(File, Value, !IO)
    ; Width = pzw_64,
        util.sorry($file, $pred, "64bit values")
    ;
        ( Width = pzw_ptr,
            % TODO: This code needs refactoring before we can enclude t_ptr
            % values.
            Type = t_wptr
        ; Width = pzw_fast,
            Type = t_wfast
        ),
        pz_enc_byte(Type, 4, EncByte),
        write_int8(File, EncByte, !IO),
        write_int32(File, Value, !IO)
    ).

%-----------------------------------------------------------------------%

:- pred write_proc(binary_output_stream::in, pz::in, pair(T, pz_proc)::in,
    io::di, io::uo) is det.

write_proc(File, PZ, _ - Proc, !IO) :-
    MaybeBlocks = Proc ^ pzp_blocks,
    ( MaybeBlocks = yes(Blocks),
        write_int32(File, length(Blocks), !IO),
        foldl(write_block(File, PZ), Blocks, !IO)
    ; MaybeBlocks = no,
        unexpected($file, $pred, "Missing definition")
    ).

:- pred write_block(binary_output_stream::in, pz::in, pz_block::in,
    io::di, io::uo) is det.

write_block(File, PZ, pz_block(InstrObjs), !IO) :-
    filter_map((pred(pzio_instr(I)::in, I::out) is semidet),
        InstrObjs, Instrs),
    write_int32(File, length(Instrs), !IO),
    foldl(write_instr(File, PZ), Instrs, !IO).

:- pred write_instr(binary_output_stream::in, pz::in, pz_instr::in,
    io::di, io::uo) is det.

write_instr(File, PZ, Instr, !IO) :-
    instr_opcode(Instr, Opcode),
    opcode_byte(Opcode, OpcodeByte),
    write_int8(File, OpcodeByte, !IO),
    instr_operand_width(Instr, Widths),
    ( Widths = no_width
    ; Widths = one_width(Width),
        pz_width_byte(Width, WidthByte),
        write_int8(File, WidthByte, !IO)
    ; Widths = two_widths(WidthA, WidthB),
        pz_width_byte(WidthA, WidthByteA),
        write_int8(File, WidthByteA, !IO),
        pz_width_byte(WidthB, WidthByteB),
        write_int8(File, WidthByteB, !IO)
    ),
    ( if pz_instr_immediate(Instr, Immediate1) then
        write_immediate(File, PZ, Immediate1, !IO)
    else
        true
    ).

:- pred write_immediate(binary_output_stream::in, pz::in,
    pz_immediate_value::in, io::di, io::uo) is det.

write_immediate(File, PZ, Immediate, !IO) :-
    ( Immediate = pz_immediate8(Int),
        write_int8(File, Int, !IO)
    ; Immediate = pz_immediate16(Int),
        write_int16(File, Int, !IO)
    ;
        ( Immediate = pz_immediate32(Int)
        ; Immediate = pz_immediate_label(Int)
        ),
        write_int32(File, Int, !IO)
    ; Immediate = pz_immediate64(IntHigh, IntLow),
        write_int64(File, IntHigh, IntLow, !IO)
    ; Immediate = pz_immediate_data(DID),
        write_int32(File, pzd_id_get_num(PZ, DID), !IO)
    ; Immediate = pz_immediate_code(PID),
        write_int32(File, pzp_id_get_num(PZ, PID), !IO)
    ; Immediate = pz_immediate_struct(SID),
        write_int32(File, pzs_id_get_num(PZ, SID), !IO)
    ; Immediate = pz_immediate_struct_field(SID, Field),
        write_int32(File, pzs_id_get_num(PZ, SID), !IO),
        % Subtract 1 for the zero-based encoding format.
        write_int8(File, Field - 1, !IO)
    ).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
