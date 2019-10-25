%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module pz.write.
%
% Write the PZ bytecode.
%
% Copyright (C) 2015-2019 Plasma Team
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

:- import_module context.
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
    MaybeEntryClosure = pz_get_maybe_entry_closure(PZ),
    ( MaybeEntryClosure = yes(EntryCID),
        write_int16(File, 1, !IO),
        write_int16(File, pzf_opt_entry_closure, !IO),
        write_int16(File, 4, !IO),
        write_int32(File, pzc_id_get_num(EntryCID), !IO)
    ; MaybeEntryClosure = no,
        write_int16(File, 0, !IO)
    ).

:- pred write_pz_entries(io.binary_output_stream::in, pz::in, io::di, io::uo)
    is det.

write_pz_entries(File, PZ, !IO) :-
    % Write counts of each entry type
    ImportedProcs = sort(pz_get_imports(PZ)),
    write_int32(File, length(ImportedProcs), !IO),
    Structs = sort(pz_get_structs(PZ)),
    write_int32(File, length(Structs), !IO),
    Datas = sort(pz_get_data_items(PZ)),
    write_int32(File, length(Datas), !IO),
    Procs = sort(pz_get_procs(PZ)),
    write_int32(File, length(Procs), !IO),
    Closures = sort(pz_get_closures(PZ)),
    write_int32(File, length(Closures), !IO),

    % Write the actual entries.
    foldl(write_imported_proc(File), ImportedProcs, !IO),
    % TODO Write imported data.
    foldl(write_struct(File), Structs, !IO),
    foldl(write_data(File, PZ), Datas, !IO),
    foldl(write_proc(File), Procs, !IO),
    foldl(write_closure(File), Closures, !IO).

%-----------------------------------------------------------------------%

:- pred write_imported_proc(io.binary_output_stream::in,
    pair(T, q_name)::in, io::di, io::uo) is det.

write_imported_proc(File, _ - QName, !IO) :-
    q_name_parts(QName, Qualifiers, ProcName),
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

write_data(File, PZ, _ - pz_data(Type, Values), !IO) :-
    write_data_type(File, Type, length(Values), !IO),
    write_data_values(File, PZ, Type, Values, !IO).

:- pred write_data_type(io.binary_output_stream::in,
    pz_data_type::in, int::in, io::di, io::uo) is det.

write_data_type(File, type_array(Width), Length, !IO) :-
    write_int8(File, pzf_data_array, !IO),
    write_int16(File, Length, !IO),
    write_width(File, Width, !IO).
write_data_type(File, type_struct(PZSId), _, !IO) :-
    write_int8(File, pzf_data_struct, !IO),
    write_int32(File, pzs_id_get_num(PZSId), !IO).

:- pred write_data_values(io.binary_output_stream::in, pz::in, pz_data_type::in,
    list(pz_data_value)::in, io::di, io::uo) is det.

write_data_values(File, PZ, Type, Values, !IO) :-
    ( Type = type_array(Width),
        foldl(write_value(File, Width), Values, !IO)
    ; Type = type_struct(PZSId),
        pz_lookup_struct(PZ, PZSId) = pz_struct(Widths),
        foldl_corresponding(write_value(File), Widths, Values, !IO)
    ).

:- pred write_value(io.binary_output_stream::in, pz_width::in,
    pz_data_value::in, io::di, io::uo) is det.

write_value(File, Width, Value, !IO) :-
    ( Value = pzv_num(Num),
        ( Width = pzw_8,
            pz_enc_byte(t_normal, 1, EncByte),
            write_int8(File, EncByte, !IO),
            write_int8(File, Num, !IO)
        ; Width = pzw_16,
            pz_enc_byte(t_normal, 2, EncByte),
            write_int8(File, EncByte, !IO),
            write_int16(File, Num, !IO)
        ; Width = pzw_32,
            pz_enc_byte(t_normal, 4, EncByte),
            write_int8(File, EncByte, !IO),
            write_int32(File, Num, !IO)
        ; Width = pzw_64,
            util.sorry($file, $pred, "64bit values")
        ; Width = pzw_fast,
            pz_enc_byte(t_wfast, 4, EncByte),
            write_int8(File, EncByte, !IO),
            write_int32(File, Num, !IO)
        ; Width = pzw_ptr,
            % This could be used by tag values in the future, currently I
            % think 32bit values are used.
            unexpected($file, $pred, "Unused")
        )
    ;
        ( Value = pzv_data(DID),
            IdNum = pzd_id_get_num(DID),
            Enc = t_data
        ; Value = pzv_import(IID),
            IdNum = pzi_id_get_num(IID),
            Enc = t_import
        ; Value = pzv_closure(CID),
            IdNum = pzc_id_get_num(CID),
            Enc = t_closure
        ),
        ( Width = pzw_ptr,
            pz_enc_byte(Enc, 4, EncByte),
            write_int8(File, EncByte, !IO),
            write_int32(File, IdNum, !IO)
        ;
            ( Width = pzw_8
            ; Width = pzw_16
            ; Width = pzw_32
            ; Width = pzw_64
            ; Width = pzw_fast
            ),
            unexpected($file, $pred,
                "Data with a non-pointer width encoding")
        )
    ).

%-----------------------------------------------------------------------%

:- pred write_proc(binary_output_stream::in, pair(T, pz_proc)::in,
    io::di, io::uo) is det.

write_proc(File, _ - Proc, !IO) :-
    write_len_string(File, q_name_to_string(Proc ^ pzp_name), !IO),
    MaybeBlocks = Proc ^ pzp_blocks,
    ( MaybeBlocks = yes(Blocks),
        write_int32(File, length(Blocks), !IO),
        foldl(write_block(File), Blocks, !IO)
    ; MaybeBlocks = no,
        unexpected($file, $pred, "Missing definition")
    ).

:- pred write_block(binary_output_stream::in, pz_block::in,
    io::di, io::uo) is det.

write_block(File, pz_block(Instr0), !IO) :-
    % Filter out the comments but leave everything else.
    filter_instrs(Instr0, pz_nil_context, [], Instrs),
    write_int32(File, length(Instrs), !IO),
    foldl(write_instr(File), Instrs, !IO).

:- pred filter_instrs(list(pz_instr_obj)::in, pz_context::in,
    list(pz_instr_obj)::in, list(pz_instr_obj)::out) is det.

filter_instrs([], _, !Instrs) :-
    reverse(!Instrs).
filter_instrs([I | Is0], PrevContext, !Instrs) :-
    ( I = pzio_instr(_),
        !:Instrs = [I | !.Instrs],
        NextContext = PrevContext
    ; I = pzio_comment(_),
        NextContext = PrevContext
    ; I = pzio_context(Context),
        ( if Context \= PrevContext then
            !:Instrs = [I | !.Instrs]
        else
            true
        ),
        NextContext = Context
    ),
    filter_instrs(Is0, NextContext, !Instrs).

:- pred write_instr(binary_output_stream::in, pz_instr_obj::in,
    io::di, io::uo) is det.

write_instr(File, pzio_instr(Instr), !IO) :-
    code_entry_byte(code_instr, CodeInstrByte),
    write_int8(File, CodeInstrByte, !IO),
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
        write_immediate(File, Immediate1, !IO)
    else
        true
    ).
write_instr(File, pzio_context(PZContext), !IO) :-
    ( PZContext = pz_context(Context, DataId),
        code_entry_byte(code_meta_context, CodeMetaByte),
        write_int8(File, CodeMetaByte, !IO),
        write_int32(File, pzd_id_get_num(DataId), !IO),
        write_int32(File, Context ^ c_line, !IO)
    ; PZContext = pz_nil_context,
        code_entry_byte(code_meta_context_nil, CodeMetaByte),
        write_int8(File, CodeMetaByte, !IO)
    ).
write_instr(_, pzio_comment(_), !IO) :-
    unexpected($file, $pred, "pzio_comment").

:- pred write_immediate(binary_output_stream::in,
    pz_immediate_value::in, io::di, io::uo) is det.

write_immediate(File, Immediate, !IO) :-
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
    ; Immediate = pz_immediate_closure(ClosureId),
        write_int32(File, pzc_id_get_num(ClosureId), !IO)
    ; Immediate = pz_immediate_proc(ProcId),
        write_int32(File, pzp_id_get_num(ProcId), !IO)
    ; Immediate = pz_immediate_import(ImportId),
        write_int32(File, pzi_id_get_num(ImportId), !IO)
    ; Immediate = pz_immediate_struct(SID),
        write_int32(File, pzs_id_get_num(SID), !IO)
    ; Immediate = pz_immediate_struct_field(SID, field_num(FieldNumInt)),
        write_int32(File, pzs_id_get_num(SID), !IO),
        % Subtract 1 for the zero-based encoding format.
        write_int8(File, FieldNumInt - 1, !IO)
    ).

%-----------------------------------------------------------------------%

:- pred write_closure(binary_output_stream::in,
    pair(T, pz_closure)::in, io::di, io::uo) is det.

write_closure(File, _ - pz_closure(Proc, Data), !IO) :-
    write_int32(File, pzp_id_get_num(Proc), !IO),
    write_int32(File, pzd_id_get_num(Data), !IO).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
