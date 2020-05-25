%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module pz.write.
%
% Write the PZ bytecode.
%
% Copyright (C) 2015-2020 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
%-----------------------------------------------------------------------%

:- interface.

:- import_module io.
:- import_module maybe.
:- import_module string.

%-----------------------------------------------------------------------%

:- type pz_file_type
    --->    pzft_ball
    ;       pzft_object.

:- pred write_pz(pz_file_type::in, string::in,
    pz::in, maybe_error::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module int16.
:- import_module int32.
:- import_module int64.
:- import_module int8.
:- import_module list.
:- import_module pair.
:- import_module require.
:- import_module uint16.
:- import_module uint32.
:- import_module uint8.

:- import_module common_types.
:- import_module context.
:- import_module io_utils.
:- import_module pz.bytecode.
:- import_module pz.format.
:- import_module q_name.
:- import_module util.

%-----------------------------------------------------------------------%

write_pz(FileType, Filename, PZ, Result, !IO) :-
    io.open_binary_output(Filename, MaybeFile, !IO),
    ( MaybeFile = ok(File),
        ( FileType = pzft_object,
            Magic = pz_object_magic,
            IdString = pz_object_id_string
        ; FileType = pzft_ball,
            Magic = pz_ball_magic,
            IdString = pz_ball_id_string
        ),
        write_binary_uint32_le(File, Magic, !IO),
        write_len_string(File, IdString, !IO),
        write_binary_uint16_le(File, pz_version, !IO),
        write_pz_options(File, PZ, !IO),
        ModuleName = q_name_to_string(pz_get_module_name(PZ)),
        write_len_string(File, ModuleName, !IO),
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
        write_binary_uint16_le(File, 1u16, !IO),
        write_binary_uint16_le(File, pzf_opt_entry_closure, !IO),
        write_binary_uint16_le(File, 4u16, !IO),
        write_binary_uint32_le(File, pzc_id_get_num(EntryCID), !IO)
    ; MaybeEntryClosure = no,
        write_binary_uint16_le(File, 0u16, !IO)
    ).

:- pred write_pz_entries(io.binary_output_stream::in, pz::in, io::di, io::uo)
    is det.

write_pz_entries(File, PZ, !IO) :-
    % Write counts of each entry type
    ImportedProcs = sort(pz_get_imports(PZ)),
    write_binary_uint32_le(File, det_from_int(length(ImportedProcs)), !IO),
    Structs = sort(pz_get_structs(PZ)),
    write_binary_uint32_le(File, det_from_int(length(Structs)), !IO),
    Datas = sort(pz_get_data_items(PZ)),
    write_binary_uint32_le(File, det_from_int(length(Datas)), !IO),
    Procs = sort(pz_get_procs(PZ)),
    write_binary_uint32_le(File, det_from_int(length(Procs)), !IO),
    Closures = sort(pz_get_closures(PZ)),
    write_binary_uint32_le(File, det_from_int(length(Closures)), !IO),

    % Write the actual entries.
    foldl(write_imported_proc(File), ImportedProcs, !IO),
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
    pair(T, pz_named_struct)::in, io::di, io::uo) is det.

write_struct(File, _ - pz_named_struct(_, pz_struct(Widths)), !IO) :-
    write_binary_uint32_le(File, det_from_int(length(Widths)), !IO),
    foldl(write_width(File), Widths, !IO).

:- pred write_width(io.binary_output_stream::in, pz_width::in,
    io::di, io::uo) is det.

write_width(File, Width, !IO) :-
    pz_width_byte(Width, Int),
    write_binary_uint8(File, Int, !IO).

%-----------------------------------------------------------------------%

:- pred write_data(io.binary_output_stream::in, pz::in,
    pair(T, pz_data)::in, io::di, io::uo) is det.

write_data(File, PZ, _ - pz_data(Type, Values), !IO) :-
    write_data_type(File, Type, !IO),
    write_data_values(File, PZ, Type, Values, !IO).

:- pred write_data_type(io.binary_output_stream::in,
    pz_data_type::in, io::di, io::uo) is det.

write_data_type(File, type_array(Width, Length), !IO) :-
    write_binary_uint8(File, pzf_data_array, !IO),
    write_binary_uint16_le(File, det_from_int(Length), !IO),
    write_width(File, Width, !IO).
write_data_type(File, type_struct(PZSId), !IO) :-
    write_binary_uint8(File, pzf_data_struct, !IO),
    write_binary_uint32_le(File, pzs_id_get_num(PZSId), !IO).

:- pred write_data_values(io.binary_output_stream::in, pz::in, pz_data_type::in,
    list(pz_data_value)::in, io::di, io::uo) is det.

write_data_values(File, PZ, Type, Values, !IO) :-
    ( Type = type_array(Width, NumValues),
        ( if length(Values, NumValues) then
            true
        else
            unexpected($file, $pred, "Incorrect length")
        ),
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
            write_binary_uint8(File, EncByte, !IO),
            write_binary_int8(File, det_from_int(Num), !IO)
        ; Width = pzw_16,
            pz_enc_byte(t_normal, 2, EncByte),
            write_binary_uint8(File, EncByte, !IO),
            write_binary_int16_le(File, det_from_int(Num), !IO)
        ; Width = pzw_32,
            pz_enc_byte(t_normal, 4, EncByte),
            write_binary_uint8(File, EncByte, !IO),
            write_binary_int32_le(File, det_from_int(Num), !IO)
        ; Width = pzw_64,
            pz_enc_byte(t_normal, 8, EncByte),
            write_binary_uint8(File, EncByte, !IO),
            write_binary_int64_le(File, from_int(Num), !IO)
        ; Width = pzw_fast,
            pz_enc_byte(t_wfast, 4, EncByte),
            write_binary_uint8(File, EncByte, !IO),
            write_binary_int32_le(File, det_from_int(Num), !IO)
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
            write_binary_uint8(File, EncByte, !IO),
            write_binary_uint32_le(File, IdNum, !IO)
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
        write_binary_uint32_le(File, det_from_int(length(Blocks)), !IO),
        foldl(write_block(File), Blocks, !IO)
    ; MaybeBlocks = no,
        unexpected($file, $pred, "Missing definition")
    ).

:- pred write_block(binary_output_stream::in, pz_block::in,
    io::di, io::uo) is det.

write_block(File, pz_block(Instr0), !IO) :-
    % Filter out the comments but leave everything else.
    filter_instrs(Instr0, pz_nil_context, [], Instrs),
    write_binary_uint32_le(File, det_from_int(length(Instrs)), !IO),
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
        ( if Context = PrevContext then
            true
        else if
            % If the filename is the same then we only need to store the
            % line number.
            Context = pz_context(context(File, Line, _), _),
            PrevContext = pz_context(context(File, _, _), _)
        then
            !:Instrs = [pzio_context(pz_context_short(Line)) | !.Instrs]
        else
            !:Instrs = [I | !.Instrs]
        ),
        NextContext = Context
    ),
    filter_instrs(Is0, NextContext, !Instrs).

:- pred write_instr(binary_output_stream::in, pz_instr_obj::in,
    io::di, io::uo) is det.

write_instr(File, pzio_instr(Instr), !IO) :-
    code_entry_byte(code_instr, CodeInstrByte),
    write_binary_uint8(File, CodeInstrByte, !IO),
    instruction(Instr, Opcode, Widths, MaybeImmediate),
    opcode_byte(Opcode, OpcodeByte),
    write_binary_uint8(File, OpcodeByte, !IO),
    ( Widths = no_width
    ; Widths = one_width(Width),
        write_width(File, Width, !IO)
    ; Widths = two_widths(WidthA, WidthB),
        write_width(File, WidthA, !IO),
        write_width(File, WidthB, !IO)
    ),
    ( MaybeImmediate = yes(Immediate),
        write_immediate(File, Immediate, !IO)
    ; MaybeImmediate = no
    ).
write_instr(File, pzio_context(PZContext), !IO) :-
    ( PZContext = pz_context(Context, DataId),
        code_entry_byte(code_meta_context, CodeMetaByte),
        write_binary_uint8(File, CodeMetaByte, !IO),
        write_binary_uint32_le(File, pzd_id_get_num(DataId), !IO),
        write_binary_uint32_le(File, det_from_int(Context ^ c_line), !IO)
    ; PZContext = pz_context_short(Line),
        code_entry_byte(code_meta_context_short, CodeMetaByte),
        write_binary_uint8(File, CodeMetaByte, !IO),
        write_binary_uint32_le(File, det_from_int(Line), !IO)
    ; PZContext = pz_nil_context,
        code_entry_byte(code_meta_context_nil, CodeMetaByte),
        write_binary_uint8(File, CodeMetaByte, !IO)
    ).
write_instr(_, pzio_comment(_), !IO) :-
    unexpected($file, $pred, "pzio_comment").

:- pred write_immediate(binary_output_stream::in,
    pz_immediate_value::in, io::di, io::uo) is det.

write_immediate(File, Immediate, !IO) :-
    ( Immediate = pz_im_i8(Int),
        write_binary_int8(File, Int, !IO)
    ; Immediate = pz_im_u8(Int),
        write_binary_uint8(File, Int, !IO)
    ; Immediate = pz_im_i16(Int),
        write_binary_int16_le(File, Int, !IO)
    ; Immediate = pz_im_u16(Int),
        write_binary_uint16_le(File, Int, !IO)
    ; Immediate = pz_im_i32(Int),
        write_binary_int32_le(File, Int, !IO)
    ; Immediate = pz_im_u32(Int),
        write_binary_uint32_le(File, Int, !IO)
    ; Immediate = pz_im_i64(Int),
        write_binary_int64_le(File, Int, !IO)
    ; Immediate = pz_im_u64(Int),
        write_binary_uint64_le(File, Int, !IO)
    ; Immediate = pz_im_label(Int),
        write_binary_uint32_le(File, Int, !IO)
    ; Immediate = pz_im_closure(ClosureId),
        write_binary_uint32_le(File, pzc_id_get_num(ClosureId), !IO)
    ; Immediate = pz_im_proc(ProcId),
        write_binary_uint32_le(File, pzp_id_get_num(ProcId), !IO)
    ; Immediate = pz_im_import(ImportId),
        write_binary_uint32_le(File, pzi_id_get_num(ImportId), !IO)
    ; Immediate = pz_im_struct(SID),
        write_binary_uint32_le(File, pzs_id_get_num(SID), !IO)
    ; Immediate = pz_im_struct_field(SID, field_num(FieldNumInt)),
        write_binary_uint32_le(File, pzs_id_get_num(SID), !IO),
        % Subtract 1 for the zero-based encoding format.
        write_binary_uint8(File, det_from_int(FieldNumInt - 1), !IO)
    ; Immediate = pz_im_depth(Int),
        write_binary_uint8(File, det_from_int(Int), !IO)
    ).

%-----------------------------------------------------------------------%

:- pred write_closure(binary_output_stream::in,
    pair(T, pz_closure)::in, io::di, io::uo) is det.

write_closure(File, _ - pz_closure(Proc, Data), !IO) :-
    write_binary_uint32_le(File, pzp_id_get_num(Proc), !IO),
    write_binary_uint32_le(File, pzd_id_get_num(Data), !IO).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
