%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module pz.read.
%
% Read the PZ bytecode.
%
% Copyright (C) 2015, 2020 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
%-----------------------------------------------------------------------%

:- interface.

:- import_module io.
:- import_module maybe.
:- import_module string.

%-----------------------------------------------------------------------%

:- type pz_read_result
    --->    pz_read_result(pz_file_type, pz).

:- type pz_file_type
    --->    pzf_object
    ;       pzf_ball.

:- pred read_pz(string::in, maybe_error(pz_read_result)::out,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

:- import_module require.
:- import_module uint16.
:- import_module uint32.
:- import_module uint8.

:- import_module common_types.
:- import_module constant.
:- import_module context.
:- import_module int.
:- import_module pz.bytecode.
:- import_module pz.code.
:- import_module pz.format.
:- import_module pz.pz_ds.
:- import_module q_name.
:- import_module util.
:- import_module util.io.
:- import_module util.mercury.
:- import_module util.path.

%-----------------------------------------------------------------------%

read_pz(Filename, Result, !IO) :-
    open_binary_input(Filename, MaybeInput, !IO),
    ( MaybeInput = ok(Input),
        filename_extension(constant.output_extension, Filename, _ModuleName),
        read_pz_2(Input, ResultPZ, !IO),
        ( ResultPZ = ok(ReadRes),
            Result = ok(ReadRes)
        ; ResultPZ = error(Error),
            Result = error(format("%s: %s",
                [s(Filename), s(Error)]))
        ),
        close_binary_input(Input, !IO)
    ; MaybeInput = error(Error),
        Result = error(format("%s: %s",
            [s(Filename), s(error_message(Error))]))
    ).

:- pred read_pz_2(binary_input_stream::in, maybe_error(pz_read_result)::out,
    io::di, io::uo) is det.

read_pz_2(Input, Result, !IO) :-
    util.io.read_uint32(Input, MaybeMagic, !IO),
    read_len_string(Input, MaybeObjectIdString, !IO),
    util.io.read_uint16(Input, MaybeVersion, !IO),
    MaybeHeader = combine_read_3(MaybeMagic, MaybeObjectIdString, MaybeVersion),
    ( MaybeHeader = ok({Magic, ObjectIdString, Version}),
        check_file_type(Magic, ObjectIdString, Version, ResultCheck),
        ( ResultCheck = ok(Type),
            read_options(Input, MaybeOptions, !IO),
            read_pz_3(Input, MaybePZ0, !IO),
            MaybePZ1 = combine_read_2(MaybeOptions, MaybePZ0),
            ( MaybePZ1 = ok({Options, PZ1}),
                ( Options = yes(Entry0),
                    ( if pzc_id_from_num(PZ1, Entry0, Entry) then
                        pz_set_entry_closure(Entry, PZ1, PZ),
                        Result = ok(pz_read_result(Type, PZ))
                    else
                        Result = error("Invalid closure ID for entry")
                    )
                ; Options = no,
                    Result = ok(pz_read_result(Type, PZ1))
                )
            ; MaybePZ1 = error(Error),
                Result = error(Error)
            )
        ; ResultCheck = error(Error),
            Result = error(Error)
        )
    ; MaybeHeader = error(Error),
        Result = error(Error)
    ).

:- pred check_file_type(uint32::in, string::in, uint16::in,
    maybe_error(pz_file_type)::out) is det.

check_file_type(Magic, String, Version, Result) :-
    ( if
        % This has only one solution but Mercury can't figure it out.
        promise_equivalent_solutions [Type]
        (
            Magic = pz_object_magic,
            prefix(String, pz_object_id_string_part),
            Type = pzf_object
        ;
            Magic = pz_ball_magic,
            prefix(String, pz_ball_id_string_part),
            Type = pzf_ball
        )
    then
        ( if Version = pz_version then
            Result = ok(Type)
        else
            Result = error(format("Incorrect file verison, need %d got %d",
                [i(to_int(pz_version)), i(to_int(Version))]))
        )
    else
        Result = error("Unrecognised file type")
    ).

:- pred read_options(binary_input_stream::in,
    maybe_error(maybe(uint32))::out, io::di, io::uo) is det.

read_options(Input, Result, !IO) :-
    util.io.read_uint16(Input, MaybeNumOptions, !IO),
    ( MaybeNumOptions = ok(NumOptions),
        % The file format currently only has one possible option, so just
        % read it if it's there.
        ( if NumOptions = 0u16 then
            Result = ok(no)
        else if NumOptions = 1u16 then
            read_option_entry(Input, Result, !IO)
        else
            Result = error("Too many options in the options section")
        )
    ; MaybeNumOptions = error(Error),
        Result = error(Error)
    ).

:- pred read_option_entry(binary_input_stream::in,
    maybe_error(maybe(uint32))::out, io::di, io::uo) is det.

read_option_entry(Input, Result, !IO) :-
    util.io.read_uint16(Input, MaybeType, !IO),
    util.io.read_uint16(Input, MaybeLen, !IO),
    MaybeTypeLen = combine_read_2(MaybeType, MaybeLen),
    ( MaybeTypeLen = ok({Type, Len}),
        ( if
            Type = pzf_opt_entry_closure,
            Len = 4u16
        then
            util.io.read_uint32(Input, MaybeClosure, !IO),
            ( MaybeClosure = ok(Closure),
                Result = ok(yes(Closure))
            ; MaybeClosure = error(Error),
                Result = error(Error)
            )
        else
            Result = error("Currupt option")
        )
    ; MaybeTypeLen = error(Error),
        error(Error)
    ).

:- pred read_pz_3(binary_input_stream::in, maybe_error(pz)::out,
    io::di, io::uo) is det.

read_pz_3(Input, Result, !IO) :-
    util.io.read_len_string(Input, MaybeName, !IO),
    util.io.read_uint32(Input, MaybeNumImports, !IO),
    util.io.read_uint32(Input, MaybeNumStructs, !IO),
    util.io.read_uint32(Input, MaybeNumDatas, !IO),
    util.io.read_uint32(Input, MaybeNumProcs, !IO),
    util.io.read_uint32(Input, MaybeNumClosures, !IO),
    util.io.read_uint32(Input, MaybeNumExports, !IO),
    MaybeNums = combine_read_7(MaybeName, MaybeNumImports, MaybeNumStructs,
        MaybeNumDatas, MaybeNumProcs, MaybeNumClosures, MaybeNumExports),
    (
        MaybeNums = ok({Name, NumImports, NumStructs, NumDatas, NumProcs,
            NumClosures, NumExports}),
        ModuleName = q_name_from_dotted_string(Name),
        PZ = init_pz(ModuleName, NumImports, NumStructs, NumDatas, NumProcs,
                     NumClosures),
        read_pz_sections([read_imports(Input, NumImports),
                          read_structs(Input, NumStructs),
                          read_datas(Input, NumDatas),
                          read_procs(Input, NumProcs),
                          read_closures(Input, NumClosures),
                          read_exports(Input, NumExports)],
            PZ, Result, !IO)
    ;
        MaybeNums = error(Error),
        Result = error(Error)
    ).

:- pred read_pz_sections(
    list(pred(pz, maybe_error(pz), io, io)),
    pz, maybe_error(pz), io, io).
:- mode read_pz_sections(
    in(list(pred(in, out, di, uo) is det)),
    in, out, di, uo) is det.

read_pz_sections([], PZ, ok(PZ), !IO).
read_pz_sections([Section | Sections], PZ0, Result, !IO) :-
    Section(PZ0, Result0, !IO),
    ( Result0 = ok(PZ),
        read_pz_sections(Sections, PZ, Result, !IO)
    ; Result0 = error(Error),
        Result = error(Error)
    ).

%-----------------------------------------------------------------------%

:- pred read_imports(binary_input_stream::in, uint32::in,
    pz::in, maybe_error(pz)::out, io::di, io::uo) is det.

read_imports(Input, Num, PZ0, Result, !IO) :-
    read_items(read_import(Input),
        (pred(N::in, I::in, PZI0::in, PZI::out) is det :-
            ( if pzi_id_from_num(PZI0, N, ImportId) then
                pz_add_import(ImportId, I, PZI0, PZI)
            else
                unexpected($file, $pred, "Bad Import Id")
            )
        ),
        Num, 0u32, PZ0, Result, !IO).

:- pred read_import(binary_input_stream::in, T::in, maybe_error(q_name)::out,
    io::di, io::uo) is det.

read_import(Input, _, Result, !IO) :-
    read_len_string(Input, MaybeModuleName, !IO),
    read_len_string(Input, MaybeSymbolName, !IO),
    MaybeNames = combine_read_2(MaybeModuleName, MaybeSymbolName),
    ( MaybeNames = ok({ModuleName, SymbolName}),
        Result = ok(q_name([ModuleName], SymbolName))
    ; MaybeNames = error(Error),
        Result = error(Error)
    ).

%-----------------------------------------------------------------------%

:- pred read_structs(binary_input_stream::in, uint32::in,
    pz::in, maybe_error(pz)::out, io::di, io::uo) is det.

read_structs(Input, Num, PZ0, Result, !IO) :-
    read_items(read_struct(Input),
        (pred(N::in, I::in, PZI0::in, PZI::out) is det :-
            ( if pzs_id_from_num(PZI0, N, StructId) then
                pz_add_struct(StructId, I, PZI0, PZI)
            else
                unexpected($file, $pred, "Bad Struct Id")
            )
        ),
        Num, 0u32, PZ0, Result, !IO).

:- pred read_struct(binary_input_stream::in, T::in,
    maybe_error(pz_struct)::out, io::di, io::uo) is det.

read_struct(Input, _, Result, !IO) :-
    read_uint32(Input, MaybeNumFields, !IO),
    ( MaybeNumFields = ok(NumFields0),
        NumFields = det_uint32_to_int(NumFields0),
        read_n(read_width(Input), NumFields, MaybeWidths, !IO),
        ( MaybeWidths = ok(Widths),
            Result = ok(pz_struct(Widths))
        ; MaybeWidths = error(Error),
            Result = error(Error)
        )
    ; MaybeNumFields = error(Error),
        Result = error(Error)
    ).

%-----------------------------------------------------------------------%

:- pred read_datas(binary_input_stream::in, uint32::in,
    pz::in, maybe_error(pz)::out, io::di, io::uo) is det.

read_datas(Input, Num, PZ0, Result, !IO) :-
    read_items(read_data(Input),
        (pred(N::in, I::in, PZI0::in, PZI::out) is det :-
            ( if pzd_id_from_num(PZI0, N, DataId) then
                pz_add_data(DataId, I, PZI0, PZI)
            else
                unexpected($file, $pred, "Bad data id")
            )
        ),
        Num, 0u32, PZ0, Result, !IO).

:- pred read_data(binary_input_stream::in, pz::in, maybe_error(pz_data)::out,
    io::di, io::uo) is det.

read_data(Input, PZ, Result, !IO) :-
    read_data_type(PZ, Input, TypeResult, !IO),
    ( TypeResult = ok(Type),
        ( Type = type_array(Width, Num),
            read_n(read_data_value(PZ, Input, Width), Num, ValuesResult, !IO)
        ; Type = type_struct(StructId),
            pz_struct(Widths) = pz_lookup_struct(PZ, StructId),
            read_map(read_data_value(PZ, Input), Widths, ValuesResult, !IO)
        ),
        ( ValuesResult = ok(Values),
            Result = ok(pz_data(Type, Values))
        ; ValuesResult = error(Error),
            Result = error(Error)
        )
    ; TypeResult = error(Error),
        Result = error(Error)
    ).

:- pred read_data_type(pz::in, binary_input_stream::in,
    maybe_error(pz_data_type)::out, io::di, io::uo) is det.

read_data_type(PZ, Input, Result, !IO) :-
    read_uint8(Input, MaybeType, !IO),
    ( MaybeType = ok(Type),
        ( if Type = pzf_data_array then
            read_uint16(Input, MaybeNumItems, !IO),
            read_width(Input, MaybeWidth, !IO),
            Result0 = combine_read_2(MaybeNumItems, MaybeWidth),
            ( Result0 = ok({NumItems, Width}),
                Result = ok(type_array(Width, to_int(NumItems)))
            ; Result0 = error(Error),
                Result = error(Error)
            )
        else if Type = pzf_data_struct then
            read_struct_id(PZ, Input, MaybeStructId, !IO),
            Result = maybe_error_map(func(Id) = type_struct(Id),
                MaybeStructId)
        else
            Result = error("Unknown data type")
        )
    ; MaybeType = error(Error),
        Result = error(Error)
    ).

:- pred read_data_value(pz::in, binary_input_stream::in,
    pz_width::in, maybe_error(pz_data_value)::out, io::di, io::uo) is det.

read_data_value(PZ, Input, Width, Result, !IO) :-
    read_uint8(Input, MaybeEncByte, !IO),
    ( MaybeEncByte = ok(EncByte),
        ( if pz_enc_byte(EncType, NumBytes, EncByte) then
            ( EncType = t_normal,
                ( if NumBytes = 1 then
                    read_uint8(Input, MaybeNum, !IO),
                    Result = maybe_error_map(
                        (func(N) = pzv_num(to_int(N))), MaybeNum)
                else if NumBytes = 2 then
                    read_uint16(Input, MaybeNum, !IO),
                    Result = maybe_error_map(
                        (func(N) = pzv_num(to_int(N))), MaybeNum)
                else if NumBytes = 4 then
                    read_uint32(Input, MaybeNum, !IO),
                    Result = maybe_error_map(
                        (func(N) = pzv_num(det_uint32_to_int(N))), MaybeNum)
                else if NumBytes = 8 then
                    read_uint64(Input, MaybeNum, !IO),
                    Result = maybe_error_map(
                        (func(N) = pzv_num(det_uint64_to_int(N))), MaybeNum)
                else
                    unexpected($file, $pred, "Unknown encoding")
                )
            ; ( EncType = t_wfast
              ; EncType = t_wptr
              ),
                read_uint32(Input, MaybeNum, !IO),
                Result = maybe_error_map(
                    (func(N) = pzv_num(det_uint32_to_int(N))), MaybeNum)
            ; EncType = t_data,
                read_data_id(PZ, Input, MaybeDataId, !IO),
                Result = maybe_error_map(func(Id) = pzv_data(Id),
                    MaybeDataId),
                expect(unify(Width, pzw_ptr), $file, $pred,
                    "These items must be pointer wiedth")
            ; EncType = t_closure,
                read_closure_id(PZ, Input, MaybeClosureId, !IO),
                Result = maybe_error_map(func(Id) = pzv_closure(Id),
                    MaybeClosureId),
                expect(unify(Width, pzw_ptr), $file, $pred,
                    "These items must be pointer wiedth")
            ; EncType = t_import,
                read_import_id(PZ, Input, MaybeImportId, !IO),
                Result = maybe_error_map(func(Id) = pzv_import(Id),
                    MaybeImportId),
                expect(unify(Width, pzw_ptr), $file, $pred,
                    "These items must be pointer wiedth")
            )
        else
            Result = error("Unknown encoding type/byte")
        )
    ; MaybeEncByte = error(Error),
        Result = error(Error)
    ).

%-----------------------------------------------------------------------%

:- pred read_procs(binary_input_stream::in, uint32::in,
    pz::in, maybe_error(pz)::out, io::di, io::uo) is det.

read_procs(Input, Num, PZ0, Result, !IO) :-
    read_items(read_proc(Input),
        (pred(N::in, I::in, PZI0::in, PZI::out) is det :-
            ( if pzp_id_from_num(PZI0, N, ProcId) then
                pz_add_proc(ProcId, I, PZI0, PZI)
            else
                unexpected($file, $pred, "Bad Proc Id")
            )
        ),
        Num, 0u32, PZ0, Result, !IO).

:- pred read_proc(binary_input_stream::in, pz::in, maybe_error(pz_proc)::out,
    io::di, io::uo) is det.

read_proc(Input, PZ, Result, !IO) :-
    read_len_string(Input, MaybeName, !IO),
    read_uint32(Input, MaybeNumBlocks, !IO),
    HeadResult = combine_read_2(MaybeName, MaybeNumBlocks),
    ( HeadResult = ok({Name, NumBlocks0}),
        NumBlocks = det_uint32_to_int(NumBlocks0),
        read_n(read_block(PZ, Input), NumBlocks, MaybeBlocks, !IO),
        ( MaybeBlocks = ok(Blocks),
            % XXX: This signature is fake.
            Signature = pz_signature([], []),
            Result = ok(pz_proc(q_name_from_dotted_string(Name),
                Signature, yes(Blocks)))
        ; MaybeBlocks = error(Error),
            Result = error(Error)
        )
    ; HeadResult = error(Error),
        Result = error(Error)
    ).

:- pred read_block(pz::in, binary_input_stream::in, maybe_error(pz_block)::out,
    io::di, io::uo) is det.

read_block(PZ, Input, Result, !IO) :-
    read_uint32(Input, MaybeNumInstrObjs, !IO),
    ( MaybeNumInstrObjs = ok(NumInstrObjs0),
        NumInstrObjs = det_uint32_to_int(NumInstrObjs0),
        read_n(read_code_item(PZ, Input), NumInstrObjs, MaybeInstrObjs, !IO),
        Result = maybe_error_map((func(Is) = pz_block(Is)), MaybeInstrObjs)
    ; MaybeNumInstrObjs = error(Error),
        Result = error(Error)
    ).

:- pred read_code_item(pz::in, binary_input_stream::in,
    maybe_error(pz_instr_obj)::out, io::di, io::uo) is det.

read_code_item(PZ, Input, Result, !IO) :-
    read_uint8(Input, TypeByteResult, !IO),
    ( TypeByteResult = ok(TypeByte),
        ( if code_entry_byte(Type, TypeByte) then
            ( Type = code_instr,
                read_instr(PZ, Input, Result, !IO)
            ;
                ( Type = code_meta_context
                ; Type = code_meta_context_short
                ; Type = code_meta_context_nil
                ),
                read_context(PZ, Input, Type, Result, !IO)
            )
        else
            Result = error("Invalid code entry type")
        )
    ; TypeByteResult = error(Error),
        Result = error(Error)
    ).

:- pred read_instr(pz::in, binary_input_stream::in,
    maybe_error(pz_instr_obj)::out, io::di, io::uo) is det.

read_instr(PZ, Input, Result, !IO) :-
    read_uint8(Input, OpcodeByteResult, !IO),
    ( OpcodeByteResult = ok(OpcodeByte),
        ( if opcode_byte(Opcode, OpcodeByte) then
            instruction_encoding(Opcode, WidthsNeeded, ImmediateNeeded),
            ( WidthsNeeded = no_width,
                MaybeWidths = ok(no_width)
            ; WidthsNeeded = one_width,
                read_width(Input, MaybeWidth, !IO),
                MaybeWidths = maybe_error_map(func(W) = one_width(W),
                    MaybeWidth)
            ; WidthsNeeded = two_widths,
                read_width(Input, MaybeWidthA, !IO),
                read_width(Input, MaybeWidthB, !IO),
                MaybeWidths = maybe_error_map(
                    func({A, B}) = two_widths(A, B),
                    combine_read_2(MaybeWidthA, MaybeWidthB))
            ),
            read_immediate(PZ, Input, ImmediateNeeded, MaybeMaybeImmediate,
                !IO),
            MaybeWidthsImmediate = combine_read_2(MaybeWidths,
                MaybeMaybeImmediate),
            ( MaybeWidthsImmediate = ok({Widths, MaybeImmediate}),
                ( if instruction(Instr, Opcode, Widths, MaybeImmediate) then
                    Result = ok(pzio_instr(Instr))
                else
                    unexpected($file, $pred,
                        "Error in instruction encoding data for " ++
                            string(Opcode))
                )
            ; MaybeWidthsImmediate = error(Error),
                Result = error(Error)
            )
        else
            Result = error("Unknown opcode")
        )
    ; OpcodeByteResult = error(Error),
        Result = error(Error)
    ).

:- pred read_immediate(pz::in, binary_input_stream::in, immediate_needed::in,
    maybe_error(maybe(pz_immediate_value))::out, io::di, io::uo) is det.

read_immediate(_, _, im_none, ok(no), !IO).
read_immediate(_, Input, im_num, Result, !IO) :-
    % XXX: The immediate value is always encoded as a 32 bit number but
    % this restriction should be lifted.
    read_int32(Input, MaybeInt, !IO),
    Result = maybe_error_map(func(N) = yes(pz_im_i32(N)), MaybeInt).
read_immediate(PZ, Input, im_closure, Result, !IO) :-
    read_closure_id(PZ, Input, MaybeClosureId, !IO),
    Result = maybe_error_map(func(C) = yes(pz_im_closure(C)),
        MaybeClosureId).
read_immediate(PZ, Input, im_proc, Result, !IO) :-
    read_proc_id(PZ, Input, MaybeProcId, !IO),
    Result = maybe_error_map(func(P) = yes(pz_im_proc(P)),
        MaybeProcId).
read_immediate(PZ, Input, im_import, Result, !IO) :-
    read_import_id(PZ, Input, MaybeImportId, !IO),
    Result = maybe_error_map(func(I) = yes(pz_im_import(I)),
        MaybeImportId).
read_immediate(PZ, Input, im_struct, Result, !IO) :-
    read_struct_id(PZ, Input, MaybeStructId, !IO),
    Result = maybe_error_map(func(S) = yes(pz_im_struct(S)),
        MaybeStructId).
read_immediate(PZ, Input, im_struct_field, Result, !IO) :-
    read_struct_id(PZ, Input, MaybeStructId, !IO),
    read_uint8(Input, MaybeFieldNo, !IO),
    MaybeStructField = combine_read_2(MaybeStructId, MaybeFieldNo),
    Result = maybe_error_map(
        func({S, F}) = yes(pz_im_struct_field(S, field_num(to_int(F) + 1))),
        MaybeStructField).
read_immediate(_, Input, im_label, Result, !IO) :-
    read_uint32(Input, MaybeInt, !IO),
    Result = maybe_error_map(func(L) = yes(pz_im_label(L)), MaybeInt).
read_immediate(_, Input, im_depth, Result, !IO) :-
    read_uint8(Input, MaybeInt, !IO),
    Result = maybe_error_map(func(D) = yes(pz_im_depth(to_int(D))),
        MaybeInt).

:- pred read_context(pz::in, binary_input_stream::in,
    code_entry_type::in(code_entry_type_context),
    maybe_error(pz_instr_obj)::out, io::di, io::uo) is det.

read_context(PZ, Input, code_meta_context, Result, !IO) :-
    read_data_id(PZ, Input, MaybeDataId, !IO),
    read_uint32(Input, MaybeLine, !IO),
    MaybeContext = combine_read_2(MaybeDataId, MaybeLine),
    ( MaybeContext = ok({DataId, Line}),
        ( if data_get_filename(PZ, DataId, Filename0) then
            Filename = Filename0
        else
            unexpected($file, $pred, "Bad filename in context information")
        ),
        Context = context(Filename, det_uint32_to_int(Line), 0),
        Result = ok(pzio_context(pz_context(Context, DataId)))
    ; MaybeContext = error(Error),
        Result = error(Error)
    ).

read_context(_, Input, code_meta_context_short, Result, !IO) :-
    read_uint32(Input, MaybeLine, !IO),
    Result = maybe_error_map(
        (func(I) = pzio_context(pz_context_short(det_uint32_to_int(I)))),
        MaybeLine).
read_context(_, _Input, code_meta_context_nil, Result, !IO) :-
    Result = ok(pzio_context(pz_nil_context)).

%-----------------------------------------------------------------------%

:- pred read_closures(binary_input_stream::in, uint32::in,
    pz::in, maybe_error(pz)::out, io::di, io::uo) is det.

read_closures(Input, Num, PZ0, Result, !IO) :-
    read_items(read_closure(Input),
        (pred(N::in, I::in, PZI0::in, PZI::out) is det :-
            ( if pzc_id_from_num(PZI0, N, ClosureId) then
                pz_add_closure(ClosureId, I, PZI0, PZI)
            else
                unexpected($file, $pred, "Bad Closure Id")
            )
        ),
        Num, 0u32, PZ0, Result, !IO).

:- pred read_closure(binary_input_stream::in, pz::in,
    maybe_error(pz_closure)::out, io::di, io::uo) is det.

read_closure(Input, PZ, Result, !IO) :-
    read_proc_id(PZ, Input, MaybeProc, !IO),
    read_data_id(PZ, Input, MaybeData, !IO),
    MaybePair = combine_read_2(MaybeProc, MaybeData),
    Result = maybe_error_map(
        func({Proc, Data}) = pz_closure(Proc, Data), MaybePair).

%-----------------------------------------------------------------------%

:- pred read_exports(binary_input_stream::in, uint32::in,
    pz::in, maybe_error(pz)::out, io::di, io::uo) is det.

read_exports(Input, Num, PZ0, Result, !IO) :-
    read_items(read_export(Input),
        (pred(_::in, {Name, CloId}::in, PZI0::in, PZI::out) is det :-
            pz_export_closure(CloId, Name, PZI0, PZI)
        ),
        Num, 0u32, PZ0, Result, !IO).

:- pred read_export(binary_input_stream::in, pz::in,
    maybe_error({nq_name, pzc_id})::out, io::di, io::uo) is det.

read_export(Input, PZ, Result, !IO) :-
    read_len_string(Input, MaybeName, !IO),
    read_uint32(Input, MaybeId, !IO),
    MaybePair = combine_read_2(MaybeName, MaybeId),
    Result = maybe_error_map(
        (func({Name, Num}) = {nq_name_det(Name), Id} :-
            ( if pzc_id_from_num(PZ, Num, Id0) then
                Id = Id0
            else
                unexpected($file, $pred, "Invalid closure id")
            )
        ), MaybePair).

%-----------------------------------------------------------------------%

:- pred read_width(binary_input_stream::in, maybe_error(pz_width)::out,
    io::di, io::uo) is det.

read_width(Input, Result, !IO) :-
    read_uint8(Input, MaybeByte, !IO),
    ( MaybeByte = ok(Byte),
        ( if pz_width_byte(Width, Byte) then
            Result = ok(Width)
        else
            Result = error("Invalid width")
        )
    ; MaybeByte = error(Error),
        Result = error(Error)
    ).

:- pred read_struct_id(pz::in, binary_input_stream::in,
    maybe_error(pzs_id)::out, io::di, io::uo) is det.

read_struct_id(PZ, Input, Result, !IO) :-
    read_uint32(Input, MaybeStructNum, !IO),
    ( MaybeStructNum = ok(StructNum),
        ( if pzs_id_from_num(PZ, StructNum, StructId) then
            Result = ok(StructId)
        else
            Result = error("Unknown struct")
        )
    ; MaybeStructNum = error(Error),
        Result = error(Error)
    ).

:- pred read_data_id(pz::in, binary_input_stream::in,
    maybe_error(pzd_id)::out, io::di, io::uo) is det.

read_data_id(PZ, Input, Result, !IO) :-
    read_uint32(Input, MaybeNum, !IO),
    ( MaybeNum = ok(Num),
        ( if pzd_id_from_num(PZ, Num, DataId) then
            Result = ok(DataId)
        else
            Result = error("Unknown data item")
        )
    ; MaybeNum = error(Error),
        Result = error(Error)
    ).

:- pred read_proc_id(pz::in, binary_input_stream::in,
    maybe_error(pzp_id)::out, io::di, io::uo) is det.

read_proc_id(PZ, Input, Result, !IO) :-
    read_uint32(Input, MaybeNum, !IO),
    ( MaybeNum = ok(Num),
        ( if pzp_id_from_num(PZ, Num, ProcId) then
            Result = ok(ProcId)
        else
            Result = error("Unknown procedure")
        )
    ; MaybeNum = error(Error),
        Result = error(Error)
    ).

:- pred read_closure_id(pz::in, binary_input_stream::in,
    maybe_error(pzc_id)::out, io::di, io::uo) is det.

read_closure_id(PZ, Input, Result, !IO) :-
    read_uint32(Input, MaybeNum, !IO),
    ( MaybeNum = ok(Num),
        ( if pzc_id_from_num(PZ, Num, ClosureId) then
            Result = ok(ClosureId)
        else
            Result = error("Unknown closure")
        )
    ; MaybeNum = error(Error),
        Result = error(Error)
    ).

:- pred read_import_id(pz::in, binary_input_stream::in,
    maybe_error(pzi_id)::out, io::di, io::uo) is det.

read_import_id(PZ, Input, Result, !IO) :-
    read_uint32(Input, MaybeNum, !IO),
    ( MaybeNum = ok(Num),
        ( if pzi_id_from_num(PZ, Num, ImportId) then
            Result = ok(ImportId)
        else
            Result = error("Unknown import")
        )
    ; MaybeNum = error(Error),
        Result = error(Error)
    ).

%-----------------------------------------------------------------------%

:- pred read_items(pred(pz, maybe_error(T), io, io), pred(uint32, T, pz, pz),
    uint32, uint32, pz, maybe_error(pz), io, io).
:- mode read_items(pred(in, out, di, uo) is det, pred(in, in, in, out) is det,
    in, in, in, out, di, uo) is det.

read_items(Read, Add, Num, Cur, PZ0, Result, !IO) :-
    ( if Cur < Num then
        Read(PZ0, Result0, !IO),
        ( Result0 = ok(Item),
            Add(Cur, Item, PZ0, PZ),
            read_items(Read, Add, Num, Cur + 1u32, PZ, Result, !IO)
        ; Result0 = error(Error),
            Result = error(Error)
        )
    else
        Result = ok(PZ0)
    ).

:- pred read_n(pred(maybe_error(T), io, io), int, maybe_error(list(T)), io, io).
:- mode read_n(pred(out, di, uo) is det, in, out, di, uo) is det.

read_n(Pred, N, Result, !IO) :-
    ( if N > 0 then
        Pred(HeadResult, !IO),
        ( HeadResult = ok(Head),
            read_n(Pred, N - 1, Result0, !IO),
            ( Result0 = ok(Tail),
                Result = ok([Head | Tail])
            ; Result0 = error(Error),
                Result = error(Error)
            )
        ; HeadResult = error(Error),
            Result = error(Error)
        )
    else
        Result = ok([])
    ).

:- pred read_map(pred(T, maybe_error(U), io, io),
    list(T), maybe_error(list(U)), io, io).
:- mode read_map(pred(in, out, di, uo) is det,
    in, out, di, uo) is det.

read_map(_, [], ok([]), !IO).
read_map(Read, [X | Xs], Result, !IO) :-
    Read(X, ResultY, !IO),
    ( ResultY = ok(Y),
        read_map(Read, Xs, ResultYs, !IO),
        ( ResultYs = ok(Ys),
            Result = ok([Y | Ys])
        ; ResultYs = error(Error),
            Result = error(Error)
        )
    ; ResultY = error(Error),
        Result = error(Error)
    ).

%-----------------------------------------------------------------------%

:- pred data_get_filename(pz::in, pzd_id::in, string::out) is semidet.

data_get_filename(PZ, DataId, String) :-
    Data = pz_lookup_data(PZ, DataId),
    pz_data(DataType, Items0) = Data,
    type_array(pzw_8, _NumItems) = DataType,
    % Drop the null byte at the end of the list.
    det_take(length(Items0) - 1, Items0, Items),
    map((pred(pzv_num(I)::in, C::out) is semidet :-
            from_int(I, C)
        ), Items, Chars),
    String = string.from_char_list(Chars).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
