%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module io_utils.
%
% Tag Length Value serialisation.
%
% Copyright (C) 2015 Paul Bone
% Distributed under the terms of the GPLv2 see ../LICENSE.tools
%
%-----------------------------------------------------------------------%

:- interface.

:- import_module io.
:- import_module int.
:- import_module string.

%-----------------------------------------------------------------------%

    % write_len_string(Stream, String, !IO)
    %
    % Write a 16bit length followed by the string.
    %
:- pred write_len_string(binary_output_stream::in, string::in,
    io::di, io::uo) is det.

    % write_string(Stream, String, !IO)
    %
:- pred write_string(binary_output_stream::in, string::in,
    io::di, io::uo) is det.

    % write_int8(Stream, Int16, !IO)
    %
:- pred write_int8(binary_output_stream::in, int::in,
    io::di, io::uo) is det.

    % write_int16(Stream, Int16, !IO)
    %
:- pred write_int16(binary_output_stream::in, int::in,
    io::di, io::uo) is det.

    % write_int32(Stream, Int16, !IO)
    %
:- pred write_int32(binary_output_stream::in, int::in,
    io::di, io::uo) is det.

    % write_int64(Stream, Int16, !IO)
    %
:- pred write_int64(binary_output_stream::in, int::in,
    io::di, io::uo) is det.

    % Write an int of up to 64bits using relatively few bytes.
    %
:- pred write_int_any_width(binary_output_stream::in, int::in,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module int.

%-----------------------------------------------------------------------%

write_len_string(Stream, String, !IO) :-
    write_int16(Stream, length(String), !IO),
    write_string(Stream, String, !IO).

write_string(Stream, String, !IO) :-
    foldl(write_char_as_byte(Stream), String, !IO).

:- pred write_char_as_byte(binary_output_stream::in, char::in,
    io::di, io::uo) is det.

write_char_as_byte(Stream, Char, !IO) :-
    write_byte(Stream, to_int(Char) /\ 0xFF, !IO).

%-----------------------------------------------------------------------%

write_int8(Stream, Int, !IO) :-
    write_byte(Stream, Int /\ 0xFF, !IO).

write_int16(Stream, Int, !IO) :-
    write_byte(Stream, (Int /\ 0xFF00) >> 8, !IO),
    write_byte(Stream, Int /\ 0x00FF, !IO).

write_int32(Stream, Int, !IO) :-
    write_byte(Stream, (Int /\ 0xFF000000) >> 24, !IO),
    write_byte(Stream, (Int /\ 0x00FF0000) >> 16, !IO),
    write_byte(Stream, (Int /\ 0x0000FF00) >> 8, !IO),
    write_byte(Stream, Int /\ 0x000000FF, !IO).

write_int64(Stream, Int, !IO) :-
    write_byte(Stream, (Int /\ 0xFF00000000000000) >> 56, !IO),
    write_byte(Stream, (Int /\ 0x00FF000000000000) >> 48, !IO),
    write_byte(Stream, (Int /\ 0x0000FF0000000000) >> 40, !IO),
    write_byte(Stream, (Int /\ 0x000000FF00000000) >> 32, !IO),
    write_byte(Stream, (Int /\ 0x00000000FF000000) >> 24, !IO),
    write_byte(Stream, (Int /\ 0x0000000000FF0000) >> 16, !IO),
    write_byte(Stream, (Int /\ 0x000000000000FF00) >> 8, !IO),
    write_byte(Stream, Int /\ 0x00000000000000FF, !IO).

%-----------------------------------------------------------------------%

write_int_any_width(Stream, Int, !IO) :-
    ( Int < 0x80 ->
        write_byte(Stream, Int, !IO)
    ; Int < 0x0100 ->
        write_byte(Stream, 0x81, !IO),
        write_int8(Stream, Int, !IO)
    ; Int < 0x010000 ->
        write_byte(Stream, 0x82, !IO),
        write_int16(Stream, Int, !IO)
    ; Int < 0x0100000000 ->
        write_byte(Stream, 0x84, !IO),
        write_int32(Stream, Int, !IO)
    ;
        write_byte(Stream, 0x88, !IO),
        write_int64(Stream, Int, !IO)
    ).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
