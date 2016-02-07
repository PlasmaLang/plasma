%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module io_utils.
%
% Tag Length Value serialisation.
%
% Copyright (C) 2015 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
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

    % write_int32(Stream, Int32, !IO)
    %
:- pred write_int32(binary_output_stream::in, int::in,
    io::di, io::uo) is det.

    % write_int64(Stream, IntHigh32, IntLow32, !IO)
    %
:- pred write_int64(binary_output_stream::in, int::in, int::in,
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

write_int64(Stream, IntHigh, IntLow, !IO) :-
    write_int32(Stream, IntHigh, !IO),
    write_int32(Stream, IntLow, !IO).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
