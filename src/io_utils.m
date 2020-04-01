%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module io_utils.
%
% Tag Length Value serialisation.
%
% Copyright (C) 2015, 2020 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
%-----------------------------------------------------------------------%

:- interface.

:- import_module io.

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

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module int.
:- import_module string.
:- import_module uint16.

%-----------------------------------------------------------------------%

write_len_string(Stream, String, !IO) :-
    write_binary_uint16_be(Stream, det_from_int(length(String)), !IO),
    write_string(Stream, String, !IO).

write_string(Stream, String, !IO) :-
    foldl(write_char_as_byte(Stream), String, !IO).

:- pred write_char_as_byte(binary_output_stream::in, char::in,
    io::di, io::uo) is det.

write_char_as_byte(Stream, Char, !IO) :-
    write_byte(Stream, to_int(Char) /\ 0xFF, !IO).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
