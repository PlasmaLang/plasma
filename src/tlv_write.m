%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module tlv_write.
%
% Tag Length Value serialisation.
%
% Copyright (C) 2015 Paul Bone
% Distributed under the terms of the GPLv2 see ../LICENSE.tools
%
%-----------------------------------------------------------------------%

:- interface.

:- import_module io.
:- import_module string.

%-----------------------------------------------------------------------%

    % write_tag_length(Stream, Tag, Length, !IO)
    %
:- pred write_tag_length(binary_output_stream::in, int::in, int::in,
    io::di, io::uo) is det.

    % write_string(Stream, String, !IO)
    %
:- pred write_string(binary_output_stream::in, string::in,
    io::di, io::uo) is det.

    % write_int16(Stream, Int16, !IO)
    %
:- pred write_int16(binary_output_stream::in, int::in,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------%

    % write_tagged_string(Stream, Tag, String, !IO)
    %
:- pred write_tagged_string(binary_output_stream::in, int::in, string::in,
    io::di, io::uo) is det.

    % write_tagged_int16(Stream, Tag, Int16, !IO)
    %
:- pred write_tagged_int16(binary_output_stream::in, int::in, int::in,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module int.

%-----------------------------------------------------------------------%

write_tag_length(Stream, Tag, Length, !IO) :-
    write_tag(Stream, Tag, !IO),
    write_length(Stream, Length, !IO).

:- pred write_tag(binary_output_stream::in, int::in, io::di, io::uo) is det.

write_tag(Stream, Tag, !IO) :-
    write_int16(Stream, Tag, !IO).

:- pred write_length(binary_output_stream::in, int::in, io::di, io::uo)
    is det.

write_length(Stream, Length, !IO) :-
    write_int16(Stream, Length, !IO).

%-----------------------------------------------------------------------%

write_string(Stream, String, !IO) :-
    foldl(write_char_as_byte(Stream), String, !IO).

:- pred write_char_as_byte(binary_output_stream::in, char::in,
    io::di, io::uo) is det.

write_char_as_byte(Stream, Char, !IO) :-
    write_byte(Stream, to_int(Char) /\ 0xFF, !IO).

%-----------------------------------------------------------------------%

write_int16(Stream, Int, !IO) :-
    write_byte(Stream, (Int /\ 0xFF00) >> 8, !IO),
    write_byte(Stream, Int /\ 0x00FF, !IO).

%-----------------------------------------------------------------------%

write_tagged_string(Stream, Tag, String, !IO) :-
    write_tag_length(Stream, Tag, length(String), !IO),
    write_string(Stream, String, !IO).

%-----------------------------------------------------------------------%

write_tagged_int16(Stream, Tag, Int, !IO) :-
    write_tag_length(Stream, Tag, 2, !IO),
    write_int16(Stream, Int, !IO).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
