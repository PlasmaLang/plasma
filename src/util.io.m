%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module util.io.
%
% Tag Length Value serialisation.
%
% Copyright (C) 2015, 2020 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
%-----------------------------------------------------------------------%

:- interface.

:- import_module io.
:- import_module list.
:- import_module maybe.

%-----------------------------------------------------------------------%

    % write_len_string(Stream, String, !IO)
    %
    % Write a 16bit length followed by the string.
    %
:- pred write_len_string(binary_output_stream::in, string::in,
    io::di, io::uo) is det.

:- pred read_len_string(binary_input_stream::in, maybe_error(string)::out,
    io::di, io::uo) is det.

    % write_string(Stream, String, !IO)
    %
:- pred write_string(binary_output_stream::in, string::in,
    io::di, io::uo) is det.

:- pred read_string(binary_input_stream::in, int::in,
    maybe_error(string)::out, io::di, io::uo) is det.

:- pred read_uint8(binary_input_stream::in, maybe_error(uint8)::out,
    io::di, io::uo) is det.

:- pred read_uint16(binary_input_stream::in, maybe_error(uint16)::out,
    io::di, io::uo) is det.

:- pred read_uint32(binary_input_stream::in, maybe_error(uint32)::out,
    io::di, io::uo) is det.

:- pred read_int32(binary_input_stream::in, maybe_error(int32)::out,
    io::di, io::uo) is det.

:- pred read_uint64(binary_input_stream::in, maybe_error(uint64)::out,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------%

:- func combine_read_2(maybe_error(T1), maybe_error(T2))
    = maybe_error({T1, T2}).

:- func combine_read_3(maybe_error(T1), maybe_error(T2), maybe_error(T3))
    = maybe_error({T1, T2, T3}).

:- func combine_read_5(maybe_error(T1), maybe_error(T2), maybe_error(T3),
        maybe_error(T4), maybe_error(T5))
    = maybe_error({T1, T2, T3, T4, T5}).

:- func combine_read_6(maybe_error(T1), maybe_error(T2), maybe_error(T3),
        maybe_error(T4), maybe_error(T5), maybe_error(T6))
    = maybe_error({T1, T2, T3, T4, T5, T6}).

:- func combine_read_7(maybe_error(T1), maybe_error(T2), maybe_error(T3),
        maybe_error(T4), maybe_error(T5), maybe_error(T6), maybe_error(T7))
    = maybe_error({T1, T2, T3, T4, T5, T6, T7}).

%-----------------------------------------------------------------------%

:- pred write_temp_and_move(pred(binary_output_stream, maybe_error, io, io),
    string, maybe_error, io, io).
:- mode write_temp_and_move(pred(in, out, di, uo) is det,
    in, out, di, uo) is det.

%-----------------------------------------------------------------------%

    % List of all the files in the current working directory.
    %
:- pred get_dir_list(maybe_error(list(string))::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module char.
:- import_module int.
:- import_module string.
:- import_module uint16.
:- import_module uint8.

:- import_module util.path.

%-----------------------------------------------------------------------%

write_len_string(Stream, String, !IO) :-
    write_binary_uint16_le(Stream, det_from_int(length(String)), !IO),
    write_string(Stream, String, !IO).

read_len_string(Stream, MaybeString, !IO) :-
    read_uint16(Stream, MaybeLen, !IO),
    ( MaybeLen = ok(Len),
        read_string(Stream, to_int(Len), MaybeString, !IO)
    ; MaybeLen = error(Error),
        MaybeString = error(Error)
    ).

write_string(Stream, String, !IO) :-
    foldl(write_char_as_byte(Stream), String, !IO).

read_string(Stream, Len, MaybeString, !IO) :-
    read_string_2(Stream, Len, "", MaybeString, !IO).

:- pred read_string_2(binary_input_stream::in, int::in, string::in,
    maybe_error(string)::out, io::di, io::uo) is det.

read_string_2(Stream, Len, Head, MaybeString, !IO) :-
    ( if Len > 0 then
        read_char_as_byte(Stream, MaybeChar, !IO),
        ( MaybeChar = ok(Char),
            % This is pretty inefficient. Fix it later / after bootstrap.
            % It's an interesting case where we should make the efficient
            % thing natural/easy/convenient in Plasma.
            read_string_2(Stream, Len - 1, Head ++ char_to_string(Char),
                MaybeString, !IO)
        ; MaybeChar = error(Error),
            MaybeString = error(Error)
        )
    else
        MaybeString = ok(Head)
    ).

:- pred write_char_as_byte(binary_output_stream::in, char::in,
    io::di, io::uo) is det.

write_char_as_byte(Stream, Char, !IO) :-
    write_binary_uint8(Stream, det_from_int(to_int(Char) /\ 0xFF), !IO).

:- pred read_char_as_byte(binary_input_stream::in, maybe_error(char)::out,
    io::di, io::uo) is det.

read_char_as_byte(Stream, MaybeChar, !IO) :-
    read_binary_uint8(Stream, Result, !IO),
    ( Result = ok(UInt8),
        MaybeChar = ok(det_from_int(to_int(UInt8)))
    ; Result = eof,
        MaybeChar = error("eof")
    ; Result = error(Error),
        MaybeChar = error(error_message(Error))
    ).

%-----------------------------------------------------------------------%

read_uint8(Stream, Result, !IO) :-
    read_binary_uint8(Stream, Result0, !IO),
    ( Result0 = ok(UInt8),
        Result = ok(UInt8)
    ; Result0 = eof,
        Result = error("eof")
    ; Result0 = error(Error),
        Result = error(error_message(Error))
    ).

read_uint16(Stream, simplify_result(Result), !IO) :-
    read_binary_uint16_le(Stream, Result, !IO).

read_uint32(Stream, simplify_result(Result), !IO) :-
    read_binary_uint32_le(Stream, Result, !IO).

read_int32(Stream, simplify_result(Result), !IO) :-
    read_binary_int32_le(Stream, Result, !IO).

read_uint64(Stream, simplify_result(Result), !IO) :-
    read_binary_uint64_le(Stream, Result, !IO).

:- func simplify_result(maybe_incomplete_result(T)) = maybe_error(T).

simplify_result(Result) = MaybeInt :-
    ( Result = ok(Int),
        MaybeInt = ok(Int)
    ;
        ( Result = eof
        ; Result = incomplete(_)
        ),
        MaybeInt = error("eof")
    ; Result = error(Error),
        MaybeInt = error(error_message(Error))
    ).

%-----------------------------------------------------------------------%

combine_read_2(Res1, Res2) = Res :-
    ( Res1 = ok(Ok1),
        ( Res2 = ok(Ok2),
            Res = ok({Ok1, Ok2})
        ; Res2 = error(Error),
            Res = error(Error)
        )
    ; Res1 = error(Error),
        Res = error(Error)
    ).

combine_read_3(Res1, Res2, Res3) = Res :-
    ( Res1 = ok(Ok1),
        ( Res2 = ok(Ok2),
            ( Res3 = ok(Ok3),
                Res = ok({Ok1, Ok2, Ok3})
            ; Res3 = error(Error),
                Res = error(Error)
            )
        ; Res2 = error(Error),
            Res = error(Error)
        )
    ; Res1 = error(Error),
        Res = error(Error)
    ).

combine_read_5(Res1, Res2, Res3, Res4, Res5) = Res :-
    ( Res1 = ok(Ok1),
        ( Res2 = ok(Ok2),
            ( Res3 = ok(Ok3),
                ( Res4 = ok(Ok4),
                    ( Res5 = ok(Ok5),
                        Res = ok({Ok1, Ok2, Ok3, Ok4, Ok5})
                    ; Res5 = error(Error),
                        Res = error(Error)
                    )
                ; Res4 = error(Error),
                    Res = error(Error)
                )
            ; Res3 = error(Error),
                Res = error(Error)
            )
        ; Res2 = error(Error),
            Res = error(Error)
        )
    ; Res1 = error(Error),
        Res = error(Error)
    ).

combine_read_6(Res1, Res2, Res3, Res4, Res5, Res6) = Res :-
    ( Res1 = ok(Ok1),
        ( Res2 = ok(Ok2),
            ( Res3 = ok(Ok3),
                ( Res4 = ok(Ok4),
                    ( Res5 = ok(Ok5),
                        ( Res6 = ok(Ok6),
                            Res = ok({Ok1, Ok2, Ok3, Ok4, Ok5, Ok6})
                        ; Res6 = error(Error),
                            Res = error(Error)
                        )
                    ; Res5 = error(Error),
                        Res = error(Error)
                    )
                ; Res4 = error(Error),
                    Res = error(Error)
                )
            ; Res3 = error(Error),
                Res = error(Error)
            )
        ; Res2 = error(Error),
            Res = error(Error)
        )
    ; Res1 = error(Error),
        Res = error(Error)
    ).

combine_read_7(Res1, Res2, Res3, Res4, Res5, Res6, Res7) = Res :-
    ( Res1 = ok(Ok1),
        ( Res2 = ok(Ok2),
            ( Res3 = ok(Ok3),
                ( Res4 = ok(Ok4),
                    ( Res5 = ok(Ok5),
                        ( Res6 = ok(Ok6),
                            ( Res7 = ok(Ok7),
                                Res = ok({Ok1, Ok2, Ok3, Ok4, Ok5, Ok6, Ok7})
                            ; Res7 = error(Error),
                                Res = error(Error)
                            )
                        ; Res6 = error(Error),
                            Res = error(Error)
                        )
                    ; Res5 = error(Error),
                        Res = error(Error)
                    )
                ; Res4 = error(Error),
                    Res = error(Error)
                )
            ; Res3 = error(Error),
                Res = error(Error)
            )
        ; Res2 = error(Error),
            Res = error(Error)
        )
    ; Res1 = error(Error),
        Res = error(Error)
    ).

%-----------------------------------------------------------------------%

write_temp_and_move(Write, Filename, Result, !IO) :-
    TempFilename = make_temp_filename(Filename),
    io.open_binary_output(TempFilename, MaybeFile, !IO),
    ( MaybeFile = ok(File),
        Write(File, Result0, !IO),
        io.close_binary_output(File, !IO),
        io.rename_file(TempFilename, Filename, MoveRes, !IO),
        ( MoveRes = ok,
            Result = Result0
        ; MoveRes = error(Error),
            Result =
                error(format("%s: %s", [s(Filename), s(error_message(Error))]))
        )
    ; MaybeFile = error(Error),
        Result =
            error(format("%s: %s", [s(Filename), s(error_message(Error))]))
    ).

%-----------------------------------------------------------------------%

get_dir_list(MaybeFiles, !IO) :-
    opendir(".", MaybeDir, !IO),
    ( MaybeDir = ok(Dir),
        lsdir(Dir, [], MaybeFiles, !IO),
        closedir(Dir, !IO)
    ; MaybeDir = error(Error),
        MaybeFiles = error(Error)
    ).

:- pragma foreign_decl("C", local,
    "
        #include <sys/types.h>
        #include <dirent.h>
    ").

:- type dir.
:- pragma foreign_type("C", dir, "DIR*").

:- pred opendir(string::in, maybe_error(dir)::out, io::di, io::uo) is det.

opendir(Name, MaybeDir, !IO) :-
    opendir_c(Name, Ok, Dir, Error, !IO),
    ( Ok = yes,
        MaybeDir = ok(Dir)
    ; Ok = no,
        MaybeDir = error(Error)
    ).

:- pred opendir_c(string::in, bool::out, dir::out, string::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    opendir_c(Name::in, Ok::out, Dir::out, Error::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, will_not_throw_exception,
        thread_safe],
    "
        Dir = opendir(Name);
        if (Dir) {
            Ok = MR_TRUE;
            Error = NULL;
        } else {
            Ok = MR_FALSE;
            Error = GC_strdup(strerror(errno));
        }
    ").


:- pred closedir(dir::in, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    closedir(Dir::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, will_not_throw_exception,
        thread_safe],
    "
        closedir(Dir);
    ").

:- pred lsdir(dir::in, list(string)::in, maybe_error(list(string))::out,
    io::di, io::uo) is det.

lsdir(Dir, Acc, Result, !IO) :-
    readdir(Dir, MaybeRead, !IO),
    ( MaybeRead = ok(Name),
        lsdir(Dir, [Name | Acc], Result, !IO)
    ; MaybeRead = eof,
        Result = ok(reverse(Acc))
    ; MaybeRead = error(Error),
        Result = error(Error)
    ).

:- type readdir_result
    --->    ok(string)
    ;       eof
    ;       error(string).

:- pred readdir(dir::in, readdir_result::out, io::di, io::uo) is det.

readdir(Dir, Result, !IO) :-
    readdir_c(Dir, Ok, EOF, Entry, Error, !IO),
    ( Ok = yes,
        Result = ok(Entry)
    ; Ok = no,
        ( EOF = yes,
            Result = eof
        ; EOF = no,
            Result = error(Error)
        )
    ).

:- pred readdir_c(dir::in, bool::out, bool::out, string::out, string::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    readdir_c(Dir::in, Ok::out, Eof::out, Entry::out, Error::out,
        _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, will_not_throw_exception],
    "
        errno = 0;

        // readdir is not defined to be threadsafe, but most modern systems
        // will implement it this way. We don not need to take any chances.
        struct dirent *ent = readdir(Dir);
        if (ent) {
            Ok = MR_TRUE;
            Entry = GC_strdup(ent->d_name);
        } else {
            Ok = MR_FALSE;
            if (errno) {
                Eof = MR_FALSE;
                Error = GC_strdup(strerror(errno));
            } else {
                Eof = MR_TRUE;
            }
        }
    ").

%-----------------------------------------------------------------------%
