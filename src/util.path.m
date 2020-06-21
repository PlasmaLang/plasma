%-----------------------------------------------------------------------%
% Path Utility code
% vim: ts=4 sw=4 et
%
% Copyright (C) 2020 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
%-----------------------------------------------------------------------%
:- module util.path.

:- interface.

%-----------------------------------------------------------------------%

    % file_and_dir(Path, Dir, File).
    %
    % Path = Dir ++ "/" ++ File AND File has no '/'.
    %
:- pred file_and_dir(string, string, string).
:- mode file_and_dir(in, out, out) is det.

:- pred file_change_extension(string, string, string, string).
:- mode file_change_extension(in, in, in, out) is det.

    % filename_extension(Ext, FullName, Base).
    %
    % FullName = Base ++ Ext
    %
:- pred filename_extension(string, string, string).
:- mode filename_extension(in, in, out) is det.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
:- implementation.

:- import_module char.
:- import_module int.
:- import_module pair.
:- import_module string.

%-----------------------------------------------------------------------%

file_and_dir(Path, Dir, File) :-
    FilePartLength = suffix_length((pred(C::in) is semidet :-
            C \= ('/')
        ), Path),
    % This length is in code units.
    left(Path, length(Path) - FilePartLength - 1, Dir0),
    ( if Dir0 \= "" then
        Dir = Dir0
    else
        Dir = "."
    ),

    right(Path, FilePartLength, File).

file_change_extension(ExtA, ExtB, FileA, FileB) :-
    filename_extension(ExtA, FileA, Base),
    FileB = Base ++ ExtB.

filename_extension(Ext, File, Base) :-
    ( if remove_suffix(File, Ext, Base0) then
        Base = Base0
    else
        Base = File
    ).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
