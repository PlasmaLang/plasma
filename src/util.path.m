%-----------------------------------------------------------------------%
% Path Utility code
% vim: ts=4 sw=4 et
%
% Copyright (C) 2020-2021 Plasma Team
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
:- mode file_and_dir(in, out, out) is semidet.
:- mode file_and_dir(out, in, in) is det.

    % file_and_dir(DefaultDir, Path, Dir, File).
    %
    % As above except if Path is unqualified then Dir = DefaultDir.
    %
:- pred file_and_dir_det(string, string, string, string).
:- mode file_and_dir_det(in, in, out, out) is det.

    % file_part(Path, File) :-
    %   file_and_dir(Path, _, File).
    %
:- pred file_part(string::in, string::out) is det.

    % file_change_extension(ExtA, ExtB, FileA, FileB)
    %   Basename ++ ExtA = FileA,
    %   Basename ++ ExtB = FileB
    %
:- pred file_change_extension(string, string, string, string).
:- mode file_change_extension(in, in, in, out) is det.

    % filename_extension(Ext, FullName, Base).
    %
    % FullName = Base ++ Ext
    %
:- pred filename_extension(string, string, string).
:- mode filename_extension(in, in, out) is det.

:- pred is_absolute(string::in) is semidet.
:- pred is_relative(string::in) is semidet.

%-----------------------------------------------------------------------%

    % TempFilename = make_temp_filename(Filename),
    %
    % Make a file name similar to Filename that can be used to write
    % incomplete data before moving it to Filename.
    %
:- func make_temp_filename(string) = string.

%-----------------------------------------------------------------------%

:- func strip_file_name_punctuation(string) = string.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
:- implementation.

:- import_module char.
:- import_module int.
:- import_module list.
:- import_module pair.
:- import_module string.

%-----------------------------------------------------------------------%

:- pragma promise_equivalent_clauses(file_and_dir/3).

file_and_dir(Path::in, Dir::out, File::out) :-
    FilePartLength = suffix_length((pred(C::in) is semidet :-
            C \= ('/')
        ), Path),
    % This length is in code units.
    left(Path, length(Path) - FilePartLength - 1, Dir),
    Dir \= "",
    right(Path, FilePartLength, File).
file_and_dir(Path::out, Dir::in, File::in) :-
    Path = Dir ++ "/" ++ File.

file_and_dir_det(DefaultDir, Path, Dir, File) :-
    ( if file_and_dir(Path, Dir0, File0) then
        Dir = Dir0,
        File = File0
    else
        Dir = DefaultDir,
        File = Path
    ).

file_part(Path, File) :-
    file_and_dir_det("", Path, _, File).

file_change_extension(ExtA, ExtB, FileA, FileB) :-
    filename_extension(ExtA, FileA, Base),
    FileB = Base ++ ExtB.

filename_extension(Ext, File, Base) :-
    ( if remove_suffix(File, Ext, Base0) then
        Base = Base0
    else
        Base = File
    ).

is_absolute(Path) :-
    append("/", _, Path).
is_relative(Path) :-
    \+ is_absolute(Path).

%-----------------------------------------------------------------------%

make_temp_filename(Orig) = Orig ++ "~".

%-----------------------------------------------------------------------%

strip_file_name_punctuation(Input) = Output :-
    to_char_list(Input, InputList),
    filter_map((pred(C0::in, C::out) is semidet :-
            ( if
                ( C0 = '_'
                ; C0 = ('-')
                )
            then
                false % Strip character
            else
                C = to_lower(C0)
            )
        ), InputList, OutputList),
    from_char_list(OutputList, Output).

%-----------------------------------------------------------------------%
