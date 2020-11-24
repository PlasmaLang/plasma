%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module file_utils.
%
% Copyright (C) 2020 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
% File handling utils specific to Plasma.  These are general for the
% different compiler tools but not general enough to go into the utils
% package.
%
%-----------------------------------------------------------------------%

:- interface.

:- import_module list.
:- import_module maybe.
:- import_module string.

:- import_module compile_error.
:- import_module q_name.

%-----------------------------------------------------------------------%

    % find_module_file(FileList, Extension, ModuleName) = Result.
    %
    % Find the interface on the disk. For now we look in the current
    % directory only, later we'll implement include paths.
    %
:- func find_module_file(list(string), string, q_name) =
    maybe_error(string, compile_error).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
:- implementation.

:- import_module constant.
:- import_module util.
:- import_module util.exception.
:- import_module util.path.

%-----------------------------------------------------------------------%

find_module_file(DirList, Extension, ModuleName) = Result :-
    filter(matching_module_file(ModuleName, Extension), DirList, Matches),
    ( Matches = [],
        Result = error(ce_module_not_found(ModuleName))
    ; Matches = [FileName],
        Result = ok(FileName)
    ; Matches = [_, _ | _],
        compile_error($file, $pred, "Ambigious interfaces found")
    ).

:- pred matching_module_file(q_name::in, string::in, string::in) is semidet.

matching_module_file(ModuleName, Extension, FileName) :-
    filename_extension(Extension, FileName, FileNameBase),
    strip_file_name_punctuation(q_name_to_string(ModuleName)) =
        strip_file_name_punctuation(FileNameBase).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
