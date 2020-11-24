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

:- import_module io.
:- import_module list.
:- import_module maybe.
:- import_module string.

:- import_module compile_error.
:- import_module q_name.

%-----------------------------------------------------------------------%

    % Find the interface on the disk. For now we look in the current
    % directory only, later we'll implement include paths.
    %
:- pred find_interface(list(string)::in, q_name::in,
    maybe_error(string, compile_error)::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
:- implementation.

:- import_module constant.
:- import_module util.
:- import_module util.exception.
:- import_module util.path.

%-----------------------------------------------------------------------%

find_interface(DirList, ModuleName, Result, !IO) :-
    filter(matching_interface_file(ModuleName), DirList, Matches),
    ( Matches = [],
        Result = error(ce_module_not_found(ModuleName))
    ; Matches = [FileName],
        Result = ok(FileName)
    ; Matches = [_, _ | _],
        compile_error($file, $pred, "Ambigious interfaces found")
    ).

:- pred matching_interface_file(q_name::in, string::in) is semidet.

matching_interface_file(ModuleName, FileName) :-
    filename_extension(interface_extension, FileName, FileNameBase),
    strip_file_name_punctuation(q_name_to_string(ModuleName)) =
        strip_file_name_punctuation(FileNameBase).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
