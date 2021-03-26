%-----------------------------------------------------------------------%
% Plasma constants
% vim: ts=4 sw=4 et
%
% Copyright (C) 2020-2021 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
% This module provides constants used in the compiler and other tools.
%
%-----------------------------------------------------------------------%
:- module constant.
%-----------------------------------------------------------------------%
:- interface.

:- import_module io.

:- func source_extension = string.
:- func interface_extension = string.
:- func dep_info_extension = string.
:- func pz_text_extension = string.
:- func output_extension = string.
:- func library_extension = string.

:- func build_file = string.
:- func build_directory = string.
:- func ninja_rules_file = string.
:- func ninja_rules_file_no_directory = string.
:- func ninja_build_file = string.
:- func import_whitelist_file = string.
:- func import_whitelist_file_no_directroy = string.

%-----------------------------------------------------------------------%

    % Print the version message.
    %
:- pred version(string::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
:- implementation.

:- import_module list.
:- import_module string.

%-----------------------------------------------------------------------%

source_extension = ".p".
interface_extension = ".pi".
dep_info_extension = ".dep".
pz_text_extension = ".pzt".
output_extension = ".pzo".
library_extension = ".pz".

build_file = "BUILD.plz".
build_directory = "_build".
ninja_rules_file = build_directory ++ "/" ++ ninja_rules_file_no_directory.
ninja_rules_file_no_directory = "rules.ninja".
% Ninja requires it uses this name.
ninja_build_file = build_directory ++ "/build.ninja".
import_whitelist_file = build_directory ++ "/" ++
    import_whitelist_file_no_directroy.
import_whitelist_file_no_directroy = "include_whitelist.txt".

%-----------------------------------------------------------------------%

version(Name, !IO) :-
    io.format("%s, development version\n", [s(Name)], !IO),
    io.write_string("https://plasmalang.org\n", !IO),
    io.write_string("Copyright (C) 2015-2021 The Plasma Team\n", !IO),
    io.write_string("Distributed under the MIT License\n", !IO).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
