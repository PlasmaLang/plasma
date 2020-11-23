%-----------------------------------------------------------------------%
% Plasma constants
% vim: ts=4 sw=4 et
%
% Copyright (C) 2020 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
% This module provides constants used in the compiler and other tools.
%
%-----------------------------------------------------------------------%
:- module constant.
%-----------------------------------------------------------------------%
:- interface.

:- func source_extension = string.
:- func interface_extension = string.
:- func pz_text_extension = string.
:- func output_extension = string.
:- func library_extension = string.

:- func build_directory = string.
:- func rules_file = string.
:- func rules_file_no_directory = string.
:- func build_file = string.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
:- implementation.

:- import_module string.

%-----------------------------------------------------------------------%

source_extension = ".p".
interface_extension = ".pi".
pz_text_extension = ".pzt".
output_extension = ".pzo".
library_extension = ".pz".

build_directory = "_build".
rules_file = build_directory ++ "/" ++ rules_file_no_directory.
rules_file_no_directory = "rules.ninja".
% Ninja requires it uses this name.
build_file = build_directory ++ "/build.ninja".

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
