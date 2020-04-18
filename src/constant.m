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
:- func pz_text_extension = string.
:- func output_extension = string.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
:- implementation.

source_extension = ".p".
pz_text_extension = ".pzt".
output_extension = ".pzo".

