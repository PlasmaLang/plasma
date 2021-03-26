%-----------------------------------------------------------------------%
% Utility code
% vim: ts=4 sw=4 et
%
% Copyright (C) 2015-2021 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
%-----------------------------------------------------------------------%
:- module util.

:- interface.

:- import_module io.
:- import_module string.

:- include_module util.exception.
:- include_module util.io.
:- include_module util.log.
:- include_module util.mercury.
:- include_module util.path.
:- include_module util.pretty.
:- include_module util.pretty_old.
:- include_module util.result.
:- include_module util.string.

:- pred version(string::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

:- import_module list.

%-----------------------------------------------------------------------%

version(Name, !IO) :-
    io.format("%s, development version\n", [s(Name)], !IO),
    io.write_string("https://plasmalang.org\n", !IO),
    io.write_string("Copyright (C) 2015-2021 The Plasma Team\n", !IO),
    io.write_string("Distributed under the MIT License\n", !IO).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
