%-----------------------------------------------------------------------%
% Plasma assembler
% vim: ts=4 sw=4 et
%
% Copyright (C) 2015 Paul Bone
% All rights reserved
%
% This program assembles the pz intermediate representation.
%
%-----------------------------------------------------------------------%
:- module pzasm.
%-----------------------------------------------------------------------%

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module list.
:- import_module maybe.
:- import_module string.

:- import_module pz.

%-----------------------------------------------------------------------%

main(!IO) :-
    io.command_line_arguments(Args, !IO),
    foldl(assemble, Args, !IO).

:- pred assemble(string::in, io::di, io::uo) is det.

assemble(Filename, !IO) :-
    read_pz(Filename, _, !IO).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
