%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module pz.bytecode.
%
% Common code for reading or writing PZ bytecode.
%
% Copyright (C) 2015 Paul Bone
% Distributed under the terms of the GPLv2 see ../LICENSE.tools
%
%-----------------------------------------------------------------------%

:- interface.

:- import_module int.
:- import_module string.

%-----------------------------------------------------------------------%

:- type plz0_tag
    --->    pt_magic.

:- pred plz0_tag_id(plz0_tag, int).
:- mode plz0_tag_id(in, out) is det.
% :- mode plz0_tag_id(out, in) is semidet.

:- func plz0_id_string = string.

:- func plz0_version = int.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

:- import_module list.

plz0_tag_id(pt_magic,       0x505A).

%-----------------------------------------------------------------------%

plz0_id_string =
    format("Plasma abstract machine bytecode version %d", [i(plz0_version)]).

%-----------------------------------------------------------------------%

plz0_version = 0.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
