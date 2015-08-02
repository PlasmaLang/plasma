%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module pz.code.
%
% PZ representation of code.
%
% Copyright (C) 2015 Paul Bone
% Distributed under the terms of the GPLv2 see ../LICENSE.tools
%
%-----------------------------------------------------------------------%
:- interface.

:- import_module list.

:- type pz_proc
    --->    pz_proc(
                pz_before       :: list(pz_data_width),
                pz_after        :: list(pz_data_width),
                pz_instrs       :: list(pz_instr)
            ).

:- type pz_instr
    --->    pzi_load_immediate_8(int)
    ;       pzi_load_immediate_16(int)
    ;       pzi_load_immediate_32(int)
    ;       pzi_load_immediate_64(int)
    ;       pzi_load_immediate_ptr(int)
    ;       pzi_load_data(pzd_id)
    ;       pzi_call(pzp_id).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
