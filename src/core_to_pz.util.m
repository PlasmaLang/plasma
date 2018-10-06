%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module core_to_pz.util.
%
% Copyright (C) 2015-2018 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
% Plasma core to pz conversion - utility code
%
%-----------------------------------------------------------------------%
:- interface.

:- import_module cord.

%-----------------------------------------------------------------------%

:- type pz_proc_or_import
    --->    pzp(pzp_id)
    ;       pzi(pzi_id).

%-----------------------------------------------------------------------%

:- func gen_load_named(pzs_id, pzi_id) = cord(pz_instr_obj).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
:- implementation.

%-----------------------------------------------------------------------%

gen_load_named(ModStructId, ImportId) =
    from_list([
        pzio_instr(pzi_get_env),
        pzio_instr(pzi_load(ModStructId, closure_parent_field, pzw_ptr)),
        pzio_instr(pzi_drop),
        pzio_instr(pzi_load_named(ImportId, pzw_ptr)),
        pzio_instr(pzi_drop)
    ]).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
