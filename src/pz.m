%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module pz.
%
% Low level plasma data structure.
%
% Copyright (C) 2015 Paul Bone
% Distributed under the terms of the GPLv2 see ../LICENSE.tools
%
%-----------------------------------------------------------------------%

:- interface.

:- import_module assoc_list.
:- import_module int.
:- import_module list.
:- import_module maybe.
:- import_module string.

:- include_module pz.code.
:- include_module pz.read.
:- include_module pz.write.

:- import_module pz.code.

%-----------------------------------------------------------------------%
%
% Static data.
%

% TODO: Seperate structs into new entries.  Allow arrays of structs.
% TODO: Allow data to reference code.
% TODO: Re-arrange data and value types to better match the on-disk format.

    % A data type.
    %
    % Note that types aren't defined recursively.  All PZ cares about is the
    % width and padding of data, so we don't need recursive definitions.
    % There is one place where recursive definitions would be useful but the
    % costs outweigh the benefit, and the workaround is simple.
    %
:- type pz_data_type
    --->    type_basic(pz_data_width)
    ;       type_array(pz_data_width)
    ;       type_struct(list(pz_data_width)).

%-----------------------------------------------------------------------%
%
% Common definitions
%

%
% PZ isn't typed like a high level language.  The only things PZ needs to
% know are data widths (for alignment and padding), whether something is a
% pointer or not (for GC) and whether something is a float (for register
% type, NIY).
%

    % Width of "atomic" data.
    %
    % TODO: I need to add a value that's "the same width as a pointer but
    % not a pointer' the difference will be important for GC.
    %
:- type pz_data_width
    --->    w8
    ;       w16
    ;       w32
    ;       w64
    ;       ptr.

:- type pz_data_value
    --->    pzv_num(int)
    ;       pzv_sequence(list(int))
    ;       pzv_data(pzd_id).

%-----------------------------------------------------------------------%

:- type pz.

:- type pzt_id ---> pzv_id(pzt_id_num :: int).
:- type pzd_id ---> pzd_id(pzd_id_num :: int).
:- type pzp_id ---> pzp_id(pzp_id_num :: int).

%-----------------------------------------------------------------------%

:- func init_pz = pz.

%-----------------------------------------------------------------------%

:- pred pz_set_entry_proc(pzp_id::in, pz::in, pz::out) is det.

:- func pz_get_maybe_entry_proc(pz) = maybe(pzp_id).

%-----------------------------------------------------------------------%

:- pred pz_new_proc_id(pzp_id::out, pz::in, pz::out) is det.

:- pred pz_add_proc(pzp_id::in, pz_proc::in, pz::in, pz::out) is det.

%-----------------------------------------------------------------------%

:- pred pz_new_data_id(pzd_id::out, pz::in, pz::out) is det.

:- pred pz_add_data(pzd_id::in, pz_data::in, pz::in, pz::out) is det.

%-----------------------------------------------------------------------%

:- func pz_get_data_items(pz) = assoc_list(pzd_id, pz_data).

%-----------------------------------------------------------------------%

:- type pz_data
    --->    pz_data(pz_data_type, pz_data_value).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

:- import_module array.
:- import_module map.

:- include_module pz.bytecode.

%-----------------------------------------------------------------------%

:- type pz
    ---> pz(
%        pz_data_types       :: map(pzt_id, pz_data_type),
%        pz_data_types       :: map(pzd_id, pz_value),
        pz_procs            :: map(pzp_id, pz_proc),
        pz_next_proc_id     :: pzp_id,
        pz_maybe_entry      :: maybe(pzp_id),

        pz_data             :: map(pzd_id, pz_data),
        pz_next_data_id     :: pzd_id
    ).

%-----------------------------------------------------------------------%

init_pz = pz(init, pzp_id(0), no, init, pzd_id(0)).

%-----------------------------------------------------------------------%

pz_new_proc_id(NewID, !PZ) :-
    NewID = !.PZ ^ pz_next_proc_id,
    !PZ ^ pz_next_proc_id := pzp_id(NewID ^ pzp_id_num + 1).

pz_add_proc(ProcID, Proc, !PZ) :-
    Procs0 = !.PZ ^ pz_procs,
    map.det_insert(ProcID, Proc, Procs0, Procs),
    !PZ ^ pz_procs := Procs.

%-----------------------------------------------------------------------%

pz_set_entry_proc(ProcID, !PZ) :-
    !PZ ^ pz_maybe_entry := yes(ProcID).

pz_get_maybe_entry_proc(PZ) = PZ ^ pz_maybe_entry.

%-----------------------------------------------------------------------%

pz_new_data_id(NewID, !PZ) :-
    NewID = !.PZ ^ pz_next_data_id,
    !PZ ^ pz_next_data_id := pzd_id(NewID ^ pzd_id_num + 1).

pz_add_data(DataID, Data, !PZ) :-
    Datas0 = !.PZ ^ pz_data,
    map.det_insert(DataID, Data, Datas0, Datas),
    !PZ ^ pz_data := Datas.

%-----------------------------------------------------------------------%

pz_get_data_items(PZ) = to_assoc_list(PZ ^ pz_data).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
