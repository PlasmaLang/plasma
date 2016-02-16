%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module pz.
%
% Low level plasma data structure.
%
% Copyright (C) 2015-2016 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
%-----------------------------------------------------------------------%

:- interface.

:- import_module assoc_list.
:- import_module cord.
:- import_module int.
:- import_module list.
:- import_module maybe.
:- import_module string.

:- include_module pz.code.
:- include_module pz.pretty.
:- include_module pz.read.
:- include_module pz.write.

:- import_module asm_error.
:- import_module common_types.
:- import_module pz.code.
:- import_module result.

%-----------------------------------------------------------------------%
%
% Static data.
%

% TODO: Separate structs into new entries.  Allow arrays of structs.
% TODO: Allow data to reference code.
% TODO: Re-arrange data and value types to better match the on-disk format.

:- type pz_struct
    --->    pz_struct(list(pz_data_width)).

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
    ;       type_struct(pzs_id).

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
:- type pz_data_width
    --->    w8
    ;       w16
    ;       w32
    ;       w64
    ;       w_fast  % A word of the fastest width on this platform.
    ;       w_ptr   % An word the same width as a pointer.
    ;       ptr.    % A pointer, this differes from pointer-sized words in
                    % that the GC will trace it.

:- type pz_data_value
    --->    pzv_num(int)
    ;       pzv_sequence(list(int))
    ;       pzv_data(pzd_id).

%-----------------------------------------------------------------------%

    % Procedure ID
    %
:- type pzp_id.

:- func pzp_id_get_num(pz, pzp_id) = int.

    % Structure ID
    %
:- type pzs_id.

:- func pzs_id_get_num(pz, pzs_id) = int.

    % Data ID
    %
:- type pzd_id.

:- func pzd_id_get_num(pz, pzd_id) = int.

%-----------------------------------------------------------------------%

:- type pz.

%-----------------------------------------------------------------------%

:- func init_pz = pz.

%-----------------------------------------------------------------------%

:- pred pz_set_entry_proc(pzp_id::in, pz::in, pz::out) is det.

:- func pz_get_maybe_entry_proc(pz) = maybe(pzp_id).

%-----------------------------------------------------------------------%

:- func pz_get_structs(pz) = assoc_list(pzs_id, pz_struct).

:- func pz_lookup_struct(pz, pzs_id) = pz_struct.

%-----------------------------------------------------------------------%

:- pred pz_new_proc_id(imported::in, pzp_id::out, pz::in, pz::out) is det.

:- pred pz_add_proc(pzp_id::in, pz_proc::in, pz::in, pz::out) is det.

:- func pz_get_procs(pz) = assoc_list(pzp_id, pz_proc).

:- func pz_get_local_procs(pz) = assoc_list(pzp_id, pz_proc).

:- func pz_get_imported_procs(pz) = assoc_list(pzp_id, pz_proc).

%-----------------------------------------------------------------------%

:- pred pz_new_data_id(pzd_id::out, pz::in, pz::out) is det.

:- pred pz_add_data(pzd_id::in, pz_data::in, pz::in, pz::out) is det.

%-----------------------------------------------------------------------%

:- func pz_get_data_items(pz) = assoc_list(pzd_id, pz_data).

%-----------------------------------------------------------------------%

:- type pz_data
    --->    pz_data(pz_data_type, pz_data_value).

%-----------------------------------------------------------------------%

:- pred pz_add_errors(cord(error(asm_error))::in, pz::in, pz::out) is det.

:- func pz_get_errors(pz) = cord(error(asm_error)).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

:- import_module array.
:- import_module map.
:- import_module pair.

:- include_module pz.bytecode.

%-----------------------------------------------------------------------%

:- type pzp_id
    ---> pzp_id_local(pzpl_id_num :: int)
    ;    pzp_id_imported(pzpi_id_num :: int).

pzp_id_get_num(PZ, pzp_id_local(Num)) = PZ ^ pz_next_imported_proc_id + Num.
pzp_id_get_num(_, pzp_id_imported(Num)) = Num.

%-----------------------------------------------------------------------%

:- type pzs_id
    ---> pzs_id(pzs_id_num  :: int).

pzs_id_get_num(_, pzs_id(Num)) = Num.

%-----------------------------------------------------------------------%

:- type pzd_id
    ---> pzd_id(pzd_id_num  :: int).

pzd_id_get_num(_, pzd_id(Num)) = Num.

%-----------------------------------------------------------------------%

:- type pz
    ---> pz(
        pz_structs                  :: map(pzs_id, pz_struct),
        pz_procs                    :: map(pzp_id, pz_proc),
        pz_next_local_proc_id       :: int,
        pz_next_imported_proc_id    :: int,
        pz_maybe_entry              :: maybe(pzp_id),

        pz_data                     :: map(pzd_id, pz_data),
        pz_next_data_id             :: pzd_id,

        pz_errors                   :: cord(error(asm_error))
    ).

%-----------------------------------------------------------------------%

init_pz = pz(init, init, 0, 0, no, init, pzd_id(0), init).

%-----------------------------------------------------------------------%

pz_get_structs(PZ) = to_assoc_list(PZ ^ pz_structs).

pz_lookup_struct(PZ, PZSId) = map.lookup(PZ ^ pz_structs, PZSId).

%-----------------------------------------------------------------------%

pz_new_proc_id(i_local, pzp_id_local(NewID), !PZ) :-
    NewID = !.PZ ^ pz_next_local_proc_id,
    !PZ ^ pz_next_local_proc_id := NewID + 1.
pz_new_proc_id(i_imported, pzp_id_imported(NewID), !PZ) :-
    NewID = !.PZ ^ pz_next_imported_proc_id,
    !PZ ^ pz_next_imported_proc_id := NewID + 1.

pz_add_proc(ProcID, Proc, !PZ) :-
    Procs0 = !.PZ ^ pz_procs,
    map.det_insert(ProcID, Proc, Procs0, Procs),
    !PZ ^ pz_procs := Procs.

pz_get_procs(PZ) = to_assoc_list(PZ ^ pz_procs).

pz_get_local_procs(PZ) =
    filter((pred((pzp_id_local(_) - _)::in) is semidet), pz_get_procs(PZ)).

pz_get_imported_procs(PZ) =
    filter((pred((pzp_id_imported(_) - _)::in) is semidet),
        pz_get_procs(PZ)).

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

pz_add_errors(Errors, !PZ) :-
    !PZ ^ pz_errors := !.PZ ^ pz_errors ++ Errors.

pz_get_errors(PZ) = PZ ^ pz_errors.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
