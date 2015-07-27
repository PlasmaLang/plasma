%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module pz.
%
% Low level plasma data structure.
%
% Copyright (C) 2015 Paul Bone
% All rights reserved
%
%-----------------------------------------------------------------------%

:- interface.

:- import_module int.
:- import_module list.
:- import_module string.

:- include_module pz.read.
:- include_module pz.write.

%-----------------------------------------------------------------------%

:- type pz.

%-----------------------------------------------------------------------%
%
% Static data.
%

    % A data type.
    %
    % Note that types aren't defined recursively.  All PZ cares about is the
    % width and padding of data, so we don't need recursive definitions.
    % There is one place where recursive definitions would be useful but the
    % costs outweigh the benefit, and the workaround is simple.
    %
:- type pz_data_type
    --->    type_data(pz_data_width)
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
:- type pz_data_width
    --->    w8
    ;       w16
    ;       w32
    ;       w64
    ;       ptr.

:- type pz_value
    --->    pzv_num(int)
    ;       pzv_sequence(list(int)).

%-----------------------------------------------------------------------%

:- func init_pz = pz.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

:- import_module array.
:- import_module map.

:- include_module pz.bytecode.

%-----------------------------------------------------------------------%

:- type pz
    ---> pz(
        pz_strings      :: map(string, pz_entry_id),
        pz_data         :: map(id, pz_data_type),
        pz_procs        :: map(id, pz_proc)
    ).

:- type id == int.

:- type pz_entry_id
    --->    pz_proc_id(id).

%-----------------------------------------------------------------------%

:- type pz_proc
    --->    pz_proc.

%-----------------------------------------------------------------------%

init_pz = pz(init, init, init).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
