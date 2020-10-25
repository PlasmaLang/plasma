%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module pz.
%
% Low level plasma data structure.
%
% Copyright (C) 2015-2020 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
%-----------------------------------------------------------------------%

:- interface.

:- import_module list.

:- include_module pz.code.
:- include_module pz.pretty.
:- include_module pz.pz_ds.
:- include_module pz.link.
:- include_module pz.read.
:- include_module pz.write.

:- import_module pz.pz_ds.

%-----------------------------------------------------------------------%
%
% Common definitions
%

:- type pz_file_type
    --->    pzft_program
    ;       pzft_library
    ;       pzft_object.

% TODO: Separate structs into new entries.  Allow arrays of structs.
% TODO: Allow data to reference code.
% TODO: Re-arrange data and value types to better match the on-disk format.

:- type pz_struct
    --->    pz_struct(list(pz_width)).

    % A data type.
    %
    % Note that types aren't defined recursively.  All PZ cares about is the
    % width and padding of data, so we don't need recursive definitions.
    % There is one place where recursive definitions would be useful but the
    % costs outweigh the benefit, and the workaround is simple.
    %
:- type pz_data_type
    --->    type_array(
                pza_width       :: pz_width,
                pza_num_items   :: int
            )
    ;       type_struct(
                pzs_id          :: pzs_id
            ).

    % A static data entry
    %
:- type pz_data
    --->    pz_data(pz_data_type, list(pz_data_value)).

:- type pz_closure
    --->    pz_closure(pzp_id, pzd_id).

%
% PZ isn't typed like a high level language.  The only things PZ needs to
% know are data widths (for alignment and padding).
%

:- type pz_width
    --->    pzw_8
    ;       pzw_16
    ;       pzw_32
    ;       pzw_64
    ;       pzw_fast
    ;       pzw_ptr.

:- type pz_data_value
    --->    pzv_num(int)
    ;       pzv_data(pzd_id)
    ;       pzv_import(pzi_id)
    ;       pzv_closure(pzc_id).

%-----------------------------------------------------------------------%

:- type pz_named_struct
    --->    pz_named_struct(
                pzs_name        :: string,
                pzs_struct      :: pz_struct
            ).

%-----------------------------------------------------------------------%

:- func pz_encode_string(string) = pz_data.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module string.

:- include_module pz.bytecode.
:- include_module pz.format.

%-----------------------------------------------------------------------%

:- pragma foreign_decl("C",
"
#include ""pz_common.h""
#include ""pz_format.h""
").

:- pragma foreign_enum("C", pz_width/0, [
    pzw_8       - "PZW_8",
    pzw_16      - "PZW_16",
    pzw_32      - "PZW_32",
    pzw_64      - "PZW_64",
    pzw_fast    - "PZW_FAST",
    pzw_ptr     - "PZW_PTR"
]).

%-----------------------------------------------------------------------%

pz_encode_string(String) = Data :-
    Values = map(func(C) = pzv_num(to_int(C)),
        to_char_list(String)) ++ [pzv_num(0)],
    Data = pz_data(type_array(pzw_8, length(Values)), Values).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
