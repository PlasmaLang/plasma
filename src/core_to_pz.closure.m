%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module core_to_pz.closure.
%
% Copyright (C) Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
% Structures and code to help construct closures.
%
%-----------------------------------------------------------------------%
:- interface.

%-----------------------------------------------------------------------%

:- type closure_builder.

:- func closure_builder_init(pzs_id) = closure_builder.

:- pred closure_add_field(pz_data_value::in, field_num::out,
    closure_builder::in, closure_builder::out) is det.

    % Create the environment for the closure.
    %
:- pred closure_finalize_data(closure_builder::in, pzd_id::out,
    pz::in, pz::out) is det.

:- func closure_get_struct(closure_builder) = pzs_id.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
:- implementation.

:- import_module int.

%-----------------------------------------------------------------------%

:- type closure_builder
    --->    closure_builder(
                cb_struct           :: pzs_id,
                cb_rev_values       :: list(pz_data_value),
                cb_next_field_num   :: int
            ).

closure_builder_init(Struct) = closure_builder(Struct, [], 1).

%-----------------------------------------------------------------------%

closure_add_field(DataValue, field_num(FieldNum),
    closure_builder(Struct, DataValues,               FieldNum),
    closure_builder(Struct, [DataValue | DataValues], FieldNum + 1)).

%-----------------------------------------------------------------------%

closure_finalize_data(CB, DataId, !PZ) :-
    Values = reverse(CB ^ cb_rev_values),
    Types = duplicate(length(Values), pzw_ptr),
    pz_add_struct(CB ^ cb_struct, pz_struct(Types), !PZ),
    pz_new_data_id(DataId, !PZ),
    pz_add_data(DataId, pz_data(type_struct(CB ^ cb_struct), Values), !PZ).

%-----------------------------------------------------------------------%

closure_get_struct(CB) = CB ^ cb_struct.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
