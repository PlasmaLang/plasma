%-----------------------------------------------------------------------%
% Plasma types representation
% vim: ts=4 sw=4 et
%
% Copyright (C) 2015-2018, 2020 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
%-----------------------------------------------------------------------%
:- module core.types.
%-----------------------------------------------------------------------%

:- interface.

%-----------------------------------------------------------------------%

:- type type_
    --->    builtin_type(builtin_type)
    ;       func_type(
                list(type_),
                list(type_),
                set(resource_id),
                set(resource_id)
            )
    ;       type_variable(type_var)
    ;       type_ref(type_id, list(type_)).

:- type type_var == string.

    % XXX: Should probably handle type var renaming/remapping.
    %
:- pred types_equal_except_resources(type_::in, type_::in) is semidet.

:- type builtin_type
    --->    int
            % string may not always be builtin.
    ;       string.

:- pred builtin_type_name(builtin_type, string).
:- mode builtin_type_name(in, out) is det.
:- mode builtin_type_name(out, in) is semidet.

%-----------------------------------------------------------------------%

:- type user_type.

:- func init(q_name, list(string), list(ctor_id)) = user_type.

:- func type_get_name(user_type) = q_name.

:- func type_get_ctors(user_type) = list(ctor_id).

%-----------------------------------------------------------------------%

:- type constructor
    --->    constructor(
                c_name          :: q_name,
                c_params        :: list(type_var),
                c_fields        :: list(type_field)
            ).

:- type type_field
    --->    type_field(
                tf_name         :: q_name,
                tf_type         :: type_
            ).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
:- implementation.

%-----------------------------------------------------------------------%

types_equal_except_resources(T1, T2) :-
    require_complete_switch [T1]
    ( T1 = builtin_type(B),
        T2 = builtin_type(B)
    ; T1 = type_variable(V),
        T2 = type_variable(V)
    ; T1 = type_ref(TypeId, ArgsA),
        T2 = type_ref(TypeId, ArgsB),
        all_true_corresponding(types_equal_except_resources, ArgsA, ArgsB)
    ; T1 = func_type(ArgsA, OutA, _, _),
        T2 = func_type(ArgsB, OutB, _, _),
        all_true_corresponding(types_equal_except_resources, ArgsA, ArgsB),
        all_true_corresponding(types_equal_except_resources, OutA, OutB)
    ).

%-----------------------------------------------------------------------%

builtin_type_name(int,      "Int").
builtin_type_name(string,   "String").

%-----------------------------------------------------------------------%

:- type user_type
    --->    user_type(
                t_symbol        :: q_name,
                t_params        :: list(string),
                t_ctors         :: list(ctor_id)
            ).

init(Name, Params, Ctors) = user_type(Name, Params, Ctors).

type_get_name(Type) = Type ^ t_symbol.

type_get_ctors(Type) = Type ^ t_ctors.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
