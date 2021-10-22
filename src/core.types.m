%-----------------------------------------------------------------------%
% Plasma types representation
% vim: ts=4 sw=4 et
%
% Copyright (C) 2015-2018, 2020-2021 Plasma Team
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
    ;       codepoint
    ;       string
    ;       string_pos.

:- pred builtin_type_name(builtin_type, nq_name).
:- mode builtin_type_name(in, out) is det.
:- mode builtin_type_name(out, in) is semidet.

    % All types have constructors, but some types don't have constructor
    % IDs (Strings, Ints, etc) and some don't provide them (abstract types).
    %
:- func type_get_ctors(core, type_) = maybe(list(ctor_id)).

%-----------------------------------------------------------------------%

:- type user_type.

:- func type_init(q_name, list(string), list(ctor_id), sharing_type) =
    user_type.

:- func type_init_abstract(q_name, arity) = user_type.

:- func utype_get_name(user_type) = q_name.

:- func utype_get_params(user_type) = maybe(list(string)).

:- func utype_get_ctors(user_type) = maybe(list(ctor_id)).

:- func utype_get_sharing(user_type) = sharing_type.

:- func utype_get_arity(user_type) = arity.

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

:- pragma promise_equivalent_clauses(builtin_type_name/2).

builtin_type_name(Type::in, nq_name_det(String)::out) :-
    builtin_type_name_2(Type, String).
builtin_type_name(Type::out, Name::in) :-
    builtin_type_name_2(Type, nq_name_to_string(Name)).

:- pred builtin_type_name_2(builtin_type, string).
:- mode builtin_type_name_2(in, out) is det.
:- mode builtin_type_name_2(out, in) is semidet.

builtin_type_name_2(int,        "Int").
builtin_type_name_2(codepoint,  "CodePoint").
builtin_type_name_2(string,     "String").
builtin_type_name_2(string_pos, "StringPos").

%-----------------------------------------------------------------------%

type_get_ctors(_, builtin_type(_)) = no.
type_get_ctors(_, func_type(_, _, _, _)) = no.
type_get_ctors(_, type_variable(_)) = no.
type_get_ctors(Core, type_ref(TypeId, _)) =
    utype_get_ctors(core_get_type(Core, TypeId)).

%-----------------------------------------------------------------------%

:- type user_type
    --->    user_type(
                t_symbol        :: q_name,
                t_params        :: list(string),
                t_ctors         :: list(ctor_id),
                t_sharing       :: sharing_type
            )
    ;       abstract_type(
                at_symbol       :: q_name,
                at_arity        :: arity
            ).

type_init(Name, Params, Ctors, Sharing) =
    user_type(Name, Params, Ctors, Sharing).

type_init_abstract(Name, Arity) = abstract_type(Name, Arity).

utype_get_name(user_type(S, _, _, _)) = S.
utype_get_name(abstract_type(S, _)) = S.

utype_get_params(user_type(_, Params, _, _)) = yes(Params).
utype_get_params(abstract_type(_, _)) = no.

utype_get_ctors(Type) =
    ( if Ctors = Type ^ t_ctors then
        yes(Ctors)
    else
        no
    ).

utype_get_sharing(user_type(_, _, _, Sharing)) = Sharing.
utype_get_sharing(abstract_type(_, _)) = st_private.

utype_get_arity(user_type(_, Params, _, _)) = arity(length(Params)).
utype_get_arity(abstract_type(_, Arity)) = Arity.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
