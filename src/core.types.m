%-----------------------------------------------------------------------%
% Plasma types representation
% vim: ts=4 sw=4 et
%
% Copyright (C) 2015-2018, 2020-2022 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
%-----------------------------------------------------------------------%
:- module core.types.
%-----------------------------------------------------------------------%

:- interface.

:- import_module context.

%-----------------------------------------------------------------------%

:- type type_
    --->    builtin_type(builtin_type)
    ;       func_type(
                % Arg types
                list(type_),
                % Return types
                list(type_),
                % Uses
                set(resource_id),
                % Observes
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

    % Return all the resources that appear in this type.
    %
:- func type_get_resources(type_) = set(resource_id).

%-----------------------------------------------------------------------%

:- type user_type.

:- func type_init(q_name, list(string), list(ctor_id), sharing_type,
    imported, context) = user_type.

:- func type_init_abstract(q_name, arity, context) = user_type.

:- func utype_get_name(user_type) = q_name.

:- func utype_get_params(user_type) = maybe(list(string)).

:- func utype_get_ctors(user_type) = maybe(list(ctor_id)).

:- func utype_get_sharing(user_type) = sharing_type.

:- func utype_get_imported(user_type) = imported.

:- func utype_get_arity(user_type) = arity.

:- func utype_get_context(user_type) = context.

:- func utype_get_resources(core, user_type) = set(resource_id).

:- func utype_get_types(core, user_type) = set(type_id).

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

type_get_resources(builtin_type(_)) = set.init.
type_get_resources(func_type(_, _, Uses, Observes)) = Uses `set.union`
    Observes.
type_get_resources(type_variable(_)) = set.init.
type_get_resources(type_ref(_, Args)) = set.union_list(
    map(type_get_resources, Args)).

%-----------------------------------------------------------------------%

:- type user_type
    --->    user_type(
                t_symbol        :: q_name,
                t_params        :: list(string),
                t_ctors         :: list(ctor_id),
                t_sharing       :: sharing_type,
                t_imported      :: imported,
                t_context       :: context
            )
    ;       abstract_type(
                at_symbol       :: q_name,
                at_arity        :: arity,
                at_context      :: context
            ).

type_init(Name, Params, Ctors, Sharing, Imported, Context) =
    user_type(Name, Params, Ctors, Sharing, Imported, Context).

type_init_abstract(Name, Arity, Context) = abstract_type(Name, Arity, Context).

utype_get_name(user_type(S, _, _, _, _, _)) = S.
utype_get_name(abstract_type(S, _, _)) = S.

utype_get_params(user_type(_, Params, _, _, _, _)) = yes(Params).
utype_get_params(abstract_type(_, _, _)) = no.

utype_get_ctors(Type) =
    ( if Ctors = Type ^ t_ctors then
        yes(Ctors)
    else
        no
    ).

utype_get_sharing(user_type(_, _, _, Sharing, _, _)) = Sharing.
utype_get_sharing(abstract_type(_, _, _)) = st_private.

utype_get_imported(user_type(_, _, _, _, Imported, _)) = Imported.
utype_get_imported(abstract_type(_, _, _)) = i_imported.

utype_get_arity(user_type(_, Params, _, _, _, _)) = arity(length(Params)).
utype_get_arity(abstract_type(_, Arity, _)) = Arity.

utype_get_context(user_type(_, _, _, _, _, Context)) = Context.
utype_get_context(abstract_type(_, _, Context)) = Context.

%-----------------------------------------------------------------------%

utype_get_resources(Core, user_type(_, _, Ctors, _, _, _)) =
    union_list(map(ctor_get_resources(Core), Ctors)).
utype_get_resources(_, abstract_type(_, _, _)) = set.init.

:- func ctor_get_resources(core, ctor_id) = set(resource_id).

ctor_get_resources(Core, CtorId) = Res :-
    core_get_constructor_det(Core, CtorId, Ctor),
    Ctor = constructor(_, _, Fields),
    Res = union_list(map(field_get_resources, Fields)).

:- func field_get_resources(type_field) = set(resource_id).

field_get_resources(type_field(_, Type)) =
    type_get_resources(Type).

%-----------------------------------------------------------------------%

utype_get_types(Core, user_type(_, _, Ctors, _, _, _)) =
    union_list(map(ctor_get_types(Core), Ctors)).
utype_get_types(_, abstract_type(_, _, _)) = set.init.

:- func ctor_get_types(core, ctor_id) = set(type_id).

ctor_get_types(Core, CtorId) = Types :-
    core_get_constructor_det(Core, CtorId, Ctor),
    Ctor = constructor(_, _, Fields),
    Types = union_list(map(field_get_types, Fields)).

:- func field_get_types(type_field) = set(type_id).

field_get_types(type_field(_, TypeExpr)) =
    type_expr_get_types(TypeExpr).

:- func type_expr_get_types(type_) = set(type_id).

type_expr_get_types(builtin_type(_)) = set.init.
type_expr_get_types(func_type(Params, Returns, _, _)) =
    union_list(map(type_expr_get_types, Params)) `union`
    union_list(map(type_expr_get_types, Returns)).
type_expr_get_types(type_variable(_)) = set.init.
type_expr_get_types(type_ref(TypeId, Args)) =
    set.make_singleton_set(TypeId) `union`
        union_list(map(type_expr_get_types, Args)).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
