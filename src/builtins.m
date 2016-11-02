%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module builtins.
%
% Copyright (C) 2015-2016 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
% Plasma builtins.
%
%-----------------------------------------------------------------------%

:- interface.

:- import_module map.

:- import_module common_types.
:- import_module core.
:- import_module q_name.

:- type builtin_item
    --->    bi_func(func_id)
    ;       bi_ctor(ctor_id).

:- pred setup_builtins(map(q_name, builtin_item)::out,
    core::in, core::out) is det.

:- func builtin_module_name = q_name.

:- func builtin_add_int = q_name.
:- func builtin_sub_int = q_name.
:- func builtin_mul_int = q_name.
:- func builtin_div_int = q_name.
:- func builtin_mod_int = q_name.
:- func builtin_lshift_int = q_name.
:- func builtin_rshift_int = q_name.
:- func builtin_and_int = q_name.
:- func builtin_or_int = q_name.
:- func builtin_xor_int = q_name.
:- func builtin_gt_int = q_name.
:- func builtin_lt_int = q_name.
:- func builtin_gteq_int = q_name.
:- func builtin_lteq_int = q_name.
:- func builtin_eq_int = q_name.
:- func builtin_neq_int = q_name.
:- func builtin_and_bool = q_name.
:- func builtin_or_bool = q_name.
:- func builtin_concat_string = q_name.

% Unary operators.
:- func builtin_minus_int = q_name.
:- func builtin_comp_int = q_name.
:- func builtin_not_bool = q_name.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module require.
:- import_module set.
:- import_module string.

:- import_module context.
:- import_module core.function.
:- import_module core.types.

%-----------------------------------------------------------------------%

setup_builtins(!:Map, !Core) :-
    !:Map = init,
    setup_bool_builtins(BoolType, !Map, !Core),
    setup_int_builtins(BoolType, !Map, !Core),
    setup_misc_builtins(BoolType, !Map, !Core).

:- pred setup_bool_builtins(type_id::out, map(q_name, builtin_item)::in,
    map(q_name, builtin_item)::out, core::in, core::out) is det.

setup_bool_builtins(BoolId, !Map, !Core) :-
    core_allocate_type_id(BoolId, !Core),
    core_allocate_ctor_id(FalseId, !Core),
    core_allocate_ctor_id(TrueId, !Core),
    BoolTypeSet = make_singleton_set(BoolId),

    % NOTE: False is first so that it is allocated 0 for its tag, this will
    % make interoperability easier.
    core_set_type(BoolId,
        init(q_name_snoc(builtin_module_name, "Bool"),
            [FalseId, TrueId]),
        !Core),

    FalseName = q_name("False"),
    False = constructor(q_name_append(builtin_module_name, FalseName),
        BoolTypeSet),
    core_set_constructor(FalseId, False, !Core),
    det_insert(FalseName, bi_ctor(FalseId), !Map),

    TrueName = q_name("True"),
    True = constructor(q_name_append(builtin_module_name, TrueName),
        BoolTypeSet),
    core_set_constructor(TrueId, True, !Core),
    det_insert(TrueName, bi_ctor(TrueId), !Map),

    NotName = q_name("not_bool"),
    register_builtin_func(NotName,
        func_init(q_name_append(builtin_module_name, NotName), nil_context,
            s_private,
            [type_ref(BoolId)], [type_ref(BoolId)], init,
            init),
        _, !Map, !Core),

    foldl2(register_bool_biop(BoolId), [
        builtin_and_bool,
        builtin_or_bool], !Map, !Core).

:- pred register_bool_biop(type_id::in, q_name::in,
    map(q_name, builtin_item)::in, map(q_name, builtin_item)::out,
    core::in, core::out) is det.

register_bool_biop(BoolType, Name, !Map, !Core) :-
    FName = q_name_append(builtin_module_name, Name),
    register_builtin_func(Name,
        func_init(FName, nil_context, s_private,
            [type_ref(BoolType), type_ref(BoolType)],
            [type_ref(BoolType)],
            init, init),
        _, !Map, !Core).

:- pred setup_int_builtins(type_id::in, map(q_name, builtin_item)::in,
    map(q_name, builtin_item)::out, core::in, core::out) is det.

setup_int_builtins(BoolType, !Map, !Core) :-
    foldl2(register_int_biop, [
        builtin_add_int,
        builtin_sub_int,
        builtin_mul_int,
        builtin_div_int,
        builtin_mod_int,
        % TODO: make the number of bits to shift a single byte.
        builtin_lshift_int,
        builtin_rshift_int,
        builtin_and_int,
        builtin_or_int,
        builtin_xor_int], !Map, !Core),

    foldl2(register_int_comp(BoolType), [
        builtin_gt_int,
        builtin_lt_int,
        builtin_gteq_int,
        builtin_lteq_int,
        builtin_eq_int,
        builtin_neq_int], !Map, !Core),

    foldl2(register_int_uop, [
        builtin_minus_int,
        builtin_comp_int], !Map, !Core).

:- pred register_int_biop(q_name::in,
    map(q_name, builtin_item)::in, map(q_name, builtin_item)::out,
    core::in, core::out) is det.

register_int_biop(Name, !Map, !Core) :-
    FName = q_name_append(builtin_module_name, Name),
    register_builtin_func(Name,
        func_init(FName, nil_context, s_private,
            [builtin_type(int), builtin_type(int)],
            [builtin_type(int)],
            init, init),
        _, !Map, !Core).

:- pred register_int_comp(type_id::in, q_name::in,
    map(q_name, builtin_item)::in, map(q_name, builtin_item)::out,
    core::in, core::out) is det.

register_int_comp(BoolType, Name, !Map, !Core) :-
    FName = q_name_append(builtin_module_name, Name),
    register_builtin_func(Name,
        func_init(FName, nil_context, s_private,
            [builtin_type(int), builtin_type(int)],
            [type_ref(BoolType)],
            init, init),
        _, !Map, !Core).

:- pred register_int_uop(q_name::in,
    map(q_name, builtin_item)::in, map(q_name, builtin_item)::out,
    core::in, core::out) is det.

register_int_uop(Name, !Map, !Core) :-
    FName = q_name_append(builtin_module_name, Name),
    register_builtin_func(Name,
        func_init(FName, nil_context, s_private,
            [builtin_type(int)], [builtin_type(int)],
            init, init),
        _, !Map, !Core).

:- pred setup_misc_builtins(type_id::in, map(q_name, builtin_item)::in,
    map(q_name, builtin_item)::out, core::in, core::out) is det.

setup_misc_builtins(BoolType, !Map, !Core) :-
    PrintName = q_name_snoc(builtin_module_name, "print"),
    register_builtin_func(q_name("print"),
        func_init(PrintName, nil_context, s_private,
            [builtin_type(string)], [], set([r_io]), init),
        _, !Map, !Core),


    IntToStringName = q_name_snoc(builtin_module_name, "int_to_string"),
    register_builtin_func(q_name("int_to_string"),
        func_init(IntToStringName, nil_context, s_private,
            [builtin_type(int)], [builtin_type(string)], init, init),
        _, !Map, !Core),

    BoolToStringName = q_name_snoc(builtin_module_name, "bool_to_string"),
    register_builtin_func(q_name("bool_to_string"),
        func_init(BoolToStringName, nil_context, s_private,
            [type_ref(BoolType)], [builtin_type(string)], init, init),
        _, !Map, !Core),

    FreeName = q_name_snoc(builtin_module_name, "free"),
    register_builtin_func(q_name("free"),
        func_init(FreeName, nil_context, s_private,
            [builtin_type(string)], [], set([r_io]), init),
        _, !Map, !Core),

    ConcatStringName = q_name_append(builtin_module_name,
        builtin_concat_string),
    register_builtin_func(ConcatStringName,
        func_init(ConcatStringName, nil_context, s_private,
            [builtin_type(string), builtin_type(string)],
            [builtin_type(string)],
            init, init),
        _, !Map, !Core).

:- pred register_builtin_func(q_name::in, function::in, func_id::out,
    map(q_name, builtin_item)::in, map(q_name, builtin_item)::out,
    core::in, core::out) is det.

register_builtin_func(Name, Func, FuncId, !Map, !Core) :-
    core_allocate_function(FuncId, !Core),
    core_set_function(FuncId, Func, !Core),
    det_insert(Name, bi_func(FuncId), !Map).

%-----------------------------------------------------------------------%

builtin_module_name = q_name("builtin").

builtin_add_int = q_name("add_int").
builtin_sub_int = q_name("sub_int").
builtin_mul_int = q_name("mul_int").
builtin_div_int = q_name("div_int").
builtin_mod_int = q_name("mod_int").
builtin_lshift_int = q_name("lshift_int").
builtin_rshift_int = q_name("rshift_int").
builtin_and_int = q_name("and_int").
builtin_or_int = q_name("or_int").
builtin_xor_int = q_name("xor_int").
builtin_gt_int = q_name("gt_int").
builtin_lt_int = q_name("lt_int").
builtin_gteq_int = q_name("gteq_int").
builtin_lteq_int = q_name("lteq_int").
builtin_eq_int = q_name("eq_int").
builtin_neq_int = q_name("neq_int").
builtin_and_bool = q_name("and_bool").
builtin_or_bool = q_name("or_bool").
builtin_concat_string = q_name("concat_string").

builtin_minus_int = q_name("minus_int").
builtin_comp_int = q_name("comp_int").
builtin_not_bool = q_name("not_bool").

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
