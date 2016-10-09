%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module builtins.
%
% Copyright (C) 2015-2016 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
% Plasma builtins
%
%-----------------------------------------------------------------------%

:- interface.

:- import_module map.

:- import_module common_types.
:- import_module core.
:- import_module q_name.

:- pred setup_builtins(map(q_name, func_id)::out,
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
:- func builtin_concat_string = q_name.

% Unary operators.
:- func builtin_minus_int = q_name.
:- func builtin_comp_int = q_name.

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

setup_builtins(Map, !Core) :-
    foldl2(register_builtin, builtins, !Core, init, Map).

%-----------------------------------------------------------------------%

:- type builtin
    --->    builtin(
                b_name          :: q_name,
                b_function      :: function
            ).

:- func builtins = list(builtin).

builtins = Builtins1 ++ Builtins2 ++ Builtins3 :-
    PrintName = q_name_snoc(builtin_module_name, "print"),
    IntToStringName = q_name_snoc(builtin_module_name, "int_to_string"),
    FreeName = q_name_snoc(builtin_module_name, "free"),
    ConcatStringName = q_name_append(builtin_module_name,
        builtin_concat_string),
    Builtins1 = [
        builtin(PrintName,
            func_init(PrintName, nil_context, s_private,
                [builtin_type(string)], [], set([r_io]), init)),
        builtin(IntToStringName,
            func_init(IntToStringName, nil_context, s_private,
                [builtin_type(int)], [builtin_type(string)], init, init)),
        builtin(FreeName,
            func_init(FreeName, nil_context, s_private,
                [builtin_type(string)], [], set([r_io]), init)),
        builtin(ConcatStringName,
            func_init(ConcatStringName, nil_context, s_private,
                [builtin_type(string), builtin_type(string)],
                [builtin_type(string)],
                init, init))
    ],
    Builtins2 = map((func(Name) =
        builtin(q_name_append(builtin_module_name, Name),
            func_init(Name, nil_context, s_private,
                [builtin_type(int), builtin_type(int)],
                [builtin_type(int)],
                init, init))
        ), [builtin_add_int,
            builtin_sub_int,
            builtin_mul_int,
            builtin_div_int,
            builtin_mod_int,
            % TODO: make the number of bits to shift a single byte.
            builtin_lshift_int,
            builtin_rshift_int,
            builtin_and_int,
            builtin_or_int,
            builtin_xor_int]),
    Builtins3 = map((func(Name) =
        builtin(q_name_append(builtin_module_name, Name),
            func_init(Name, nil_context, s_private,
                [builtin_type(int)], [builtin_type(int)],
                init, init))
        ),
        [   builtin_minus_int,
            builtin_comp_int
        ]).

:- pred register_builtin(builtin::in, core::in, core::out,
    map(q_name, func_id)::in, map(q_name, func_id)::out) is det.

register_builtin(Builtin, !Core, !Map) :-
    Builtin = builtin(Name, Func),
    core_allocate_function(FuncId, !Core),
    core_set_function(FuncId, Func, !Core),
    det_insert(Name, FuncId, !Map).

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
builtin_concat_string = q_name("concat_string").

builtin_minus_int = q_name("minus_int").
builtin_comp_int = q_name("comp_int").

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
