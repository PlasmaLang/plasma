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

:- import_module core.
:- import_module q_name.

:- pred setup_builtins(map(q_name, func_id)::out,
    core::in, core::out) is det.

:- func builtin_module_name = q_name.

:- func builtin_add_int = q_name.
:- func builtin_sub_int = q_name.
:- func builtin_mul_int = q_name.
:- func builtin_div_int = q_name.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module require.
:- import_module set.
:- import_module string.

:- import_module context.
:- import_module common_types.
:- import_module core.types.

%-----------------------------------------------------------------------%

setup_builtins(Map, !Core) :-
    foldl2(register_builtin, builtins, !Core, init, Map).

%-----------------------------------------------------------------------%

:- type builtin
    --->    builtin(
                b_name          :: string,
                b_function      :: function
            ).

:- func builtins = list(builtin).

builtins = [
        builtin("print",
            func_init(nil_context, s_private, [builtin_type(string)],
                [], set([r_io]), init)),
        builtin("int_to_string",
            func_init(nil_context, s_private, [builtin_type(int)],
                [builtin_type(string)], init, init)),
        builtin("free",
            func_init(nil_context, s_private, [builtin_type(string)],
                [], set([r_io]), init))
    ] ++
    map((func(Name) =
        builtin(q_name_to_string(Name),
            func_init(nil_context, s_private,
                [builtin_type(int), builtin_type(int)],
                [builtin_type(int)],
                init, init))
        ), [builtin_add_int,
            builtin_sub_int,
            builtin_mul_int,
            builtin_div_int]).

:- pred register_builtin(builtin::in, core::in, core::out,
    map(q_name, func_id)::in, map(q_name, func_id)::out) is det.

register_builtin(Builtin, !Core, !Map) :-
    Builtin = builtin(Name, Func),
    QName = q_name_snoc(builtin_module_name, Name),
    ( if
        core_register_function(QName, FuncId, !Core)
    then
        core_set_function(FuncId, Func, !Core),
        det_insert(QName, FuncId, !Map)
    else
        unexpected($file, $pred, "Duplicate builtin")
    ).

%-----------------------------------------------------------------------%

builtin_module_name = q_name("builtin").

builtin_add_int = q_name("add_int").
builtin_sub_int = q_name("sub_int").
builtin_mul_int = q_name("mul_int").
builtin_div_int = q_name("div_int").

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
