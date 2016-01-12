%-----------------------------------------------------------------------%
% Plasma builtins
% vim: ts=4 sw=4 et
%
% Copyright (C) 2015-2016 Paul Bone
% Distributed under the terms of the GPLv2 see ../LICENSE.tools
%
% This program compiles plasma modules.
%
%-----------------------------------------------------------------------%
:- module builtins.
%-----------------------------------------------------------------------%

:- interface.

:- import_module core.

:- pred setup_builtins(core::in, core::out) is det.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module require.
:- import_module set.
:- import_module string.

:- import_module core.types.
:- import_module symtab.

%-----------------------------------------------------------------------%

setup_builtins(!Core) :-
    foldl(register_builtin, builtins, !Core).

%-----------------------------------------------------------------------%

:- type builtin
    --->    builtin(
                b_name          :: string,
                b_function      :: function
            ).

:- func builtins = list(builtin).

builtins = [
        builtin("print",
            function_init(s_private, [builtin_type(string)],
                builtin_type(void), set([r_io]), init)),
        builtin("int_to_string",
            function_init(s_private, [builtin_type(int)],
                builtin_type(string), init, init)),
        builtin("free",
            function_init(s_private, [builtin_type(string)],
                builtin_type(void), set([r_io]), init))
    ].

:- pred register_builtin(builtin::in, core::in, core::out) is det.

register_builtin(Builtin, !Core) :-
    Builtin = builtin(Name, Func),
    ( if
        core_register_function(symbol_append(symbol("builtin"), Name),
            FuncId, !Core)
    then
        core_set_function(FuncId, Func, !Core)
    else
        unexpected($file, $pred, "Duplicate builtin")
    ).

