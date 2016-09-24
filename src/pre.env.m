%-----------------------------------------------------------------------%
% Plasma AST Environment manipulation routines
% vim: ts=4 sw=4 et
%
% Copyright (C) 2015-2016 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
% This module contains code to track the environment of a statement in the
% Plasma AST.
%
%-----------------------------------------------------------------------%
:- module pre.env.
%-----------------------------------------------------------------------%

:- interface.

:- import_module string.

:- import_module ast.
:- import_module common_types.
:- import_module q_name.
:- import_module varmap.

%-----------------------------------------------------------------------%

:- type env.

:- func init = env.

:- pred env_add_var(string::in, var::out, env::in, env::out,
    varmap::in, varmap::out) is semidet.

:- pred env_add_func(q_name::in, func_id::in, env::in, env::out) is det.

:- pred env_import_star(q_name::in, env::in, env::out) is det.

:- type env_entry
    --->    ee_var(var)
    ;       ee_func(func_id).

:- pred env_search(env::in, q_name::in, env_entry::out) is semidet.

    % NOTE: This is currently only implemented for one data type per
    % operator.
    %
:- pred env_operator_func(env::in, ast_bop::in, func_id::out) is semidet.

:- pred env_unary_operator_func(env::in, ast_uop::in, func_id::out)
    is semidet.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

:- import_module map.
:- import_module require.

:- import_module builtins.

%-----------------------------------------------------------------------%

    % TODO, use a radix structure.  Lookup errors can be more informative.
    %
:- type env
    --->    env(
                e_map           :: map(q_name, env_entry)
            ).

%-----------------------------------------------------------------------%

init = env(init).

env_add_var(Name, Var, !Env, !Varmap) :-
    get_or_add_var(Name, Var, !Varmap),
    insert(q_name(Name), ee_var(Var), !.Env ^ e_map, Map),
    !:Env = env(Map).

env_add_func(Name, Func, !Env) :-
    det_insert(Name, ee_func(Func), !.Env ^ e_map, Map),
    !:Env = env(Map).

env_import_star(Name, !Env) :-
    Map0 = !.Env ^ e_map,
    foldl(do_env_import_star(Name), Map0, Map0, Map),
    !Env ^ e_map := Map.

:- pred do_env_import_star(q_name::in, q_name::in, env_entry::in,
    map(q_name, env_entry)::in, map(q_name, env_entry)::out) is det.

do_env_import_star(Module, Name, Entry, !Map) :-
    ( if q_name_append(Module, UnqualName, Name) then
        det_insert(UnqualName, Entry, !Map)
    else
        true
    ).

env_search(Env, QName, Entry) :-
    search(Env ^ e_map, QName, Entry).

%-----------------------------------------------------------------------%

env_operator_func(Env, Op, FuncId) :-
    env_operator_name(Op, Name),
    get_builtin_func(Env, Name, FuncId).

:- pred env_operator_name(ast_bop, q_name).
:- mode env_operator_name(in, out) is semidet.

env_operator_name(b_add,        builtin_add_int).
env_operator_name(b_sub,        builtin_sub_int).
env_operator_name(b_mul,        builtin_mul_int).
env_operator_name(b_div,        builtin_div_int).
env_operator_name(b_mod,        builtin_mod_int).
env_operator_name(b_lshift,     builtin_lshift_int).
env_operator_name(b_rshift,     builtin_rshift_int).
env_operator_name(b_and,        builtin_and_int).
env_operator_name(b_or,         builtin_or_int).
env_operator_name(b_xor,        builtin_xor_int).
env_operator_name(b_concat,     builtin_concat_string).

env_unary_operator_func(Env, UOp, FuncId) :-
    env_unary_operator_name(UOp, Name),
    get_builtin_func(Env, Name, FuncId).

:- pred env_unary_operator_name(ast_uop, q_name).
:- mode env_unary_operator_name(in, out) is det.

env_unary_operator_name(u_minus,    builtin_minus_int).
env_unary_operator_name(u_comp,     builtin_comp_int).

:- pred get_builtin_func(env::in, q_name::in, func_id::out) is semidet.

get_builtin_func(Env, Name, FuncId) :-
    q_name_append(builtin_module_name, Name, QName),
    env_search(Env, QName, Entry),
    require_complete_switch [Entry]
    ( Entry = ee_var(_),
        unexpected($file, $pred, "var")
    ; Entry = ee_func(FuncId)
    ).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
