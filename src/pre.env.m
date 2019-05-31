%-----------------------------------------------------------------------%
% Plasma AST Environment manipulation routines
% vim: ts=4 sw=4 et
%
% Copyright (C) 2015-2019 Plasma Team
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

    % init(BoolTrue, BoolFalse, ListNil, ListCons) = Env.
    %
:- func init(ctor_id, ctor_id, ctor_id, ctor_id) = env.

    % Add but leave a variable uninitialised.
    %
    % The variable must not already exist.
    %
:- pred env_add_uninitialised_var(string::in, var::out, env::in, env::out,
    varmap::in, varmap::out) is semidet.

    % Add and initialise a variable.
    %
    % The variable must not already exist.
    %
:- pred env_add_and_initlalise_var(string::in, var::out, env::in, env::out,
    varmap::in, varmap::out) is semidet.

    % Initialise an existing variable.
    %
:- pred env_initialise_var(string::in, var::out, env::in, env::out,
    varmap::in, varmap::out) is semidet.

:- pred env_add_func(q_name::in, func_id::in, env::in, env::out) is semidet.

    % Used to add builtins, which always have unique names.
    %
:- pred env_add_func_det(q_name::in, func_id::in, env::in, env::out) is det.

:- pred env_add_type(q_name::in, arity::in, type_id::in, env::in, env::out)
    is semidet.

:- pred env_add_type_det(q_name::in, arity::in, type_id::in, env::in, env::out)
    is det.

    % Constructors may be overloaded, so this always succeeds.
    %
:- pred env_add_constructor(q_name::in, ctor_id::in, env::in, env::out)
    is det.

:- pred env_add_resource(q_name::in, resource_id::in, env::in, env::out)
    is semidet.

:- pred env_add_resource_det(q_name::in, resource_id::in, env::in, env::out)
    is det.

:- pred env_import_star(q_name::in, env::in, env::out) is det.

:- type env_entry
    --->    ee_var(var, initialised)
    ;       ee_func(func_id)
    ;       ee_constructor(ctor_id).

:- inst env_entry_func_or_ctor
    --->    ee_func(ground)
    ;       ee_constructor(ground).

:- type initialised
    --->    var_is_initialised
    ;       var_is_uninitialised.

:- pred env_search(env::in, q_name::in, env_entry::out) is semidet.

    % Throws an exception if the entry doesn't exist or isn't a function.
    %
:- pred env_lookup_function(env::in, q_name::in, func_id::out) is det.

:- pred env_search_type(env::in, q_name::in, type_id::out, arity::out)
    is semidet.

:- pred env_lookup_type(env::in, q_name::in, type_id::out, arity::out) is det.

:- pred env_search_constructor(env::in, q_name::in, ctor_id::out) is semidet.

    % NOTE: This is currently only implemented for one data type per
    % operator.
    %
:- pred env_operator_entry(env, ast_bop, env_entry).
:- mode env_operator_entry(in, in, out(env_entry_func_or_ctor)) is semidet.

:- pred env_unary_operator_func(env::in, ast_uop::in, func_id::out)
    is semidet.

:- pred env_search_resource(env::in, q_name::in, resource_id::out)
    is semidet.

:- pred env_lookup_resource(env::in, q_name::in, resource_id::out) is det.

%-----------------------------------------------------------------------%

    % Lookup very specific symbols.
    %
:- func env_get_bool_true(env) = ctor_id.
:- func env_get_bool_false(env) = ctor_id.

:- func env_get_list_nil(env) = ctor_id.
:- func env_get_list_cons(env) = ctor_id.

%-----------------------------------------------------------------------%

:- pred do_var_or_wildcard(pred(X, Y, A, A, B, B),
    var_or_wildcard(X), var_or_wildcard(Y), A, A, B, B).
:- mode do_var_or_wildcard(pred(in, out, in, out, in, out) is semidet,
    in, out, in, out, in, out) is semidet.

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
                e_map           :: map(q_name, env_entry),
                e_typemap       :: map(q_name, type_entry),
                e_resmap        :: map(q_name, resource_id),

                % Some times we need to look up particular constructors, whe
                % we do this we know exactly which constroctor and don't
                % need to use the normal name resolution.

                % We need to lookup bool constructors for generating ITE
                % code.
                e_bool_true     :: ctor_id,
                e_bool_false    :: ctor_id,

                % We need to lookup list constructors to handle built in
                % list syntax.
                e_list_nil      :: ctor_id,
                e_list_cons     :: ctor_id
            ).

:- type type_entry
    --->    type_entry(
                te_id           :: type_id,
                te_arity        :: arity
            ).

%-----------------------------------------------------------------------%

init(BoolTrue, BoolFalse, ListNil, ListCons) =
    env(init, init, init, BoolTrue, BoolFalse, ListNil, ListCons).

%-----------------------------------------------------------------------%

env_add_uninitialised_var(Name, Var, !Env, !Varmap) :-
    env_add_var(Name, Var, var_is_uninitialised, !Env, !Varmap).

env_add_and_initlalise_var(Name, Var, !Env, !Varmap) :-
    env_add_var(Name, Var, var_is_initialised, !Env, !Varmap).

:- pred env_add_var(string::in, var::out, initialised::in,
    env::in, env::out, varmap::in, varmap::out) is semidet.

env_add_var(Name, Var, State, !Env, !Varmap) :-
    ( if Name = "_" then
        unexpected($file, $pred, "Wildcard string as varname")
    else
        get_or_add_var(Name, Var, !Varmap),
        insert(q_name(Name), ee_var(Var, State),
            !.Env ^ e_map, Map),
        !Env ^ e_map := Map
    ).

env_initialise_var(Name, Var, !Env, !Varmap) :-
    ( if Name = "_" then
        unexpected($file, $pred, "Windcard string as varname")
    else
        ( if
            search(!.Env ^ e_map, q_name(Name), ee_var(VarPrime, State))
        then
            State = var_is_uninitialised,
            Var = VarPrime,
            update(q_name(Name), ee_var(Var, var_is_initialised),
                !.Env ^ e_map, Map),
            !Env ^ e_map := Map
        else
            env_add_and_initlalise_var(Name, Var, !Env, !Varmap)
        )
    ).

%-----------------------------------------------------------------------%

env_add_func(Name, Func, !Env) :-
    insert(Name, ee_func(Func), !.Env ^ e_map, Map),
    !Env ^ e_map := Map.

env_add_func_det(Name, Func, !Env) :-
    ( if env_add_func(Name, Func, !Env) then
        true
    else
        unexpected($file, $pred, "Function already exists")
    ).

%-----------------------------------------------------------------------%

env_add_type(Name, Arity, Type, !Env) :-
    insert(Name, type_entry(Type, Arity), !.Env ^ e_typemap, Map),
    !Env ^ e_typemap := Map.

env_add_type_det(Name, Arity, Type, !Env) :-
    ( if env_add_type(Name, Arity, Type, !Env) then
        true
    else
        unexpected($file, $pred, "Type already defined")
    ).

%-----------------------------------------------------------------------%

env_add_constructor(Name, Cons, !Env) :-
    det_insert(Name, ee_constructor(Cons), !.Env ^ e_map, Map),
    !Env ^ e_map := Map.

%-----------------------------------------------------------------------%

env_add_resource(Name, ResId, !Env) :-
    insert(Name, ResId, !.Env ^ e_resmap, Map),
    !Env ^ e_resmap := Map.

env_add_resource_det(Name, ResId, !Env) :-
    det_insert(Name, ResId, !.Env ^ e_resmap, Map),
    !Env ^ e_resmap := Map.

%-----------------------------------------------------------------------%

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

%-----------------------------------------------------------------------%

env_search(Env, QName, Entry) :-
    search(Env ^ e_map, QName, Entry).

env_lookup_function(Env, QName, FuncId) :-
    ( if env_search(Env, QName, ee_func(FuncIdPrime)) then
        FuncId = FuncIdPrime
    else
        unexpected($file, $pred, "Entry not found or not a function")
    ).

env_search_type(Env, QName, TypeId, Arity) :-
    search(Env ^ e_typemap, QName, type_entry(TypeId, Arity)).

env_lookup_type(Env, QName, TypeId, Arity) :-
    ( if env_search_type(Env, QName, TypeIdPrime, ArityPrime) then
        TypeId = TypeIdPrime,
        Arity = ArityPrime
    else
        unexpected($file, $pred, "Type not found")
    ).

env_search_constructor(Env, QName, CtorId) :-
    env_search(Env, QName, ee_constructor(CtorId)).

%-----------------------------------------------------------------------%

env_operator_entry(Env, Op, Entry) :-
    env_operator_name(Op, Name),
    env_search(Env, Name, Entry),
    ( Entry = ee_func(_)
    ; Entry = ee_constructor(_)
    ).

:- pred env_operator_name(ast_bop, q_name).
:- mode env_operator_name(in, out) is semidet.

env_operator_name(b_add,            builtin_add_int).
env_operator_name(b_sub,            builtin_sub_int).
env_operator_name(b_mul,            builtin_mul_int).
env_operator_name(b_div,            builtin_div_int).
env_operator_name(b_mod,            builtin_mod_int).
env_operator_name(b_gt,             builtin_gt_int).
env_operator_name(b_lt,             builtin_lt_int).
env_operator_name(b_gteq,           builtin_gteq_int).
env_operator_name(b_lteq,           builtin_lteq_int).
env_operator_name(b_eq,             builtin_eq_int).
env_operator_name(b_neq,            builtin_neq_int).
env_operator_name(b_logical_and,    builtin_and_bool).
env_operator_name(b_logical_or,     builtin_or_bool).
env_operator_name(b_concat,         builtin_concat_string).
env_operator_name(b_list_cons,      builtin_cons_list).

env_unary_operator_func(Env, UOp, FuncId) :-
    env_unary_operator_name(UOp, Name),
    get_builtin_func(Env, Name, FuncId).

:- pred env_unary_operator_name(ast_uop, q_name).
:- mode env_unary_operator_name(in, out) is det.

env_unary_operator_name(u_minus,    builtin_minus_int).
env_unary_operator_name(u_not,      builtin_not_bool).

:- pred get_builtin_func(env::in, q_name::in, func_id::out) is semidet.

get_builtin_func(Env, Name, FuncId) :-
    env_search(Env, Name, Entry),
    require_complete_switch [Entry]
    ( Entry = ee_var(_, _),
        unexpected($file, $pred, "var")
    ; Entry = ee_constructor(_),
        unexpected($file, $pred, "constructor")
    ; Entry = ee_func(FuncId)
    ).

%-----------------------------------------------------------------------%

env_search_resource(Env, QName, ResId) :-
    search(Env ^ e_resmap, QName, ResId).

env_lookup_resource(Env, QName, ResId) :-
    lookup(Env ^ e_resmap, QName, ResId).

%-----------------------------------------------------------------------%

env_get_bool_true(Env) = Env ^ e_bool_true.
env_get_bool_false(Env) = Env ^ e_bool_false.

env_get_list_nil(Env) = Env ^ e_list_nil.
env_get_list_cons(Env) = Env ^ e_list_cons.

%-----------------------------------------------------------------------%

do_var_or_wildcard(Pred, var(Name), var(Var), !Env, !Varmap) :-
    Pred(Name, Var, !Env, !Varmap).
do_var_or_wildcard(_, wildcard, wildcard, !Env, !Varmap).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
