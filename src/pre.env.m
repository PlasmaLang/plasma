%-----------------------------------------------------------------------%
% Plasma AST Environment manipulation routines
% vim: ts=4 sw=4 et
%
% Copyright (C) 2015-2021 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
% This module contains code to track the environment of a statement in the
% Plasma AST.
%
%-----------------------------------------------------------------------%
:- module pre.env.
%-----------------------------------------------------------------------%

:- interface.

:- import_module set.
:- import_module string.

:- import_module ast.
:- import_module context.
:- import_module common_types.
:- import_module core.
:- import_module core.types.
:- import_module q_name.
:- import_module varmap.

%-----------------------------------------------------------------------%

:- type env.

    % init(Operators) = Env.
    %
:- func init(operators) = env.

    % Sometimes we need to look up particular operators and constructors,
    % when we do this we know exactly which constroctor and don't need to
    % use the normal name resolution.
    %
:- type operators
    --->    operators(
                o_int_add       :: func_id,
                o_int_sub       :: func_id,
                o_int_mul       :: func_id,
                o_int_div       :: func_id,
                o_int_mod       :: func_id,
                o_int_gt        :: func_id,
                o_int_lt        :: func_id,
                o_int_gteq      :: func_id,
                o_int_lteq      :: func_id,
                o_int_eq        :: func_id,
                o_int_neq       :: func_id,

                % Unary minus
                o_int_minus     :: func_id,

                % We need to lookup bool constructors for generating ITE
                % code.
                o_bool_true     :: ctor_id,
                o_bool_false    :: ctor_id,
                o_bool_and      :: func_id,
                o_bool_or       :: func_id,
                o_bool_not      :: func_id,

                % We need to lookup list constructors to handle built in
                % list syntax.
                o_list_type     :: type_id,
                o_list_nil      :: ctor_id,
                o_list_cons     :: ctor_id,

                o_string_concat :: func_id
    ).

:- func env_operators(env) = operators.

%-----------------------------------------------------------------------%
%
% Code to add variables and maniuplate their visibility in the environment.
%

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

:- type initialise_result(T)
    --->    ok(T)
    ;       does_not_exist
    ;       already_initialised
    ;       inaccessible.

    % Initialise an existing variable.
    %
    % The variable must already exist.
    %
:- pred env_initialise_var(string::in, initialise_result(var)::out,
    env::in, env::out, varmap::in, varmap::out) is det.

    % All the vars that are defined but not initialised.
    %
:- func env_uninitialised_vars(env) = set(var).

    % Mark all these uninitialised vars as initialised.
    %
:- pred env_mark_initialised(set(var)::in, env::in, env::out) is det.

    % Within a closure scope the currently-uninitialised variables cannot be
    % accessed from the closure.
    %
    % We leave closures (like any scope) by discarding the environment and
    % using a "higher" one.
    %
:- pred env_enter_closure(env::in, env::out) is det.

    % Add a letrec variable.
    %
    % These are added to help resolve names within nested functions.
    % They're cleared when the real variable bindings become available.
    % Discarding is performed by discarding the environment.
    %
:- pred env_add_for_letrec(string::in, var::out, env::in, env::out,
    varmap::in, varmap::out) is semidet.

    % Within a letrec temporally set a self-recursive reference to a direct
    % function call.  This is how we handle self-recursion, which works
    % because its the same environment.
    %
:- pred env_letrec_self_recursive(string::in, func_id::in,
    env::in, env::out) is det.

    % Mark the formerly-letrec variable as a fully defined variable, because
    % it has now been defined while processing the letrec.
    %
:- pred env_letrec_defined(string::in, env::in, env::out) is det.

    % Make all letrec variables initalised (we've finished building the
    % letrec).
    %
:- pred env_leave_letrec(env::in, env::out) is det.

%-----------------------------------------------------------------------%
%
% Code to add other symbols to the environment.
%

:- pred env_add_func(q_name::in, func_id::in, env::in, env::out) is semidet.

    % Used to add builtins, which always have unique names.
    %
:- pred env_add_func_det(q_name::in, func_id::in, env::in, env::out) is det.

:- pred env_add_type(q_name::in, arity::in, type_id::in, env::in, env::out)
    is semidet.

:- pred env_add_type_det(q_name::in, arity::in, type_id::in, env::in, env::out)
    is det.

:- pred env_add_builtin_type_det(q_name::in, builtin_type::in,
    env::in, env::out) is det.

    % Constructors may be overloaded, unlike other symbols, this predicate
    % will add this constructor ID to the set of constructor IDs that this
    % name may be referring to.  If the name is already bound to something
    % else, it throws an exception.
    %
:- pred env_add_constructor(q_name::in, ctor_id::in, env::in, env::out)
    is det.

:- pred env_add_resource(q_name::in, resource_id::in, env::in, env::out)
    is semidet.

:- pred env_add_resource_det(q_name::in, resource_id::in, env::in, env::out)
    is det.

%-----------------------------------------------------------------------%
%
% Code to query the environment.
%

:- type env_entry
    --->    ee_var(var)
    ;       ee_func(func_id)
    ;       ee_constructor(set(ctor_id)).

:- inst env_entry_func_or_ctor for env_entry/0
    --->    ee_func(ground)
    ;       ee_constructor(ground).

:- type env_search_result(T)
    --->    ok(T)
    ;       not_found
    ;       not_initaliased
    ;       inaccessible
    ;       maybe_cyclic_retlec.

:- pred env_search(env::in, q_name::in, env_search_result(env_entry)::out)
    is det.

    % Throws an exception if the entry doesn't exist or isn't a function.
    %
:- pred env_lookup_function(env::in, q_name::in, func_id::out) is det.

:- type type_entry
    --->    te_builtin(
                te_builtin      :: builtin_type
            )
    ;       te_id(
                te_id           :: type_id,
                te_arity        :: arity
            ).

:- pred env_search_type(env::in, q_name::in, type_entry::out) is semidet.

:- pred env_lookup_type(env::in, q_name::in, type_entry::out) is det.

:- pred env_search_constructor(env::in, q_name::in, set(ctor_id)::out)
    is semidet.

    % NOTE: This is currently only implemented for one data type per
    % operator.
    %
:- pred env_operator_entry(env, ast_bop, env_entry).
:- mode env_operator_entry(in, in, out(env_entry_func_or_ctor)) is semidet.

:- func env_unary_operator_func(env, ast_uop) = func_id.

:- pred env_search_resource(env::in, q_name::in, resource_id::out)
    is semidet.

:- pred env_lookup_resource(env::in, q_name::in, resource_id::out) is det.

%-----------------------------------------------------------------------%
%
% Misc.
%

    % Make a mangled name for a lambda.
    %
:- func mangle_lambda(string, context) = string.

    % A name->func_id mapping is tracked in the environment.  These aren't
    % actual name bindings in the Plasma language, and env_search won't find
    % them.  It's just convenient to put them in this data structure since
    % they're added at the top level and not needed after the pre-core
    % compilation stage.
    %
    % This is different from the letrec entries added above.
    %
:- pred env_add_lambda(string::in, func_id::in, env::in, env::out) is det.

:- pred env_lookup_lambda(env::in, string::in, func_id::out) is det.

%-----------------------------------------------------------------------%

:- pred do_var_or_wildcard(pred(X, Y, A, A, B, B),
    var_or_wildcard(X), var_or_wildcard(Y), A, A, B, B).
:- mode do_var_or_wildcard(pred(in, out, in, out, in, out) is det,
    in, out, in, out, in, out) is det.
:- mode do_var_or_wildcard(pred(in, out, in, out, in, out) is semidet,
    in, out, in, out, in, out) is semidet.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module map.
:- import_module maybe.
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
                e_lambdas       :: map(string, func_id),

                % The set of uninitialised variables
                e_uninitialised :: set(var),

                % The set of letrec variables, they're also uninitialised but
                % their definition may be recursive and so we don't generate
                % an error as we do for uninitialised ones.
                e_letrec_vars   :: set(var),

                % Uninitalised variables outside this closure.
                e_inaccessible  :: set(var),

                e_operators     :: operators
            ).

%-----------------------------------------------------------------------%

init(Operators) =
    env(init, init, init, init, init, init, init, Operators).

env_operators(Env) = Env ^ e_operators.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

env_add_uninitialised_var(Name, Var, !Env, !Varmap) :-
    env_add_var(Name, Var, !Env, !Varmap),
    !Env ^ e_uninitialised := insert(!.Env ^ e_uninitialised, Var).

env_add_and_initlalise_var(Name, Var, !Env, !Varmap) :-
    env_add_var(Name, Var, !Env, !Varmap).

:- pred env_add_var(string::in, var::out, env::in, env::out,
    varmap::in, varmap::out) is semidet.

env_add_var(Name, Var, !Env, !Varmap) :-
    ( if Name = "_" then
        unexpected($file, $pred, "Wildcard string as varname")
    else
        add_fresh_var(Name, Var, !Varmap),
        insert(q_name_single(Name), ee_var(Var), !.Env ^ e_map, Map),
        !Env ^ e_map := Map
    ).

env_initialise_var(Name, Result, !Env, !Varmap) :-
    ( if Name = "_" then
        unexpected($file, $pred, "Windcard string as varname")
    else
        ( if search(!.Env ^ e_map, q_name_single(Name), ee_var(Var))
        then
            ( if remove(Var, !.Env ^ e_uninitialised, Uninitialised) then
                !Env ^ e_uninitialised := Uninitialised,
                Result = ok(Var)
            else if member(Var, !.Env ^ e_inaccessible) then
                Result = inaccessible
            else if member(Var, !.Env ^ e_letrec_vars) then
                unexpected($file, $pred,
                    "Cannot set letrec variables this way")
            else
                Result = already_initialised
            )
        else
            Result = does_not_exist
        )
    ).

%-----------------------------------------------------------------------%

env_uninitialised_vars(Env) = Env ^ e_uninitialised.

env_mark_initialised(Vars, !Env) :-
    !Env ^ e_uninitialised := !.Env ^ e_uninitialised `difference` Vars.

env_enter_closure(!Env) :-
    !Env ^ e_inaccessible := !.Env ^ e_uninitialised,
    !Env ^ e_uninitialised := set.init.

%-----------------------------------------------------------------------%

env_add_for_letrec(Name, Var, !Env, !Varmap) :-
    env_add_var(Name, Var, !Env, !Varmap),
    !Env ^ e_letrec_vars := insert(!.Env ^ e_letrec_vars, Var).

env_letrec_self_recursive(Name, FuncId, !Env) :-
    lookup(!.Env ^ e_map, q_name_single(Name), Entry),
    ( Entry = ee_var(Var),
        det_update(q_name_single(Name), ee_func(FuncId), !.Env ^ e_map, Map),
        !Env ^ e_map := Map,
        det_remove(Var, !.Env ^ e_letrec_vars, LetrecVars),
        !Env ^ e_letrec_vars := LetrecVars
    ;
        ( Entry = ee_func(_)
        ; Entry = ee_constructor(_)
        ),
        unexpected($file, $pred, "Entry is not a variable")
    ).

env_letrec_defined(Name, !Env) :-
    lookup(!.Env ^ e_map, q_name_single(Name), Entry),
    ( Entry = ee_var(Var),
        det_remove(Var, !.Env ^ e_letrec_vars, LetrecVars),
        !Env ^ e_letrec_vars := LetrecVars
    ;
        ( Entry = ee_func(_)
        ; Entry = ee_constructor(_)
        ),
        unexpected($file, $pred, "Not a variable")
    ).

env_leave_letrec(!Env) :-
    ( if not is_empty(!.Env ^ e_letrec_vars) then
        !Env ^ e_letrec_vars := set.init
    else
        unexpected($file, $pred, "Letrec had no variables")
    ).

%-----------------------------------------------------------------------%
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
    insert(Name, te_id(Type, Arity), !.Env ^ e_typemap, Map),
    !Env ^ e_typemap := Map.

env_add_type_det(Name, Arity, Type, !Env) :-
    ( if env_add_type(Name, Arity, Type, !Env) then
        true
    else
        unexpected($file, $pred, "Type already defined")
    ).

env_add_builtin_type_det(Name, Builtin, !Env) :-
    map.det_insert(Name, te_builtin(Builtin), !.Env ^ e_typemap, Map),
    !Env ^ e_typemap := Map.

%-----------------------------------------------------------------------%

env_add_constructor(Name, Cons, !Env) :-
    some [!Map] (
        !:Map = !.Env ^ e_map,
        ( if search(!.Env ^ e_map, Name, Entry) then
            ( Entry = ee_constructor(ConsSet0),
                ConsSet = insert(ConsSet0, Cons),
                det_update(Name, ee_constructor(ConsSet), !Map)
            ;
                ( Entry = ee_var(_)
                ; Entry = ee_func(_)
                ),
                unexpected($file, $pred,
                    "name already exists as non-constructor")
            )
        else
            det_insert(Name, ee_constructor(make_singleton_set(Cons)), !Map)
        ),
        !Env ^ e_map := !.Map
    ).

%-----------------------------------------------------------------------%

env_add_resource(Name, ResId, !Env) :-
    insert(Name, ResId, !.Env ^ e_resmap, Map),
    !Env ^ e_resmap := Map.

env_add_resource_det(Name, ResId, !Env) :-
    det_insert(Name, ResId, !.Env ^ e_resmap, Map),
    !Env ^ e_resmap := Map.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

env_search(Env, QName, Result) :-
    ( if search(Env ^ e_map, QName, Entry) then
        ( Entry = ee_var(Var),
            ( if member(Var, Env ^ e_inaccessible) then
                Result = inaccessible
            else if member(Var, Env ^ e_uninitialised) then
                Result = not_initaliased
            else if member(Var, Env ^ e_letrec_vars) then
                Result = maybe_cyclic_retlec
            else
                Result = ok(Entry)
            )
        ;
            ( Entry = ee_func(_)
            ; Entry = ee_constructor(_)
            ),
            Result = ok(Entry)
        )
    else
        Result = not_found
    ).

env_lookup_function(Env, QName, FuncId) :-
    ( if env_search(Env, QName, ok(ee_func(FuncIdPrime))) then
        FuncId = FuncIdPrime
    else
        unexpected($file, $pred, "Entry not found or not a function")
    ).

env_search_type(Env, QName, Type) :-
    search(Env ^ e_typemap, QName, Type).

env_lookup_type(Env, QName, Type) :-
    ( if env_search_type(Env, QName, TypePrime) then
        Type = TypePrime
    else
        unexpected($file, $pred, "Type not found")
    ).

env_search_constructor(Env, QName, CtorId) :-
    env_search(Env, QName, ok(ee_constructor(CtorId))).

%-----------------------------------------------------------------------%

env_operator_entry(Env, Op, Entry) :-
    Ops = env_operators(Env),
    (
        ( Op = b_add,
            Func = Ops ^ o_int_add
        ; Op = b_sub,
            Func = Ops ^ o_int_sub
        ; Op = b_mul,
            Func = Ops ^ o_int_mul
        ; Op = b_div,
            Func = Ops ^ o_int_div
        ; Op = b_mod,
            Func = Ops ^ o_int_mod
        ; Op = b_gt,
            Func = Ops ^ o_int_gt
        ; Op = b_lt,
            Func = Ops ^ o_int_lt
        ; Op = b_gteq,
            Func = Ops ^ o_int_gteq
        ; Op = b_lteq,
            Func = Ops ^ o_int_lteq
        ; Op = b_eq,
            Func = Ops ^ o_int_eq
        ; Op = b_neq,
            Func = Ops ^ o_int_neq
        ; Op = b_logical_and,
            Func = Ops ^ o_bool_and
        ; Op = b_logical_or,
            Func = Ops ^ o_bool_or
        ; Op = b_concat,
            Func = Ops ^ o_string_concat
        ),
        Entry = ee_func(Func)
    ; Op = b_list_cons,
        Entry = ee_constructor(make_singleton_set(Ops ^ o_list_cons))
    ).

env_unary_operator_func(Env, UOp) = FuncId :-
    Ops = env_operators(Env),
    ( UOp = u_minus,
        FuncId = Ops ^ o_int_minus
    ; UOp = u_not,
        FuncId = Ops ^ o_bool_not
    ).

%-----------------------------------------------------------------------%

env_search_resource(Env, QName, ResId) :-
    search(Env ^ e_resmap, QName, ResId).

env_lookup_resource(Env, QName, ResId) :-
    lookup(Env ^ e_resmap, QName, ResId).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

mangle_lambda(Name, context(_, Line, Col)) =
    string.format("lambda_l%d_%s_c%d", [i(Line), s(Name), i(Col)]).

env_add_lambda(Name, FuncId, !Env) :-
    det_insert(Name, FuncId, !.Env ^ e_lambdas, Lambdas),
    !Env ^ e_lambdas := Lambdas.

env_lookup_lambda(Env, Name, FuncId) :-
    lookup(Env ^ e_lambdas, Name, FuncId).

%-----------------------------------------------------------------------%

do_var_or_wildcard(Pred, var(Name), var(Var), !Env, !Varmap) :-
    Pred(Name, Var, !Env, !Varmap).
do_var_or_wildcard(_, wildcard, wildcard, !Env, !Varmap).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
