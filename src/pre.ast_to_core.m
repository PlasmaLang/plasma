%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module pre.ast_to_core.
%
% Copyright (C) 2015-2016 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
% Plasma parse tree to core representation conversion
%
%-----------------------------------------------------------------------%

:- interface.

:- import_module ast.
:- import_module compile_error.
:- import_module core.
:- import_module result.

%-----------------------------------------------------------------------%

:- pred ast_to_core(ast::in, result(core, compile_error)::out) is det.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module cord.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module require.
:- import_module set.
:- import_module string.

:- import_module pre.env.
:- import_module pre.from_ast.
:- import_module pre.nonlocals.
:- import_module pre.pre_ds.
:- import_module pre.to_core.
:- import_module builtins.
:- import_module context.
:- import_module common_types.
:- import_module core.code.
:- import_module core.types.
:- import_module q_name.
:- import_module result.
:- import_module varmap.

%-----------------------------------------------------------------------%

ast_to_core(ast(ModuleName, Entries), Result) :-
    Exports = gather_exports(Entries),
    some [!Core, !Errors] (
        !:Core = core.init(q_name(ModuleName)),
        !:Errors = init,

        setup_builtins(BuiltinMap, !Core),
        map.foldl(env_add_func, BuiltinMap, env.init, Env0),
        env_import_star(builtin_module_name, Env0, Env1),

        foldl3(gather_funcs, Entries, !Core, Env1, Env, !Errors),
        ( if is_empty(!.Errors) then
            foldl2(build_function(Exports, Env), Entries, !Core, !Errors),
            ( if is_empty(!.Errors) then
                Result = ok(!.Core)
            else
                Result = errors(!.Errors)
            )
        else
            Result = errors(!.Errors)
        )
    ).

%-----------------------------------------------------------------------%

:- type exports
    --->    exports(set(string))
    ;       export_all.

:- func gather_exports(list(ast_entry)) = exports.

gather_exports(Entries) = Exports :-
    ( if member(ast_export(export_all), Entries) then
        Exports = export_all
    else
        filter_map(
            (pred(Entry::in, Export::out) is semidet :-
                Entry = ast_export(export_some(List)),
                Export = set(List)
            ), Entries, Sets),
        Exports = exports(union_list(Sets))
    ).

%-----------------------------------------------------------------------%

:- pred gather_funcs(ast_entry::in, core::in, core::out, env::in, env::out,
    errors(compile_error)::in, errors(compile_error)::out) is det.

gather_funcs(ast_export(_), !Core, !Env, !Errors).
gather_funcs(ast_import(_, _), !Core, !Env, !Errors).
gather_funcs(ast_type(_, _, _, _), !Core, !Env, !Errors).
gather_funcs(ast_function(Name, _, _, _, _, Context), !Core, !Env,
        !Errors) :-
    QName = q_name_snoc(module_name(!.Core), Name),
    ( if
        core_register_function(QName, FuncId, !Core)
    then
        % Add the function to the environment with it's local name, since
        % we're in the scope of the module already.
        env_add_func(q_name(Name), FuncId, !Env)
    else
        add_error(Context, ce_function_already_defined(Name), !Errors)
    ).

%-----------------------------------------------------------------------%

:- pred build_function(exports::in, env::in, ast_entry::in,
    core::in, core::out,
    errors(compile_error)::in, errors(compile_error)::out) is det.

build_function(_, _, ast_export(_), !Core, !Errors).
build_function(_, _, ast_import(_, _), !Core, !Errors).
build_function(_, _, ast_type(_, _, _, _), !Core, !Errors).
build_function(Exports, Env0, ast_function(Name, Params, Return, Using0,
        Body0, Context), !Core, !Errors) :-
    ModuleName = module_name(!.Core),
    det_core_lookup_function(!.Core, q_name_snoc(ModuleName, Name), FuncId),

    % Build basic information about the function.
    Sharing = sharing(Exports, Name),
    ParamTypesResult = result_list_to_result(map(build_param_type, Params)),
    ReturnTypeResult = build_type(Return),
    foldl2(build_using, Using0, set.init, Using, set.init, Observing),
    IntersectUsingObserving = intersect(Using, Observing),
    ( if
        ParamTypesResult = ok(ParamTypes),
        ReturnTypeResult = ok(ReturnType),
        is_empty(IntersectUsingObserving)
    then
        Function0 = func_init(Context, Sharing, ParamTypes, [ReturnType],
            Using, Observing),

        % Build body.
        ParamNames = map((func(ast_param(N, _)) = N), Params),
        some [!Varmap] (
            !:Varmap = varmap.init,
            % XXX: parameters must be named appart.
            map_foldl2(env_add_var, ParamNames, ParamVars, Env0, Env,
                !Varmap),
            build_body(Env, Context, ParamVars, Body0, Body, !Varmap),
            func_set_body(!.Varmap, ParamVars, Body, Function0, Function)
        ),
        core_set_function(FuncId, Function, !Core)
    else
        ( if ParamTypesResult = errors(ParamTypesErrors) then
            !:Errors = ParamTypesErrors ++ !.Errors
        else
            true
        ),
        ( if ReturnTypeResult = errors(ReturnTypeErrors) then
            !:Errors = ReturnTypeErrors ++ !.Errors
        else
            true
        ),
        ( if not is_empty(IntersectUsingObserving) then
            add_error(Context,
                ce_using_observing_not_distinct(IntersectUsingObserving),
                !Errors)
        else
            true
        )
    ).

:- func sharing(exports, string) = sharing.

sharing(export_all, _) = s_public.
sharing(exports(Exports), Name) =
    ( if member(Name, Exports) then
        s_public
    else
        s_private
    ).

:- func build_param_type(ast_param) = result(type_, compile_error).

build_param_type(ast_param(_, Type)) = build_type(Type).

:- func build_type(ast_type_expr) = result(type_, compile_error).

build_type(ast_type(Qualifiers, Name, Args0, Context)) = Result :-
    ( if
        Qualifiers = [],
        builtin_type_name(Type, Name)
    then
        ( Args0 = [],
            Result = ok(builtin_type(Type))
        ; Args0 = [_ | _],
            Result = return_error(Context, ce_builtin_type_with_args(Name))
        )
    else
        ArgsResult = result_list_to_result(map(build_type, Args0)),
        ( ArgsResult = ok(Args),
            Result = ok(type_(q_name(Qualifiers, Name), Args))
        ; ArgsResult = errors(Error),
            Result = errors(Error)
        )
    ).
build_type(ast_type_var(Name, _Context)) = Result :-
    Result = ok(type_variable(Name)).

:- pred build_using(ast_using::in,
    set(resource)::in, set(resource)::out,
    set(resource)::in, set(resource)::out) is det.

build_using(ast_using(Type, ResourceName), !Using, !Observing) :-
    ( if ResourceName = "IO" then
        Resource = r_io,
        ( Type = ut_using,
            !:Using = set.insert(!.Using, Resource)
        ; Type = ut_observing,
            !:Observing = set.insert(!.Observing, Resource)
        )
    else
        sorry($file, $pred, "Only IO resource is supported")
    ).

%-----------------------------------------------------------------------%

:- pred build_body(env::in, context::in, list(var)::in,
    list(ast_statement)::in, expr::out, varmap::in, varmap::out) is det.

build_body(Env, _Context, ParamVars, !.Statements, Expr, !Varmap) :-
    % Steps 1-4 transform the statements to get them into a form
    % that's easy to create the core representation from (steps 5 & 6).

    % 1. Resolve symbols, build a varmap, build var use sets and
    %    over-conservative var-def sets.
    ast_to_pre(!Statements, _, _, Env, _, !Varmap),

    % 2. Determine nonlocals
    compute_nonlocals_stmts(set(ParamVars), !Statements),

    % NOTE: This code is being actively worked on.  But it works now for
    % programs without control flow.

    % 3. TODO: Name appart vars on different branches, except where they are
    %    nonlocals, fixup var-def sets.

    % 4. Transform the whole structure into an expression tree.
    %    TODO: Handle return statements in branches, where some branches
    %    fall-through and others don't.
    pre_to_core(!.Statements, Expr, !Varmap).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
