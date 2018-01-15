%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module pre.ast_to_core.
%
% Copyright (C) 2015-2018 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
% Plasma parse tree to core representation conversion
%
%-----------------------------------------------------------------------%

:- interface.

:- import_module io.
:- import_module map.

:- import_module ast.
:- import_module builtins.
:- import_module compile_error.
:- import_module core.
:- import_module options.
:- import_module q_name.
:- import_module result.

%-----------------------------------------------------------------------%

:- pred ast_to_core(compile_options::in, ast::in,
    map(q_name, builtin_item)::out, result(core, compile_error)::out,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module cord.
:- import_module int.
:- import_module list.
:- import_module maybe.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module util.

:- import_module common_types.
:- import_module context.
:- import_module core.code.
:- import_module core.function.
:- import_module core.resource.
:- import_module core.types.
:- import_module dump_stage.
:- import_module pre.branches.
:- import_module pre.env.
:- import_module pre.from_ast.
:- import_module pre.nonlocals.
:- import_module pre.pre_ds.
:- import_module pre.pretty.
:- import_module pre.resource.
:- import_module pre.to_core.
:- import_module q_name.
:- import_module result.
:- import_module varmap.

%-----------------------------------------------------------------------%

ast_to_core(COptions, ast(ModuleName, Entries), BuiltinMap, Result, !IO) :-
    Exports = gather_exports(Entries),
    some [!Env, !Core, !Errors] (
        !:Core = core.init(q_name(ModuleName)),
        !:Errors = init,

        setup_builtins(BuiltinMap, BoolTrue, BoolFalse, ListNil, ListCons,
            !Core),
        map.foldl(env_add_builtin, BuiltinMap,
            env.init(BoolTrue, BoolFalse, ListNil, ListCons),
            !:Env),
        env_import_star(builtin_module_name, !Env),

        ast_to_core_types(Entries, !Env, !Core, !Errors),

        ast_to_core_resources(Entries, !Env, !Core, !Errors),

        ast_to_core_funcs(COptions, ModuleName, Exports, Entries, !.Env,
            !Core, !Errors, !IO),
        ( if is_empty(!.Errors) then
            Result = ok(!.Core)
        else
            Result = errors(!.Errors)
        )
    ).

:- pred env_add_builtin(q_name::in, builtin_item::in, env::in, env::out)
    is det.

env_add_builtin(Name, bi_func(FuncId), !Env) :-
    env_add_func_det(Name, FuncId, !Env).
env_add_builtin(Name, bi_ctor(CtorId), !Env) :-
    env_add_constructor(Name, CtorId, !Env).
env_add_builtin(Name, bi_resource(ResId), !Env) :-
    env_add_resource_det(Name, ResId, !Env).
env_add_builtin(Name, bi_type(TypeId, Arity), !Env) :-
    env_add_type_det(Name, Arity, TypeId, !Env).

%-----------------------------------------------------------------------%

:- pred ast_to_core_types(list(ast_entry)::in, env::in, env::out,
    core::in, core::out,
    errors(compile_error)::in, errors(compile_error)::out) is det.

ast_to_core_types(Entries, !Env, !Core, !Errors) :-
    foldl2(gather_type, Entries, !Env, !Core),
    foldl3(ast_to_core_type, Entries, !Env, !Core, !Errors).

:- pred gather_type(ast_entry::in, env::in, env::out, core::in, core::out)
    is det.

gather_type(ast_export(_), !Env, !Core).
gather_type(ast_import(_, _), !Env, !Core).
gather_type(ast_type(Name, Params, _, _), !Env, !Core) :-
    Arity = arity(length(Params)),
    core_allocate_type_id(TypeId, !Core),
    Symbol = q_name(Name),
    ( if env_add_type(Symbol, Arity, TypeId, !Env) then
        true
    else
        compile_error($file, $pred, "Type already defined")
    ).
gather_type(ast_resource(_, _), !Env, !Core).
gather_type(ast_function(_, _, _, _, _, _), !Env, !Core).

:- pred ast_to_core_type(ast_entry::in, env::in, env::out,
    core::in, core::out,
    errors(compile_error)::in, errors(compile_error)::out) is det.

ast_to_core_type(ast_export(_), !Env, !Core, !Errors).
ast_to_core_type(ast_import(_, _), !Env, !Core, !Errors).
ast_to_core_type(ast_type(Name, Params, Constrs0, _Context),
        !Env, !Core, !Errors) :-
    % Check that each parameter is unique.
    foldl(check_param, Params, init, ParamsSet),

    Symbol = q_name(Name),
    env_lookup_type(!.Env, Symbol, TypeId, _),
    map_foldl2(ast_to_core_type_constructor(TypeId, Params, ParamsSet),
        Constrs0, CtorIdResults, !Env, !Core),
    CtorIdsResult = result_list_to_result(CtorIdResults),
    ( CtorIdsResult = ok(CtorIds),
        core_set_type(TypeId, init(Symbol, Params, CtorIds), !Core)
    ; CtorIdsResult = errors(Errors),
        add_errors(Errors, !Errors)
    ).
ast_to_core_type(ast_resource(_, _), !Env, !Core, !Errors).
ast_to_core_type(ast_function(_, _, _, _, _, _), !Env, !Core, !Errors).

:- pred check_param(string::in, set(string)::in, set(string)::out) is det.

check_param(Param, !Params) :-
    ( if insert_new(Param, !Params) then
        true
    else
        compile_error($file, $pred, "Non unique type parameters")
    ).

:- pred ast_to_core_type_constructor(type_id::in, list(string)::in,
    set(string)::in, at_constructor::in, result(ctor_id, compile_error)::out,
    env::in, env::out, core::in, core::out) is det.

ast_to_core_type_constructor(Type, Params, ParamsSet,
        at_constructor(Name, Fields0, _), Result, !Env, !Core) :-
    Symbol = q_name(Name),
    % TODO: Constructors in the environment may need to handle their arity.
    ( if env_search(!.Env, Symbol, Entry) then
        % Constructors can be overloaded with other constructors, but
        % not with functions or variables (Constructors start with
        % capital letters to avoid this).  Constructors with the same
        % name will share the same ctor_id, they'll be disambiguated
        % during type checking.
        ( Entry = ee_constructor(CtorId)
        ;
            ( Entry = ee_var(_)
            ; Entry = ee_func(_)
            ),
            util.compile_error($file, $pred,
                "Constructor name already used")
        )
    else
        env_add_constructor(Symbol, CtorId, !Env),
        core_allocate_ctor_id(CtorId, Symbol, !Core)
    ),

    map(ast_to_core_field(!.Env, ParamsSet), Fields0, FieldResults),
    FieldsResult = result_list_to_result(FieldResults),
    ( FieldsResult = ok(Fields),
        Constructor = constructor(Symbol, Params, Fields),
        core_set_constructor(Type, CtorId, Constructor, !Core),
        Result = ok(CtorId)
    ; FieldsResult = errors(Errors),
        Result = errors(Errors)
    ).

:- pred ast_to_core_field(env::in, set(string)::in, at_field::in,
    result(type_field, compile_error)::out) is det.

ast_to_core_field(Env, ParamsSet, at_field(Name, Type0, _), Result) :-
    Symbol = q_name(Name),
    TypeResult = build_type_ref(Env, check_type_vars(ParamsSet), Type0),
    ( TypeResult = ok(Type),
        Result = ok(type_field(Symbol, Type))
    ; TypeResult = errors(Errors),
        Result = errors(Errors)
    ).

%-----------------------------------------------------------------------%

:- pred ast_to_core_resources(list(ast_entry)::in, env::in, env::out,
    core::in, core::out,
    errors(compile_error)::in, errors(compile_error)::out) is det.

ast_to_core_resources(Entries, !Env, !Core, !Errors) :-
    foldl2(gather_resource, Entries, !Env, !Core),
    foldl2(ast_to_core_resource(!.Env), Entries, !Core, !Errors).

:- pred gather_resource(ast_entry::in, env::in, env::out,
    core::in, core::out) is det.

gather_resource(ast_export(_), !Env, !Core).
gather_resource(ast_import(_, _), !Env, !Core).
gather_resource(ast_type(_, _, _, _), !Env, !Core).
gather_resource(ast_resource(Name, _), !Env, !Core) :-
    core_allocate_resource_id(Res, !Core),
    Symbol = q_name(Name),
    ( if env_add_resource(Symbol, Res, !Env) then
        true
    else
        compile_error($file, $pred, "Resource already defined")
    ).
gather_resource(ast_function(_, _, _, _, _, _), !Env, !Core).

:- pred ast_to_core_resource(env::in, ast_entry::in, core::in, core::out,
    errors(compile_error)::in, errors(compile_error)::out) is det.

ast_to_core_resource(_, ast_export(_), !Core, !Errors).
ast_to_core_resource(_, ast_import(_, _), !Core, !Errors).
ast_to_core_resource(_, ast_type(_, _, _, _), !Core, !Errors).
ast_to_core_resource(Env, ast_resource(Name, FromName), !Core, !Errors) :-
    Symbol = q_name(Name),
    env_lookup_resource(Env, Symbol, Res),
    ( if
        env_search_resource(Env, FromName, FromRes)
    then
        core_set_resource(Res, r_other(Symbol, FromRes), !Core)
    else
        compile_error($file, $pred, "From resource not known")
    ).
ast_to_core_resource(_, ast_function(_, _, _, _, _, _), !Core, !Errors).

%-----------------------------------------------------------------------%

:- pred ast_to_core_funcs(compile_options::in, string::in, exports::in,
    list(ast_entry)::in, env::in, core::in, core::out,
    errors(compile_error)::in, errors(compile_error)::out, io::di, io::uo)
    is det.

ast_to_core_funcs(COptions, ModuleName, Exports, Entries, Env0, !Core,
        !Errors, !IO) :-
    foldl3(gather_funcs(Exports), Entries, !Core, Env0, Env, !Errors),
    ( if is_empty(!.Errors) then
        some [!Pre] (
            % 1. the func_to_pre step resolves symbols, builds a varmap,
            % builds var use sets and over-conservative var-def sets.
            list.foldl2(func_to_pre(Env), Entries, map.init,
                !:Pre, !Errors),
            ModuleNameQ = q_name(ModuleName),
            maybe_dump_stage(COptions, ModuleNameQ, "pre1_initial",
                pre_pretty(!.Core), !.Pre, !IO),

            % 2. Determine nonlocals
            map.map_values_only(compute_nonlocals, !Pre),
            maybe_dump_stage(COptions, ModuleNameQ, "pre2_nonlocals",
                pre_pretty(!.Core), !.Pre, !IO),

            % 3. Fixup how variables are used in branching code, this pass:
            %    * fixes var-def sets
            %    * checks that used variables are always well defined (eg
            %      along all execution paths)
            %    * names-appart branch-local variables (from other
            %      branches).
            %    * Updates the reachability information for branches.
            %      Reachability information is incomplete until after
            %      typechecking.
            %    * Adds terminating "return" statements where needed.
            %
            % NOTE: This code is being actively worked on.  But it works for
            % some simple cases of control flow.
            %
            process_procs(fix_branches, !Pre, !Errors),
            maybe_dump_stage(COptions, ModuleNameQ, "pre3_branches",
                pre_pretty(!.Core), !.Pre, !IO),

            % 4. Check resource usage is okay.
            ResErrors = cord_list_to_cord(
                map(check_resources(!.Core), map.values(!.Pre))),
            add_errors(ResErrors, !Errors),
            maybe_dump_stage(COptions, ModuleNameQ, "pre4_resources",
                pre_pretty(!.Core), !.Pre, !IO),

            % 5. Transform the pre structure into an expression tree.
            %    TODO: Handle return statements in branches, where some
            %    branches fall-through and others don't.
            ( if is_empty(!.Errors) then
                map.foldl(pre_to_core, !.Pre, !Core)
            else
                true
            )
        )
    else
        true
    ).

:- pred process_procs(func(V) = result(V, E), map(K, V), map(K, V),
    errors(E), errors(E)).
:- mode process_procs(func(in) = (out) is det, in, out, in, out) is det.

process_procs(Func, !Map, !Errors) :-
    map.map_values_foldl(process_proc(Func), !Map, !Errors).

:- pred process_proc(func(V) = result(V, E), V, V, errors(E), errors(E)).
:- mode process_proc(func(in) = (out) is det, in, out, in, out) is det.

process_proc(Func, !Proc, !Errors) :-
    Result = Func(!.Proc),
    ( Result = ok(!:Proc)
    ; Result = errors(NewErrors),
        add_errors(NewErrors, !Errors)
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

:- pred gather_funcs(exports::in, ast_entry::in, core::in, core::out,
    env::in, env::out,
    errors(compile_error)::in, errors(compile_error)::out) is det.

gather_funcs(_, ast_export(_), !Core, !Env, !Errors).
gather_funcs(_, ast_import(_, _), !Core, !Env, !Errors).
gather_funcs(_, ast_type(_, _, _, _), !Core, !Env, !Errors).
gather_funcs(_, ast_resource(_, _), !Core, !Env, !Errors).
gather_funcs(Exports, ast_function(Name, Params, Returns, Uses0, _, Context),
        !Core, !Env, !Errors) :-
    ( if
        core_allocate_function(FuncId, !Core),
        % Add the function to the environment with it's local name, since
        % we're in the scope of the module already.
        env_add_func(q_name(Name), FuncId, !Env)
    then
        ( if Name = "main" then
            core_set_entry_function(FuncId, !Core)
        else
            true
        ),

        % Build basic information about the function.
        Sharing = sharing(Exports, Name),
        ParamTypesResult = result_list_to_result(
            map(build_param_type(!.Env), Params)),
        ReturnTypeResults = map(build_type_ref(!.Env, dont_check_type_vars),
            Returns),
        ReturnTypesResult = result_list_to_result(ReturnTypeResults),
        foldl2(build_uses(!.Env), Uses0, set.init, Uses, set.init, Observes),
        IntersectUsesObserves = intersect(Uses, Observes),
        ( if
            ParamTypesResult = ok(ParamTypes),
            ReturnTypesResult = ok(ReturnTypes),
            is_empty(IntersectUsesObserves)
        then
            QName = q_name_snoc(module_name(!.Core), Name),
            Function = func_init(QName, Context, Sharing, ParamTypes,
                ReturnTypes, Uses, Observes),
            core_set_function(FuncId, Function, !Core)
        else
            ( if ParamTypesResult = errors(ParamTypesErrors) then
                add_errors(ParamTypesErrors, !Errors)
            else
                true
            ),
            ( if ReturnTypesResult = errors(ReturnTypesErrors) then
                add_errors(ReturnTypesErrors, !Errors)
            else
                true
            ),
            ( if not is_empty(IntersectUsesObserves) then
                Resources = list.map(core_get_resource(!.Core),
                    set.to_sorted_list(IntersectUsesObserves)),
                add_error(Context, ce_uses_observes_not_distinct(Resources),
                    !Errors)
            else
                true
            )
        )
    else
        add_error(Context, ce_function_already_defined(Name), !Errors)
    ).

:- func sharing(exports, string) = sharing.

sharing(export_all, _) = s_public.
sharing(exports(Exports), Name) =
    ( if member(Name, Exports) then
        s_public
    else
        s_private
    ).

:- func build_param_type(env, ast_param) = result(type_, compile_error).

build_param_type(Env, ast_param(_, Type)) =
    build_type_ref(Env, dont_check_type_vars, Type).

:- type check_type_vars
            % Should check that each type variable is in the given set.
    --->    check_type_vars(set(string))

            % Don't check, because this type expression is not part of a
            % type declaration.
    ;       dont_check_type_vars.

:- func build_type_ref(env, check_type_vars, ast_type_expr) =
    result(type_, compile_error).

build_type_ref(Env, CheckVars, ast_type(Qualifiers, Name, Args0, Context)) =
        Result :-
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
        ArgsResult = result_list_to_result(
            map(build_type_ref(Env, CheckVars), Args0)),
        ( ArgsResult = ok(Args),
            ( if
                env_search_type(Env, q_name(Qualifiers, Name), TypeId,
                    TypeArity)
            then
                ( if length(Args) = TypeArity ^ a_num then
                    Result = ok(type_ref(TypeId, Args))
                else
                    Result = return_error(Context,
                        ce_type_has_incorrect_num_of_args(Name,
                            TypeArity ^ a_num, length(Args)))
                )
            else
                Result = return_error(Context,
                    ce_type_not_known(Name))
            )
        ; ArgsResult = errors(Error),
            Result = errors(Error)
        )
    ).
build_type_ref(Env, MaybeCheckVars, ast_type_func(Args0, Returns0, Uses, _)) =
        Result :-
    ( Uses = []
    ; Uses = [_ | _],
        util.sorry($file, $pred, "Uses")
    ),
    ArgsResult = result_list_to_result(
        map(build_type_ref(Env, MaybeCheckVars), Args0)),
    ReturnsResult = result_list_to_result(
        map(build_type_ref(Env, MaybeCheckVars), Returns0)),
    (
        ArgsResult = ok(Args),
        ReturnsResult = ok(Returns),
        Result = ok(func_type(Args, Returns))
    ;
        ArgsResult = ok(_),
        ReturnsResult = errors(Errors),
        Result = errors(Errors)
    ;
        ArgsResult = errors(Errors),
        ReturnsResult = ok(_),
        Result = errors(Errors)
    ;
        ArgsResult = errors(ArgsErrors),
        ReturnsResult = errors(ReturnsErrors),
        Result = errors(ArgsErrors ++ ReturnsErrors)
    ).
build_type_ref(_, MaybeCheckVars, ast_type_var(Name, _Context)) = Result :-
    ( if
        MaybeCheckVars = check_type_vars(CheckVars) =>
        member(Name, CheckVars)
    then
        Result = ok(type_variable(Name))
    else
        compile_error($file, $pred, "Unknown type variable")
    ).

:- pred build_uses(env::in, ast_uses::in,
    set(resource_id)::in, set(resource_id)::out,
    set(resource_id)::in, set(resource_id)::out) is det.

build_uses(Env, ast_uses(Type, ResourceName), !Uses, !Observes) :-
    ( if env_search_resource(Env, q_name(ResourceName), ResourcePrime) then
        Resource = ResourcePrime
    else
        compile_error($file, $pred, "Unknown resource")
    ),
    ( Type = ut_uses,
        !:Uses = set.insert(!.Uses, Resource)
    ; Type = ut_observes,
        !:Observes = set.insert(!.Observes, Resource)
    ).

%-----------------------------------------------------------------------%

:- pred func_to_pre(env::in, ast_entry::in,
    map(func_id, pre_procedure)::in, map(func_id, pre_procedure)::out,
    errors(compile_error)::in, errors(compile_error)::out) is det.

func_to_pre(_, ast_export(_), !Pre, !Errors).
func_to_pre(_, ast_import(_, _), !Pre, !Errors).
func_to_pre(_, ast_type(_, _, _, _), !Pre, !Errors).
func_to_pre(_, ast_resource(_, _), !Pre, !Errors).
func_to_pre(Env0, ast_function(Name, Params, Returns, _, Body0, Context),
        !Pre, !Errors) :-
    env_lookup_function(Env0, q_name(Name), FuncId),

    % Build body.
    ParamNames = map((func(ast_param(N, _)) = N), Params),
    some [!Varmap] (
        !:Varmap = varmap.init,
        ( if
            map_foldl2(env_add_var_or_wildcard, ParamNames,
                ParamVarsOrWildcardsPrime, Env0, EnvPrime, !Varmap)
        then
            ParamVarsOrWildcards = ParamVarsOrWildcardsPrime,
            Env = EnvPrime
        else
            compile_error($file, $pred, Context,
                "Two or more parameters have the same name")
        ),
        ast_to_pre(Env, Body0, Body, !Varmap),
        Proc = pre_procedure(FuncId, !.Varmap, ParamVarsOrWildcards,
            arity(length(Returns)), Body, Context),
        map.det_insert(FuncId, Proc, !Pre)
    ).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
