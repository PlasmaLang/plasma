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

:- import_module io.

:- import_module ast.
:- import_module compile_error.
:- import_module core.
:- import_module options.
:- import_module result.

%-----------------------------------------------------------------------%

:- pred ast_to_core(compile_options::in, ast::in,
    result(core, compile_error)::out, io::di, io::uo) is det.

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
:- import_module util.

:- import_module dump_stage.
:- import_module pre.branches.
:- import_module pre.env.
:- import_module pre.from_ast.
:- import_module pre.nonlocals.
:- import_module pre.pre_ds.
:- import_module pre.pretty.
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

ast_to_core(COptions, ast(ModuleName, Entries), Result, !IO) :-
    Exports = gather_exports(Entries),
    some [!Core, !Errors] (
        !:Core = core.init(q_name(ModuleName)),
        !:Errors = init,

        setup_builtins(BuiltinMap, !Core),
        map.foldl(env_add_func, BuiltinMap, env.init, Env0),
        env_import_star(builtin_module_name, Env0, Env1),

        ast_to_core_types(Entries, Env1, Env, !Core, !Errors),

        ast_to_core_funcs(COptions, ModuleName, Exports, Entries, Env,
            !Core, !Errors, !IO),
        ( if is_empty(!.Errors) then
            Result = ok(!.Core)
        else
            Result = errors(!.Errors)
        )
    ).

%-----------------------------------------------------------------------%

:- pred ast_to_core_types(list(ast_entry)::in, env::in, env::out,
    core::in, core::out,
    errors(compile_error)::in, errors(compile_error)::out) is det.

ast_to_core_types(Entries, !Env, !Core, !Errors) :-
    foldl2(gather_types, Entries, !Core, !Errors),
    foldl3(ast_to_core_type, Entries, !Env, !Core, !Errors).

:- pred gather_types(ast_entry::in, core::in, core::out,
    errors(compile_error)::in, errors(compile_error)::out) is det.

gather_types(ast_export(_), !Core, !Errors).
gather_types(ast_import(_, _), !Core, !Errors).
gather_types(ast_type(Name, _, _, Context), !Core, !Errors) :-
    ( if core_register_type(q_name(Name), _, !Core) then
        true
    else
        add_error(Context, ce_type_already_defined(Name), !Errors)
    ).
gather_types(ast_function(_, _, _, _, _, _), !Core, !Errors).

:- pred ast_to_core_type(ast_entry::in, env::in, env::out,
    core::in, core::out,
    errors(compile_error)::in, errors(compile_error)::out) is det.

ast_to_core_type(ast_export(_), !Env, !Core, !Errors).
ast_to_core_type(ast_import(_, _), !Env, !Core, !Errors).
ast_to_core_type(ast_type(_Name, Params, Constrs0, _Context),
        !Env, !Core, !Errors) :-
    ( Params = [_ | _],
        util.sorry($file, $pred, "Parameterized type")
    ; Params = [],
        foldl2(ast_to_core_type_constructor, Constrs0, !Env, !Core)
    ).
ast_to_core_type(ast_function(_, _, _, _, _, _), !Env, !Core, !Errors).

:- pred ast_to_core_type_constructor(at_constructor::in,
    env::in, env::out, core::in, core::out) is det.

ast_to_core_type_constructor(at_constructor(Name, Fields, _), !Env, !Core) :-
    ( Fields = [],
        Symbol = q_name(Name),
        ( if core_register_constructor(Symbol, ConsIdPrime, !Core) then
            ConsId = ConsIdPrime
        else
            core_lookup_constructor(!.Core, Symbol, ConsId)
        ),
        env_add_constructor(Symbol, ConsId, !Env)
    ; Fields = [_ | _],
        util.sorry($file, $pred, "Non-enum types")
    ).

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
            list.foldl2(func_to_pre(Env, !.Core), Entries, map.init,
                !:Pre, !Errors),
            ModuleNameQ = q_name(ModuleName),
            maybe_dump_stage(COptions, ModuleNameQ, "pre0_initial",
                pre_pretty(!.Core), !.Pre, !IO),

            % 2. Determine nonlocals
            map.map_values_only(compute_nonlocals, !Pre),
            maybe_dump_stage(COptions, ModuleNameQ, "pre1_nonlocals",
                pre_pretty(!.Core), !.Pre, !IO),

            % 3. Fixup how variables are used in branching code, this pass:
            %    * fixes var-def sets
            %    * checks that used variables are always well defined (eg
            %      along all execution paths)
            %    * names-appart branch-local variables (from other
            %      branches).
            %
            % NOTE: This code is being actively worked on.  But it works for
            % some simple cases of control flow.
            %
            map.map_values_only(fix_branches, !Pre),
            maybe_dump_stage(COptions, ModuleNameQ, "pre2_branches",
                pre_pretty(!.Core), !.Pre, !IO),

            % 4. Transform the pre structure into an expression tree.
            %    TODO: Handle return statements in branches, where some
            %    branches fall-through and others don't.
            map.foldl(pre_to_core, !.Pre, !Core)
        )
    else
        true
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
gather_funcs(Exports, ast_function(Name, Params, Return, Using0, _, Context),
        !Core, !Env, !Errors) :-
    QName = q_name_snoc(module_name(!.Core), Name),
    ( if
        core_register_function(QName, FuncId, !Core),

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
            Function = func_init(Context, Sharing, ParamTypes, [ReturnType],
                Using, Observing),
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
        )
    then
        % Add the function to the environment with it's local name, since
        % we're in the scope of the module already.
        env_add_func(q_name(Name), FuncId, !Env)
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
        util.sorry($file, $pred, "Only IO resource is supported")
    ).

%-----------------------------------------------------------------------%

:- pred func_to_pre(env::in, core::in, ast_entry::in,
    map(func_id, pre_procedure)::in, map(func_id, pre_procedure)::out,
    errors(compile_error)::in, errors(compile_error)::out) is det.

func_to_pre(_, _, ast_export(_), !Pre, !Errors).
func_to_pre(_, _, ast_import(_, _), !Pre, !Errors).
func_to_pre(_, _, ast_type(_, _, _, _), !Pre, !Errors).
func_to_pre(Env0, Core, ast_function(Name, Params, _, _, Body0, Context),
        !Pre, !Errors) :-
    ModuleName = module_name(Core),
    det_core_lookup_function(Core, q_name_snoc(ModuleName, Name), FuncId),

    % Build body.
    ParamNames = map((func(ast_param(N, _)) = N), Params),
    some [!Varmap] (
        !:Varmap = varmap.init,
        ( if
            map_foldl2(env_add_var, ParamNames, ParamVarsPrime,
                Env0, EnvPrime, !Varmap)
        then
            ParamVars = ParamVarsPrime,
            Env = EnvPrime
        else
            compile_error($file, $pred, Context,
                "Two or more parameters have the same name")
        ),
        ast_to_pre(Env, Body0, Body, !Varmap),
        Proc = pre_procedure(!.Varmap, ParamVars, Body),
        map.det_insert(FuncId, Proc, !Pre)
    ).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
