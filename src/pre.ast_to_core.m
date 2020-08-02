%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module pre.ast_to_core.
%
% Copyright (C) 2015-2020 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
% Plasma parse tree to core representation conversion
%
%-----------------------------------------------------------------------%

:- interface.

:- import_module io.

:- import_module ast.
:- import_module common_types.
:- import_module compile_error.
:- import_module core.
:- import_module core.function.
:- import_module options.
:- import_module pre.env.
:- import_module q_name.
:- import_module util.
:- import_module util.result.

%-----------------------------------------------------------------------%

:- type process_definitions
    --->    process_only_declarations
    ;       process_declarations_and_definitions.

:- pred ast_to_core(general_options::in, process_definitions::in, ast::in,
    result_partial(core, compile_error)::out, io::di, io::uo) is det.

% Exported for pre.import's use.
:- pred ast_to_func_decl(core::in, env::in, q_name::in, ast_function_decl::in,
    sharing::in, result(function, compile_error)::out) is det.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module cord.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module require.
:- import_module set.
:- import_module string.

:- import_module builtins.
:- import_module constant.
:- import_module context.
:- import_module core.resource.
:- import_module core.types.
:- import_module dump_stage.
:- import_module pre.bang.
:- import_module pre.branches.
:- import_module pre.closures.
:- import_module pre.from_ast.
:- import_module pre.import.
:- import_module pre.pre_ds.
:- import_module pre.pretty.
:- import_module pre.to_core.
:- import_module util.exception.
:- import_module util.path.
:- import_module varmap.

%-----------------------------------------------------------------------%

ast_to_core(GOptions, ProcessDefinitions, ast(ModuleName, Context, Entries),
        Result, !IO) :-
    some [!Env, !Core, !Errors] (
        !:Errors = init,

        check_module_name(GOptions, Context, ModuleName, !Errors),

        !:Core = core.init(ModuleName),

        setup_builtins(BuiltinMap, BoolTrue, BoolFalse, ListType,
            ListNil, ListCons, !Core),

        InitEnv = env.init(BoolTrue, BoolFalse, ListType, ListNil, ListCons),
        map.foldl(env_add_builtin(q_name), BuiltinMap, InitEnv, !:Env),

        filter_entries(Entries, Imports, Resources, Types, Funcs),

        ( ProcessDefinitions = process_declarations_and_definitions,
            % We create a second environment, this one is used only for reading
            % interface files.
            map.foldl(env_add_builtin(func(Name) =
                    q_name_append(builtin_module_name, Name)
                ), BuiltinMap, InitEnv, ImportEnv),
            ast_to_core_imports(ImportEnv, Imports, !Env, !Core, !Errors, !IO)
        ; ProcessDefinitions = process_only_declarations
        ),

        ast_to_core_resources(Resources, !Env, !Core, !Errors),

        ast_to_core_types(Types, !Env, !Core, !Errors),

        foldl3(gather_funcs, Funcs, !Core, !Env, !Errors),
        ( if not has_fatal_errors(!.Errors) then
            ( ProcessDefinitions = process_declarations_and_definitions,
                ast_to_core_funcs(GOptions, ModuleName, Funcs, !.Env,
                    !Core, !Errors, !IO),
                ( if not has_fatal_errors(!.Errors) then
                    Result = ok(!.Core, !.Errors)
                else
                    Result = errors(!.Errors)
                )
            ; ProcessDefinitions = process_only_declarations,
                % Our caller doesn't need us to process function
                % definitions, we're probably building a module interface
                % only.
                Result = ok(!.Core, !.Errors)
            )
        else
            Result = errors(!.Errors)
        )
    ).

:- pred check_module_name(general_options::in, context::in, q_name::in,
    errors(compile_error)::in, errors(compile_error)::out) is det.

check_module_name(GOptions, Context, ModuleName, !Errors) :-
    % The module name and file name are both converted to an internal
    % representation and then compared lexicographically.  If that matches
    % then they match.  This allows the file name to vary with case and
    % punctuation differences.

    ModuleNameStr = q_name_to_string(ModuleName),
    ( if not is_all_alnum_or_underscore(ModuleNameStr) then
        % This check should be lifted later for submodules, but for now it
        % prevents punctuation within module names.  In the future we need
        % to allow other scripts also.
        add_error(Context, ce_invalid_module_name(ModuleNameStr), !Errors)
    else
        true
    ),

    ModuleNameStripped = strip_file_name_punctuation(ModuleNameStr),

    InputFileName = GOptions ^ go_input_file,
    filename_extension(source_extension, InputFileName, InputFileNameBase),
    ( if
        strip_file_name_punctuation(InputFileNameBase) \= ModuleNameStripped
    then
        add_error(Context,
            ce_source_file_name_not_match_module(ModuleNameStr, InputFileName),
            !Errors)
    else
        true
    ),

    OutputFileName = GOptions ^ go_output_file,
    ( if
        ( Extension = output_extension
        ; Extension = interface_extension
        ),
        filename_extension(Extension, OutputFileName, OutputFileNameBase),
        strip_file_name_punctuation(OutputFileNameBase) = ModuleNameStripped
    then
        true
    else
        add_error(Context, ce_object_file_name_not_match_module(ModuleNameStr,
            OutputFileName), !Errors)
    ).

:- pred env_add_builtin((func(T) = q_name)::in, T::in, builtin_item::in,
    env::in, env::out) is det.

    % Resources and types arn't copied into the new namespace with
    % env_import_star.  But that's okay because that actually needs
    % replacing in the future so will fix this then (TODO).
    %
env_add_builtin(MakeName, Name, bi_func(FuncId), !Env) :-
    env_add_func_det(MakeName(Name), FuncId, !Env).
env_add_builtin(MakeName, Name, bi_ctor(CtorId), !Env) :-
    env_add_constructor(MakeName(Name), CtorId, !Env).
env_add_builtin(MakeName, Name, bi_resource(ResId), !Env) :-
    env_add_resource_det(MakeName(Name), ResId, !Env).
env_add_builtin(MakeName, Name, bi_type(TypeId, Arity), !Env) :-
    env_add_type_det(MakeName(Name), Arity, TypeId, !Env).
env_add_builtin(MakeName, Name, bi_type_builtin(Builtin), !Env) :-
    env_add_builtin_type_det(MakeName(Name), Builtin, !Env).

:- pred filter_entries(list(ast_entry)::in, list(ast_import)::out,
    list(named(ast_resource))::out, list(named(ast_type))::out,
    list(named(ast_function))::out) is det.

filter_entries([], [], [], [], []).
filter_entries([E | Es], Is, Rs, Ts, Fs) :-
    filter_entries(Es, Is0, Rs0, Ts0, Fs0),
    ( E = ast_import(I),
        Is = [I | Is0],
        Rs = Rs0,
        Ts = Ts0,
        Fs = Fs0
    ; E = ast_resource(N, R),
        Is = Is0,
        Rs = [named(N, R) | Rs0],
        Ts = Ts0,
        Fs = Fs0
    ; E = ast_type(N, T),
        Is = Is0,
        Rs = Rs0,
        Ts = [named(N, T) | Ts0],
        Fs = Fs0
    ; E = ast_function(N, F),
        Is = Is0,
        Rs = Rs0,
        Ts = Ts0,
        Fs = [named(N, F) | Fs0]
    ).

%-----------------------------------------------------------------------%

:- pred ast_to_core_types(list(named(ast_type))::in, env::in, env::out,
    core::in, core::out,
    errors(compile_error)::in, errors(compile_error)::out) is det.

ast_to_core_types(Types, !Env, !Core, !Errors) :-
    foldl2(gather_type, Types, !Env, !Core),
    foldl3(ast_to_core_type, Types, !Env, !Core, !Errors).

:- pred gather_type(named(ast_type)::in, env::in, env::out, core::in, core::out)
    is det.

gather_type(named(Name, ast_type(Params, _, _)), !Env, !Core) :-
    Arity = arity(length(Params)),
    core_allocate_type_id(TypeId, !Core),
    ( if env_add_type(q_name(Name), Arity, TypeId, !Env) then
        true
    else
        compile_error($file, $pred, "Type already defined")
    ).

:- pred ast_to_core_type(named(ast_type)::in, env::in, env::out,
    core::in, core::out,
    errors(compile_error)::in, errors(compile_error)::out) is det.

ast_to_core_type(named(Name, ast_type(Params, Constrs0, _Context)),
        !Env, !Core, !Errors) :-
    % Check that each parameter is unique.
    foldl(check_param, Params, init, ParamsSet),

    env_lookup_type(!.Env, q_name(Name), Type),
    ( Type = te_id(TypeId, _)
    ; Type = te_builtin(_),
        unexpected($file, $pred, "What happens here?")
    ),
    map_foldl2(ast_to_core_type_constructor(TypeId, Params, ParamsSet),
        Constrs0, CtorIdResults, !Env, !Core),
    CtorIdsResult = result_list_to_result(CtorIdResults),
    ( CtorIdsResult = ok(CtorIds),
        FullName = q_name_append(module_name(!.Core), Name),
        core_set_type(TypeId, init(FullName, Params, CtorIds), !Core)
    ; CtorIdsResult = errors(Errors),
        add_errors(Errors, !Errors)
    ).

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
        at_constructor(Symbol, Fields0, _), Result, !Env, !Core) :-
    % TODO: Constructors in the environment may need to handle their arity.
    env_search(!.Env, q_name(Symbol), MaybeEntry),
    ( MaybeEntry = ok(Entry),
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
            compile_error($file, $pred,
                "Constructor name already used by other value")
        )
    ; MaybeEntry = not_found,
        % TODO: we're converting a nq_name to a q_name without adding a
        % module name. Other modules won't be able to find this constructor
        % like this.
        env_add_constructor(q_name(Symbol), CtorId, !Env),
        core_allocate_ctor_id(CtorId,
            q_name_append(module_name(!.Core), Symbol), !Core)
    ;
        ( MaybeEntry = not_initaliased
        ; MaybeEntry = inaccessible
        ; MaybeEntry = maybe_cyclic_retlec
        ),
        compile_error($file, $pred,
            "Constructor name already used by other value")
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
    Symbol = q_name_single(Name),
    TypeResult = build_type_ref(Env, check_type_vars(ParamsSet), Type0),
    ( TypeResult = ok(Type),
        Result = ok(type_field(Symbol, Type))
    ; TypeResult = errors(Errors),
        Result = errors(Errors)
    ).

%-----------------------------------------------------------------------%

:- pred ast_to_core_resources(list(named(ast_resource))::in, env::in, env::out,
    core::in, core::out,
    errors(compile_error)::in, errors(compile_error)::out) is det.

ast_to_core_resources(Resources, !Env, !Core, !Errors) :-
    foldl2(gather_resource, Resources, !Env, !Core),
    foldl2(ast_to_core_resource(!.Env), Resources, !Core, !Errors).

:- pred gather_resource(named(ast_resource)::in, env::in, env::out,
    core::in, core::out) is det.

gather_resource(named(Name, _), !Env, !Core) :-
    core_allocate_resource_id(Res, !Core),
    ( if env_add_resource(q_name(Name), Res, !Env) then
        true
    else
        compile_error($file, $pred, "Resource already defined")
    ).

:- pred ast_to_core_resource(env::in, named(ast_resource)::in,
    core::in, core::out,
    errors(compile_error)::in, errors(compile_error)::out) is det.

ast_to_core_resource(Env, named(Name, ast_resource(FromName)), !Core,
        !Errors) :-
    env_lookup_resource(Env, q_name(Name), Res),
    ( if
        env_search_resource(Env, FromName, FromRes)
    then
        FullName = q_name_append(module_name(!.Core), Name),
        core_set_resource(Res, r_other(FullName, FromRes), !Core)
    else
        compile_error($file, $pred, "From resource not known")
    ).

%-----------------------------------------------------------------------%

:- pred ast_to_core_funcs(general_options::in, q_name::in,
    list(named(ast_function))::in, env::in, core::in, core::out,
    errors(compile_error)::in, errors(compile_error)::out, io::di, io::uo)
    is det.

ast_to_core_funcs(GOptions, ModuleName, Funcs, Env, !Core, !Errors, !IO) :-
    some [!Pre] (
        % 1. the func_to_pre step resolves symbols, builds a varmap,
        % builds var-use and var-def sets.
        list.foldl(func_to_pre(Env), Funcs, map.init, !:Pre),
        maybe_dump_stage(GOptions, ModuleName, "pre1_initial",
            pre_pretty(!.Core), !.Pre, !IO),

        % 2. Annotate closures with captured variable information
        map.map_values_only(compute_closures, !Pre),
        maybe_dump_stage(GOptions, ModuleName, "pre2_closures",
            pre_pretty(!.Core), !.Pre, !IO),

        % 3. Fixup how variables are used in branching code, this pass:
        %    * checks that used variables are always well defined (eg
        %      along all execution paths)
        %    * Updates the reachability information for branches.
        %      Reachability information is incomplete until after
        %      typechecking.
        %    * Adds terminating "return" statements where needed.
        %
        process_procs(fix_branches, !Pre, !Errors),
        maybe_dump_stage(GOptions, ModuleName, "pre3_branches",
            pre_pretty(!.Core), !.Pre, !IO),

        % 4. Check bang placment is okay
        ResErrors = cord_list_to_cord(
            map(check_bangs(!.Core), map.values(!.Pre))),
        add_errors(ResErrors, !Errors),
        maybe_dump_stage(GOptions, ModuleName, "pre4_resources",
            pre_pretty(!.Core), !.Pre, !IO),

        % 5. Transform the pre structure into an expression tree.
        %    TODO: Handle return statements in branches, where some
        %    branches fall-through and others don't.
        ( if not has_fatal_errors(!.Errors) then
            map.foldl(pre_to_core, !.Pre, !Core)
        else
            true
        )
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

:- pred gather_funcs(named(ast_function)::in, core::in, core::out,
    env::in, env::out,
    errors(compile_error)::in, errors(compile_error)::out) is det.

gather_funcs(named(Name, Func), !Core, !Env, !Errors) :-
    gather_funcs_defn(top_level, Name, Func, !Core, !Env, !Errors).

:- type level
    --->    top_level
    ;       nested.

:- pred gather_funcs_defn(level::in, nq_name::in, ast_function::in,
    core::in, core::out, env::in, env::out,
    errors(compile_error)::in, errors(compile_error)::out) is det.

gather_funcs_defn(Level, Name0, ast_function(Decl, Body, Sharing), !Core, !Env,
        !Errors) :-
    Context = Decl ^ afd_context,
    ( Level = top_level,
        NameStr = nq_name_to_string(Name0),
        Name = Name0
    ; Level = nested,
        NameStr = clobber_lambda(nq_name_to_string(Name0), Context),
        Name = nq_name_det(NameStr)
    ),

    ( if
        core_allocate_function(FuncId, !Core),
        ( Level = top_level,
            % Add the function to the environment with it's local name,
            % since we're in the scope of the module already.
            env_add_func(q_name(Name), FuncId, !Env)
        ; Level = nested,
            env_add_lambda(NameStr, FuncId, !Env)
        )
    then
        QName = q_name_append(module_name(!.Core), Name),
        ast_to_func_decl(!.Core, !.Env, QName, Decl, Sharing, MaybeFunction),
        ( MaybeFunction = ok(Function),
            core_set_function(FuncId, Function, !Core),
            ( if
                Level = top_level,
                Sharing = s_public,
                Name = nq_name_det("main")
            then
                func_get_type_signature(Function, Params, Returns, _),
                ListTypeId = env_get_list_type(!.Env),
                ( if
                    Returns = [builtin_type(int)],
                    ( Params = [],
                        Entrypoint = entry_plain(FuncId)
                    ; Params = [type_ref(ListTypeId, [builtin_type(string)])],
                        Entrypoint = entry_argv(FuncId)
                    )
                then
                    core_set_entry_function(Entrypoint, !Core)
                else
                    add_error(Context, ce_main_function_wrong_signature,
                        !Errors)
                )
            else
                true
            )
        ; MaybeFunction = errors(Errors),
            add_errors(Errors, !Errors)
        )
    else
        add_error(Context, ce_function_already_defined(NameStr), !Errors)
    ),

    foldl3(gather_funcs_block, Body, !Core, !Env, !Errors).

ast_to_func_decl(Core, Env, Name, Decl, Sharing, Result) :-
    Decl = ast_function_decl(Params, Returns, Uses0, Context),
    % Build basic information about the function.
    ParamTypesResult = result_list_to_result(
        map(build_param_type(Env), Params)),
    ReturnTypeResults = map(build_type_ref(Env, dont_check_type_vars),
        Returns),
    ReturnTypesResult = result_list_to_result(ReturnTypeResults),
    map_foldl2(build_uses(Context, Env), Uses0, ResourceErrorss,
        set.init, Uses, set.init, Observes),
    ResourceErrors = cord_list_to_cord(ResourceErrorss),
    IntersectUsesObserves = intersect(Uses, Observes),
    ( if
        ParamTypesResult = ok(ParamTypes),
        ReturnTypesResult = ok(ReturnTypes),
        is_empty(ResourceErrors),
        is_empty(IntersectUsesObserves)
    then
        Function = func_init_user(Name, Context, Sharing, ParamTypes,
            ReturnTypes, Uses, Observes),
        Result = ok(Function)
    else
        some [!Errors] (
            !:Errors = init,
            add_errors_from_result(ParamTypesResult, !Errors),
            add_errors_from_result(ReturnTypesResult, !Errors),
            add_errors(ResourceErrors, !Errors),
            ( if not is_empty(IntersectUsesObserves) then
                Resources = list.map(core_get_resource(Core),
                    set.to_sorted_list(IntersectUsesObserves)),
                add_error(Context, ce_uses_observes_not_distinct(Resources),
                    !Errors)
            else
                true
            ),
            Result = errors(!.Errors)
        )
    ).

:- pred gather_funcs_block(ast_block_thing::in,
    core::in, core::out, env::in, env::out,
    errors(compile_error)::in, errors(compile_error)::out) is det.

gather_funcs_block(astbt_statement(Stmt), !Core, !Env, !Errors) :-
    ast_statement(Type, _) = Stmt,
    gather_funcs_stmt(Type, !Core, !Env, !Errors).
gather_funcs_block(astbt_function(Name, Defn), !Core, !Env, !Errors) :-
    gather_funcs_defn(nested, Name, Defn, !Core, !Env, !Errors).

:- pred gather_funcs_stmt(ast_stmt_type(context)::in,
    core::in, core::out, env::in, env::out,
    errors(compile_error)::in, errors(compile_error)::out) is det.

gather_funcs_stmt(s_call(Call), !Core, !Env, !Errors) :-
    gather_funcs_call(Call, !Core, !Env, !Errors).
gather_funcs_stmt(s_assign_statement(_, Expr), !Core, !Env, !Errors) :-
    gather_funcs_expr(Expr, !Core, !Env, !Errors).
gather_funcs_stmt(s_vars_statement(_, MaybeExpr), !Core, !Env, !Errors) :-
    ( MaybeExpr = yes(Expr),
        gather_funcs_expr(Expr, !Core, !Env, !Errors)
    ; MaybeExpr = no
    ).
gather_funcs_stmt(s_array_set_statement(_, ExprA, ExprB), !Core, !Env,
        !Errors) :-
    gather_funcs_expr(ExprA, !Core, !Env, !Errors),
    gather_funcs_expr(ExprB, !Core, !Env, !Errors).
gather_funcs_stmt(s_return_statement(Exprs), !Core, !Env, !Errors) :-
    foldl3(gather_funcs_expr, Exprs, !Core, !Env, !Errors).
gather_funcs_stmt(s_match_statement(Expr, Cases), !Core, !Env, !Errors) :-
    gather_funcs_expr(Expr, !Core, !Env, !Errors),
    foldl3(gather_funcs_case, Cases, !Core, !Env, !Errors).
gather_funcs_stmt(s_ite(Cond, Then, Else), !Core, !Env, !Errors) :-
    gather_funcs_expr(Cond, !Core, !Env, !Errors),
    foldl3(gather_funcs_block, Then, !Core, !Env, !Errors),
    foldl3(gather_funcs_block, Else, !Core, !Env, !Errors).

:- pred gather_funcs_case(ast_match_case::in,
    core::in, core::out, env::in, env::out,
    errors(compile_error)::in, errors(compile_error)::out) is det.

gather_funcs_case(ast_match_case(_, Block), !Core, !Env, !Errors) :-
    foldl3(gather_funcs_block, Block, !Core, !Env, !Errors).

:- pred gather_funcs_call(ast_call_like::in,
    core::in, core::out, env::in, env::out,
    errors(compile_error)::in, errors(compile_error)::out) is det.

gather_funcs_call(Call, !Core, !Env, !Errors) :-
    ( Call = ast_call_like(Callee, Args)
    ; Call = ast_bang_call(Callee, Args)
    ),
    gather_funcs_expr(Callee, !Core, !Env, !Errors),
    foldl3(gather_funcs_expr, Args, !Core, !Env, !Errors).

:- pred gather_funcs_expr(ast_expression::in,
    core::in, core::out, env::in, env::out,
    errors(compile_error)::in, errors(compile_error)::out) is det.

gather_funcs_expr(e_call_like(Call), !Core, !Env, !Errors) :-
    gather_funcs_call(Call, !Core, !Env, !Errors).
gather_funcs_expr(e_u_op(_, Expr), !Core, !Env, !Errors) :-
    gather_funcs_expr(Expr, !Core, !Env, !Errors).
gather_funcs_expr(e_b_op(Left, _, Right), !Core, !Env, !Errors) :-
    gather_funcs_expr(Left, !Core, !Env, !Errors),
    gather_funcs_expr(Right, !Core, !Env, !Errors).
gather_funcs_expr(e_symbol(_), !Core, !Env, !Errors).
gather_funcs_expr(e_const(_), !Core, !Env, !Errors).
gather_funcs_expr(e_array(Exprs), !Core, !Env, !Errors) :-
    foldl3(gather_funcs_expr, Exprs, !Core, !Env, !Errors).

%-----------------------------------------------------------------------%

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

build_type_ref(Env, CheckVars, ast_type(Name, Args0, Context)) = Result :-
    ArgsResult = result_list_to_result(
        map(build_type_ref(Env, CheckVars), Args0)),
    ( ArgsResult = ok(Args),
        ( if env_search_type(Env, Name, Type) then
            ( Type = te_builtin(BuiltinType),
                ( Args0 = [],
                    Result = ok(builtin_type(BuiltinType))
                ; Args0 = [_ | _],
                    Result = return_error(Context,
                        ce_builtin_type_with_args(q_name_to_string(Name)))
                )
            ; Type = te_id(TypeId, TypeArity),
                ( if length(Args) = TypeArity ^ a_num then
                    Result = ok(type_ref(TypeId, Args))
                else
                    Result = return_error(Context,
                        ce_type_has_incorrect_num_of_args(
                            q_name_to_string(Name),
                            TypeArity ^ a_num, length(Args)))
                )
            )
        else
            Result = return_error(Context,
                ce_type_not_known(q_name_to_string(Name)))
        )
    ; ArgsResult = errors(Error),
        Result = errors(Error)
    ).
build_type_ref(Env, MaybeCheckVars, Func) = Result :-
    Func = ast_type_func(Args0, Returns0, Uses0, Context),
    ArgsResult = result_list_to_result(
        map(build_type_ref(Env, MaybeCheckVars), Args0)),
    ReturnsResult = result_list_to_result(
        map(build_type_ref(Env, MaybeCheckVars), Returns0)),
    map_foldl2(build_uses(Context, Env), Uses0, ResourceErrorss,
        set.init, UsesSet, set.init, ObservesSet),
    ResourceErrors = cord_list_to_cord(ResourceErrorss),
    ( if
        ArgsResult = ok(Args),
        ReturnsResult = ok(Returns),
        is_empty(ResourceErrors)
    then
        Result = ok(func_type(Args, Returns, UsesSet, ObservesSet))
    else
        some [!Errors] (
            !:Errors = init,
            add_errors_from_result(ArgsResult, !Errors),
            add_errors_from_result(ReturnsResult, !Errors),
            add_errors(ResourceErrors, !Errors),
            Result = errors(!.Errors)
        )
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

:- pred build_uses(context::in, env::in, ast_uses::in,
    errors(compile_error)::out,
    set(resource_id)::in, set(resource_id)::out,
    set(resource_id)::in, set(resource_id)::out) is det.

build_uses(Context, Env, ast_uses(Type, ResourceName), Errors,
        !Uses, !Observes) :-
    ( if env_search_resource(Env, ResourceName, ResourcePrime) then
        Resource = ResourcePrime,
        Errors = init,
        ( Type = ut_uses,
            !:Uses = set.insert(!.Uses, Resource)
        ; Type = ut_observes,
            !:Observes = set.insert(!.Observes, Resource)
        )
    else
        Errors = error(Context,
            ce_resource_unknown(q_name_to_string(ResourceName)))
    ).

%-----------------------------------------------------------------------%

:- pred func_to_pre(env::in, named(ast_function)::in,
    map(func_id, pre_procedure)::in, map(func_id, pre_procedure)::out) is det.

func_to_pre(Env0, named(Name, Func), !Pre) :-
    Func = ast_function(ast_function_decl(Params, Returns, _, Context),
        Body, _),
    % The name parameter is the name in the environment and doesn't need to
    % be qualified.
    func_to_pre_func(Env0, q_name(Name), Params, Returns, Body, Context,
        !Pre).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
