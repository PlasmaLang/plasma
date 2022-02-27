%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module pre.ast_to_core.
%
% Copyright (C) 2015-2022 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
% Plasma parse tree to core representation conversion
%
%-----------------------------------------------------------------------%

:- interface.

:- import_module io.
:- import_module list.

:- import_module ast.
:- import_module common_types.
:- import_module compile_error.
:- import_module core.
:- import_module core.function.
:- import_module core.types.
:- import_module options.
:- import_module pre.env.
:- import_module q_name.
:- import_module util.
:- import_module util.result.

%-----------------------------------------------------------------------%

    % The informationa bout a resource we need for ast_to_core (a2c).
    %
:- type a2c_resource
    --->    a2c_resource(
                r_name      :: nq_name,
                r_id        :: resource_id,
                r_resource  :: ast_resource
            ).

:- type a2c_type
    --->    a2c_type(
                t_name      :: nq_name,
                t_id        :: type_id,
                t_type      :: ast_type(nq_name)
            ).

%-----------------------------------------------------------------------%

:- pred ast_to_core_declarations(general_options::in,
    list(a2c_resource)::in, list(a2c_type)::in,
    list(nq_named(ast_function))::in, env::in, env::out, core::in, core::out,
    errors(compile_error)::in, errors(compile_error)::out,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------%
% Exported for pre.import's use.
%

:- pred ast_to_func_decl(core::in, env::in, q_name::in, ast_function_decl::in,
    sharing::in, result(function, compile_error)::out) is det.

    % ast_to_core_type_i(GetCtorName, Env, TypeName, TypeId, Type, Result,
    %   !Core)
    %
    % The constructors in an AST Type have a polymorphic name type.  It
    % could be a q_name when reading from interfaces, or nq_name when
    % reading a local module.  The caller provides GetCtorName which will
    % turn it into the actual q_name used within the core representation
    % (not the environment).
    %
:- pred ast_to_core_type_i((func(Name) = q_name)::in, imported::in,
    env::in, q_name::in, type_id::in, ast_type(Name)::in,
    result({user_type, list(ctor_binding(Name))}, compile_error)::out,
    core::in, core::out) is det.

    % Map a constructor name to an ID, so that a caller can update the
    % environment.
    %
:- type ctor_binding(Name)
    --->    cb(
                cb_name     :: Name,
                cb_id       :: ctor_id
            ).

    % After processing declarations, call this to process the bodies of
    % functions.
    %
:- pred ast_to_core_funcs(general_options::in, q_name::in,
    list(nq_named(ast_function))::in, env::in, core::in, core::out,
    errors(compile_error)::in, errors(compile_error)::out, io::di, io::uo)
    is det.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module cord.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module string.

:- import_module builtins.
:- import_module constant.
:- import_module context.
:- import_module core.resource.
:- import_module dump_stage.
:- import_module pre.bang.
:- import_module pre.branches.
:- import_module pre.closures.
:- import_module pre.from_ast.
:- import_module pre.import.
:- import_module pre.pre_ds.
:- import_module pre.pretty.
:- import_module pre.to_core.
:- import_module util.my_exception.
:- import_module util.log.
:- import_module util.path.
:- import_module varmap.

%-----------------------------------------------------------------------%

ast_to_core_declarations(GOptions, Resources, Types, Funcs, !Env,
        !Core, !Errors, !IO) :-
    Verbose = GOptions ^ go_verbose,

    verbose_output(Verbose, "pre_to_core: Processing resources\n", !IO),
    ast_to_core_resources(Resources, !Env, !Core, !Errors),

    verbose_output(Verbose, "pre_to_core: Processing types\n", !IO),
    ast_to_core_types(Types, !Env, !Core, !Errors),

    verbose_output(Verbose, "pre_to_core: Processing function signatures\n",
        !IO),
    foldl3(gather_funcs, Funcs, !Core, !Env, !Errors),

    verbose_output(Verbose, "pre_to_core: Checking exports\n", !IO),
    add_errors(check_resource_exports(!.Core), !Errors),
    add_errors(check_type_exports(!.Core), !Errors),
    add_errors(check_function_exports(!.Core), !Errors).

%-----------------------------------------------------------------------%

:- pred ast_to_core_types(list(a2c_type)::in,
    env::in, env::out, core::in, core::out,
    errors(compile_error)::in, errors(compile_error)::out) is det.

ast_to_core_types(Types, !Env, !Core, !Errors) :-
    foldl3(ast_to_core_type, Types, !Env, !Core, !Errors).

:- pred ast_to_core_type(a2c_type::in, env::in, env::out, core::in, core::out,
    errors(compile_error)::in, errors(compile_error)::out) is det.

ast_to_core_type(a2c_type(Name, TypeId, ASTType), !Env, !Core, !Errors) :-
    ModuleName = module_name(!.Core),
    ast_to_core_type_i(q_name_append(ModuleName), i_local, !.Env,
        q_name_append(ModuleName, Name),
        TypeId, ASTType, Result, !Core),
    ( Result = ok({Type, Ctors}),
        core_set_type(TypeId, Type, !Core),
        foldl((pred(C::in, E0::in, E::out) is det :-
                % TODO: Constructors in the environment may need to handle
                % their arity.
                env_add_constructor(q_name(C ^ cb_name), C ^ cb_id, E0, E)
            ), Ctors, !Env)
    ; Result = errors(Errors),
        add_errors(Errors, !Errors)
    ).

ast_to_core_type_i(GetName, Imported, Env, Name, TypeId,
        ast_type(Params, Constrs0, Sharing, Context), Result, !Core) :-
    % Check that each parameter is unique.
    foldl(check_param, Params, init, ParamsSet),

    map_foldl2(
        ast_to_core_type_constructor(GetName, Env, TypeId, Params, ParamsSet),
        Constrs0, CtorResults, init, _, !Core),
    CtorsResult = result_list_to_result(CtorResults),
    ( CtorsResult = ok(Ctors),
        CtorIds = map(func(C) = C ^ cb_id, Ctors),
        Result = ok({
            type_init(Name, Params, CtorIds, Sharing, Imported, Context),
            Ctors})
    ; CtorsResult = errors(Errors),
        Result = errors(Errors)
    ).
ast_to_core_type_i(_, _, _, Name, _, ast_type_abstract(Arity, Context),
        Result, !Core) :-
    Result = ok({type_init_abstract(Name, Arity, Context), []}).

:- pred check_param(string::in, set(string)::in, set(string)::out) is det.

check_param(Param, !Params) :-
    ( if insert_new(Param, !Params) then
        true
    else
        compile_error($file, $pred, "Non unique type parameters")
    ).

:- pred ast_to_core_type_constructor((func(Name) = q_name)::in, env::in,
    type_id::in, list(string)::in, set(string)::in, at_constructor(Name)::in,
    result(ctor_binding(Name), compile_error)::out,
    set(q_name)::in, set(q_name)::out, core::in, core::out) is det.

ast_to_core_type_constructor(GetName, Env, Type, Params, ParamsSet,
        at_constructor(EnvSymbol, Fields0, Context), Result, !CtorNameSet,
        !Core) :-

    Symbol = GetName(EnvSymbol),
    ( if insert_new(Symbol, !CtorNameSet) then
        core_allocate_ctor_id(CtorId, !Core),

        map(ast_to_core_field(!.Core, Env, ParamsSet), Fields0, FieldResults),
        FieldsResult = result_list_to_result(FieldResults),
        ( FieldsResult = ok(Fields),
            Constructor = constructor(Symbol, Params, Fields),
            core_set_constructor(CtorId, Symbol, Type, Constructor, !Core),
            Result = ok(cb(EnvSymbol, CtorId))
        ; FieldsResult = errors(Errors),
            Result = errors(Errors)
        )
    else
        Result = return_error(Context, ce_type_duplicate_constructor(Symbol))
    ).

:- pred ast_to_core_field(core::in, env::in, set(string)::in,
    at_field::in, result(type_field, compile_error)::out) is det.

ast_to_core_field(Core, Env, ParamsSet, at_field(Name, Type0, _),
        Result) :-
    Symbol = q_name_single(Name),
    TypeResult = build_type_ref(Core, Env, s_private,
        check_type_vars(ParamsSet), Type0),
    ( TypeResult = ok(Type),
        Result = ok(type_field(Symbol, Type))
    ; TypeResult = errors(Errors),
        Result = errors(Errors)
    ).

%-----------------------------------------------------------------------%

:- pred ast_to_core_resources(list(a2c_resource)::in,
    env::in, env::out, core::in, core::out,
    errors(compile_error)::in, errors(compile_error)::out) is det.

ast_to_core_resources(Resources, !Env, !Core, !Errors) :-
    foldl2(ast_to_core_resource(!.Env), Resources, !Core, !Errors).

:- pred ast_to_core_resource(env::in, a2c_resource::in, core::in, core::out,
    errors(compile_error)::in, errors(compile_error)::out) is det.

ast_to_core_resource(Env,
        a2c_resource(Name, Res, ast_resource(FromName, Sharing, Context)),
        !Core, !Errors) :-
    ( if
        env_search_resource(Env, FromName, FromRes)
    then
        FullName = q_name_append(module_name(!.Core), Name),
        core_set_resource(Res,
            r_other(FullName, FromRes, Sharing, i_local, Context), !Core)
    else
        add_error(Context, ce_resource_unknown(FromName), !Errors)
    ).

%-----------------------------------------------------------------------%

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

:- pred gather_funcs(nq_named(ast_function)::in, core::in, core::out,
    env::in, env::out,
    errors(compile_error)::in, errors(compile_error)::out) is det.

gather_funcs(nq_named(Name, Func), !Core, !Env, !Errors) :-
    Func = ast_function(Decl, Body, Sharing, IsEntrypoint),
    Context = Decl ^ afd_context,
    NameStr = nq_name_to_string(Name),

    ( if
        core_allocate_function(FuncId, !Core),
        % Add the function to the environment with it's local name,
        % since we're in the scope of the module already.
        env_add_func(q_name(Name), FuncId, !Env)
    then
        QName = q_name_append(module_name(!.Core), Name),
        ast_to_func_decl(!.Core, !.Env, QName, Decl, Sharing, MaybeFunction),
        ( MaybeFunction = ok(Function0),
            ( Body = ast_body_block(_),
                Function = Function0
            ; Body = ast_body_foreign,
                func_set_foreign( Function0, Function)
            ),

            core_set_function(FuncId, Function, !Core),
            ( IsEntrypoint = is_entrypoint,
                expect(unify(Sharing, s_public), $file, $pred,
                    "entrypoints are always public"),
                func_get_type_signature(Function, Params, Returns, _),
                ListTypeId = env_operators(!.Env) ^ o_list_type,
                ( if
                    Returns = [builtin_type(int)],
                    ( Params = [],
                        Entrypoint = entry_plain(FuncId)
                    ; Params = [type_ref(ListTypeId, [builtin_type(string)])],
                        Entrypoint = entry_argv(FuncId)
                    )
                then
                    core_add_entry_function(Entrypoint, !Core)
                else
                    add_error(Context, ce_entry_function_wrong_signature,
                        !Errors)
                )
            ; IsEntrypoint = not_entrypoint
            )
        ; MaybeFunction = errors(Errors),
            add_errors(Errors, !Errors)
        )
    else
        add_error(Context, ce_function_already_defined(NameStr), !Errors)
    ),

    ( Body = ast_body_block(Block),
        foldl3(gather_funcs_block, Block, !Core, !Env, !Errors)
    ; Body = ast_body_foreign
    ).

:- pred gather_nested_funcs(nq_name::in, ast_nested_function::in,
    core::in, core::out, env::in, env::out,
    errors(compile_error)::in, errors(compile_error)::out) is det.

gather_nested_funcs(Name0, ast_nested_function(Decl, Body),
        !Core, !Env, !Errors) :-
    Context = Decl ^ afd_context,
    NameStr = mangle_lambda(nq_name_to_string(Name0), Context),
    Name = nq_name_det(NameStr),

    core_allocate_function(FuncId, !Core),
    env_add_lambda(NameStr, FuncId, !Env),

    QName = q_name_append(module_name(!.Core), Name),
    ast_to_func_decl(!.Core, !.Env, QName, Decl, s_private, MaybeFunction),
    ( MaybeFunction = ok(Function),
        core_set_function(FuncId, Function, !Core)
    ; MaybeFunction = errors(Errors),
        add_errors(Errors, !Errors)
    ),

    foldl3(gather_funcs_block, Body, !Core, !Env, !Errors).

ast_to_func_decl(Core, Env, Name, Decl, Sharing, Result) :-
    Decl = ast_function_decl(Params, Returns, Uses0, Context),
    % Build basic information about the function.
    ParamTypesResult = result_list_to_result(
        map(build_param_type(Core, Env, Sharing), Params)),
    ReturnTypeResults = map(
        build_type_ref(Core, Env, Sharing, dont_check_type_vars),
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
    gather_nested_funcs(Name, Defn, !Core, !Env, !Errors).

:- pred gather_funcs_stmt(ast_stmt_type(context)::in,
    core::in, core::out, env::in, env::out,
    errors(compile_error)::in, errors(compile_error)::out) is det.

gather_funcs_stmt(s_call(Call), !Core, !Env, !Errors) :-
    gather_funcs_call(Call, !Core, !Env, !Errors).
gather_funcs_stmt(s_assign_statement(_, Exprs), !Core, !Env, !Errors) :-
    foldl3(gather_funcs_expr, Exprs, !Core, !Env, !Errors).
gather_funcs_stmt(s_var_statement(_), !Core, !Env, !Errors).
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
gather_funcs_expr(e_if(Cond, Then, Else), !Core, !Env, !Errors) :-
    gather_funcs_expr(Cond, !Core, !Env, !Errors),
    foldl3(gather_funcs_expr, Then, !Core, !Env, !Errors),
    foldl3(gather_funcs_expr, Else, !Core, !Env, !Errors).
gather_funcs_expr(e_match(Expr, Cases), !Core, !Env, !Errors) :-
    gather_funcs_expr(Expr, !Core, !Env, !Errors),
    foldl3(gather_funcs_expr_case, Cases, !Core, !Env, !Errors).
gather_funcs_expr(e_symbol(_), !Core, !Env, !Errors).
gather_funcs_expr(e_const(_), !Core, !Env, !Errors).
gather_funcs_expr(e_array(Exprs), !Core, !Env, !Errors) :-
    foldl3(gather_funcs_expr, Exprs, !Core, !Env, !Errors).

:- pred gather_funcs_expr_case(ast_expr_match_case::in,
    core::in, core::out, env::in, env::out,
    errors(compile_error)::in, errors(compile_error)::out) is det.

gather_funcs_expr_case(ast_emc(_, Exprs), !Core, !Env, !Errors) :-
    foldl3(gather_funcs_expr, Exprs, !Core, !Env, !Errors).

%-----------------------------------------------------------------------%

:- func build_param_type(core, env, sharing, ast_param) =
    result(type_, compile_error).

build_param_type(Core, Env, Sharing, ast_param(_, Type)) =
    build_type_ref(Core, Env, Sharing, dont_check_type_vars, Type).

:- type check_type_vars
            % Should check that each type variable is in the given set.
    --->    check_type_vars(set(string))

            % Don't check, because this type expression is not part of a
            % type declaration.
    ;       dont_check_type_vars.

    % build_type_ref(Core, Env, ParentSharing, Check, AstType) = Res,
    %
    % Build a type for this ast type expression.  If the expression occurs
    % in an exported function declaration then ParentSharing should be
    % s_public.
    %
:- func build_type_ref(core, env, sharing, check_type_vars, ast_type_expr) =
    result(type_, compile_error).

build_type_ref(Core, Env, Sharing, CheckVars, ast_type(Name, Args0, Context)) =
        Result :-
    ArgsResult = result_list_to_result(
        map(build_type_ref(Core, Env, Sharing, CheckVars), Args0)),
    ( ArgsResult = ok(Args),
        ( if env_search_type(Env, Name, Type) then
            ( Type = te_builtin(BuiltinType),
                ( Args0 = [],
                    Result = ok(builtin_type(BuiltinType))
                ; Args0 = [_ | _],
                    Result = return_error(Context,
                        ce_builtin_type_with_args(Name))
                )
            ; Type = te_id(TypeId, TypeArity),
                ( if length(Args) = TypeArity ^ a_num then
                    Result = ok(type_ref(TypeId, Args))
                else
                    Result = return_error(Context,
                        ce_type_has_incorrect_num_of_args(
                            Name, TypeArity ^ a_num, length(Args)))
                )
            )
        else
            Result = return_error(Context,
                ce_type_not_known(Name))
        )
    ; ArgsResult = errors(Error),
        Result = errors(Error)
    ).
build_type_ref(Core, Env, Sharing, MaybeCheckVars, Func) = Result :-
    Func = ast_type_func(Args0, Returns0, Uses0, Context),
    ArgsResult = result_list_to_result(
        map(build_type_ref(Core, Env, Sharing, MaybeCheckVars), Args0)),
    ReturnsResult = result_list_to_result(
        map(build_type_ref(Core, Env, Sharing, MaybeCheckVars), Returns0)),
    map_foldl2(build_uses(Context, Env), Uses0,
        ResourceErrorss, set.init, UsesSet, set.init, ObservesSet),
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
build_type_ref(_, _, _, MaybeCheckVars, ast_type_var(Name, Context)) =
        Result :-
    ( if
        MaybeCheckVars = check_type_vars(CheckVars) =>
        member(Name, CheckVars)
    then
        Result = ok(type_variable(Name))
    else
        Result = return_error(Context, ce_type_var_unknown(Name))
    ).

:- pred build_uses(context::in, env::in, ast_uses::in,
    errors(compile_error)::out,
    set(resource_id)::in, set(resource_id)::out,
    set(resource_id)::in, set(resource_id)::out) is det.

build_uses(Context, Env, ast_uses(Type, ResourceName), !:Errors,
        !Uses, !Observes) :-
    !:Errors = init,
    ( if env_search_resource(Env, ResourceName, ResourceId) then
        ( Type = ut_uses,
            !:Uses = set.insert(!.Uses, ResourceId)
        ; Type = ut_observes,
            !:Observes = set.insert(!.Observes, ResourceId)
        )
    else
        add_error(Context, ce_resource_unknown(ResourceName), !Errors)
    ).

%-----------------------------------------------------------------------%

:- pred func_to_pre(env::in, nq_named(ast_function)::in,
    map(func_id, pre_function)::in, map(func_id, pre_function)::out) is det.

func_to_pre(Env0, nq_named(Name, Func), !Pre) :-
    Func = ast_function(ast_function_decl(Params, Returns, _, Context),
        Body, _, _),
    ( Body = ast_body_block(Block),
        % The name parameter is the name in the environment and doesn't need to
        % be qualified.
        func_to_pre_func(Env0, q_name(Name), Params, Returns, Block, Context,
            !Pre)
    ; Body = ast_body_foreign
        % Foreign functions skip pre representation.
    ).

%-----------------------------------------------------------------------%

:- func check_resource_exports(core) = errors(compile_error).

check_resource_exports(Core) = Errors :-
    Resources = core_all_exported_resources(Core),
    Errors = cord_list_to_cord(
        map(check_resource_exports_2(Core), Resources)).

:- func check_resource_exports_2(core, pair(resource_id, resource)) =
    errors(compile_error).

check_resource_exports_2(Core, _ - Res) = Errors :-
    ( Res = r_io,
        Errors = init
    ; Res = r_other(Name, FromId, _, _, Context),
        Errors = check_resource_exports_3(Name, Context, Core, FromId)
    ; Res = r_abstract(_),
        Errors = init
    ).

:- func check_resource_exports_3(q_name, context, core, resource_id) =
    errors(compile_error).

check_resource_exports_3(Name, Context, Core, Res) = Errors :-
    resource_is_private(Core, Res) = IsPrivate,
    ( IsPrivate = is_private(RName),
        Errors = error(Context, ce_resource_not_public_in_resource(
            q_name_unqual(Name),
            q_name_unqual(RName)))
    ; IsPrivate = is_not_private,
        Errors = init
    ).

%-----------------------------------------------------------------------%

:- type is_private
    --->    is_private(q_name)
            % could be public, abstract or imported.
    ;       is_not_private.

:- func resource_is_private(core, resource_id) = is_private.

resource_is_private(Core, ResId) =
    resource_is_private_2(core_get_resource(Core, ResId)).

:- func resource_is_private_2(resource) = is_private.

resource_is_private_2(r_io) = is_not_private.
resource_is_private_2(r_other(RName, _, Sharing, Imported, _))
        = Private :-
    ( Sharing = s_public,
        Private = is_not_private
    ; Sharing = s_private,
        ( Imported = i_imported,
            Private = is_not_private
        ; Imported = i_local,
            Private = is_private(RName)
        )
    ).
resource_is_private_2(r_abstract(_)) = is_not_private.

:- func type_is_private(core, type_id) = is_private.

type_is_private(Core, TypeId) =
    type_is_private_2(core_get_type(Core, TypeId)).

:- func type_is_private_2(user_type) = is_private.

type_is_private_2(Type) = Private :-
    Sharing = utype_get_sharing(Type),
    ( Sharing = st_private,
        Imported = utype_get_imported(Type),
        ( Imported = i_imported,
            Private = is_not_private
        ; Imported = i_local,
            Private = is_private(utype_get_name(Type))
        )
    ; Sharing = st_public,
        Private = is_not_private
    ; Sharing = st_public_opaque,
        Private = is_not_private
    ).

%-----------------------------------------------------------------------%

:- func check_type_exports(core) = errors(compile_error).

check_type_exports(Core) = Errors :-
    Types = core_all_exported_types(Core),
    Errors = cord_list_to_cord(
        map(check_type_exports_2(Core), Types)).

:- func check_type_exports_2(core, pair(type_id, user_type)) =
    errors(compile_error).

check_type_exports_2(Core, _ - Type) = Errors :-
    Sharing = utype_get_sharing(Type),
    ( Sharing = st_public,
        ResourceErrors = cord_list_to_cord(
            map(check_type_resource(Core, Type),
                set.to_sorted_list(utype_get_resources(Core, Type)))),
        TypeErrors = cord_list_to_cord(
            map(check_type_type(Core, Type),
                set.to_sorted_list(utype_get_types(Core, Type)))),
        Errors = ResourceErrors ++ TypeErrors
    ; ( Sharing = st_public_opaque
      ; Sharing = st_private
      ),
      Errors = init
    ).

:- func check_type_resource(core, user_type, resource_id) =
    errors(compile_error).

check_type_resource(Core, Type, ResId) = Errors :-
    resource_is_private(Core, ResId) = Private,
    ( Private = is_private(RName),
        Name = utype_get_name(Type),
        Context = utype_get_context(Type),
        Errors = error(Context, ce_resource_not_public_in_type(
            q_name_unqual(Name), q_name_unqual(RName)))
    ; Private = is_not_private,
        Errors = init
    ).

:- func check_type_type(core, user_type, type_id) =
    errors(compile_error).

check_type_type(Core, Type, TypeId) = Errors :-
    type_is_private(Core, TypeId) = Private,
    ( Private = is_private(TName),
        Name = utype_get_name(Type),
        Context = utype_get_context(Type),
        Errors = error(Context, ce_type_not_public_in_type(
            q_name_unqual(Name), q_name_unqual(TName)))
    ; Private = is_not_private,
        Errors = init
    ).

%-----------------------------------------------------------------------%

:- func check_function_exports(core) = errors(compile_error).

check_function_exports(Core) = Errors :-
    Functions = core_all_exported_functions(Core),
    Errors = cord_list_to_cord(
        map(check_function_exports_2(Core), Functions)).

:- func check_function_exports_2(core, pair(func_id, function)) =
    errors(compile_error).

check_function_exports_2(Core, _ - Func) = Errors :-
    func_get_resource_signature(Func, Uses, Observes),
    func_get_type_signature(Func, Params, Returns, _),
    ParamsRes = union_list(map(type_get_resources, Params)),
    ReturnsRes = union_list(map(type_get_resources, Returns)),
    ResIds = set.to_sorted_list(Uses `set.union` Observes `set.union`
        ParamsRes `set.union` ReturnsRes),
    ResErrors = cord_list_to_cord(
        map(check_function_resource(Core, Func), ResIds)),
    TypeIds = set.to_sorted_list(
        union_list(map(type_get_types, Params)) `set.union`
        union_list(map(type_get_types, Returns))),
    TypeErrors = cord_list_to_cord(
        map(check_function_type(Core, Func), TypeIds)),
    Errors = ResErrors ++ TypeErrors.

:- func check_function_resource(core, function, resource_id) =
    errors(compile_error).

check_function_resource(Core, Func, ResId) = Errors :-
    resource_is_private(Core, ResId) = Private,
    ( Private = is_private(RName),
        Name = func_get_name(Func),
        Context = func_get_context(Func),
        Errors = error(Context, ce_resource_not_public_in_function(
            q_name_unqual(Name), q_name_unqual(RName)))
    ; Private = is_not_private,
        Errors = init
    ).

:- func check_function_type(core, function, type_id) =
    errors(compile_error).

check_function_type(Core, Func, TypeId) = Errors :-
    Private = type_is_private(Core, TypeId),
    ( Private = is_private(TName),
        FName = func_get_name(Func),
        Context = func_get_context(Func),
        Errors = error(Context, ce_type_not_public_in_func(
            q_name_unqual(FName), q_name_unqual(TName)))
    ; Private = is_not_private,
        Errors = init
    ).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
