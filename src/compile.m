%-----------------------------------------------------------------------%
% Plasma compilation process
% vim: ts=4 sw=4 et
%
% Copyright (C) 2020-2021 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
% This module drives the compilation process.  It sits between plzc.m which
% interprets command line options to start the process and the other modules
% to actually do the compilation.
%
%-----------------------------------------------------------------------%
:- module compile.
%-----------------------------------------------------------------------%
:- interface.

:- import_module io.
:- import_module list.

:- import_module ast.
:- import_module common_types.
:- import_module compile_error.
:- import_module core.
:- import_module options.
:- import_module pz.
:- import_module pz.pz_ds.
:- import_module q_name.
:- import_module util.
:- import_module util.result.

%-----------------------------------------------------------------------%

:- pred process_declarations(general_options::in, ast::in,
    result_partial(core, compile_error)::out, io::di, io::uo) is det.

:- pred compile(general_options::in, compile_options::in, ast::in,
    result_partial(pz, compile_error)::out, io::di, io::uo) is det.

:- type typeres_exports
    --->    typeres_exports(
                te_resources        :: list(q_name),
                te_types            :: list({q_name, arity})
            ).

:- func find_typeres_exports(general_options, ast) =
    result_partial(typeres_exports, compile_error).

%-----------------------------------------------------------------------%

    % Exported so plzc can filter entries to process imports.
    %
:- pred filter_entries(list(ast_entry)::in, list(ast_import)::out,
    list(nq_named(ast_resource))::out, list(nq_named(ast_type(nq_name)))::out,
    list(nq_named(ast_function))::out) is det.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
:- implementation.

:- import_module cord.
:- import_module map.
:- import_module string.

:- import_module builtins.
:- import_module constant.
:- import_module context.
:- import_module core.arity_chk.
:- import_module core.branch_chk.
:- import_module core.pretty.
:- import_module core.res_chk.
:- import_module core.simplify.
:- import_module core.type_chk.
:- import_module core_to_pz.
:- import_module core_to_pz.data.
:- import_module dump_stage.
:- import_module file_utils.
:- import_module pre.
:- import_module pre.ast_to_core.
:- import_module pre.env.
:- import_module pre.import.
:- import_module pz.pretty.
:- import_module util.exception.
:- import_module util.log.
:- import_module util.path.

%-----------------------------------------------------------------------%

process_declarations(GeneralOpts, ast(ModuleName, Context, Entries), Result,
        !IO) :-
    Verbose = GeneralOpts ^ go_verbose,
    some [!Env, !ImportEnv, !Core, !Errors] (
        !:Errors = init,

        check_module_name(GeneralOpts, Context, ModuleName, !Errors),
        filter_entries(Entries, Imports, Resources0, Types0, Funcs),

        setup_env_and_core(ModuleName, !:ImportEnv, !:Env, !:Core),

        map_foldl3(gather_resource(ModuleName), Resources0, Resources,
            !ImportEnv, !Env, !Core),
        map_foldl3(gather_type(ModuleName), Types0, Types,
            !ImportEnv, !Env, !Core),

        ast_to_core_imports(Verbose, ModuleName, typeres_import,
            !.ImportEnv, GeneralOpts ^ go_import_whitelist_file, Imports,
            !Env, !Core, !Errors, !IO),

        ast_to_core_declarations(GeneralOpts, Resources, Types, Funcs, !.Env,
            _, !Core, !Errors, !IO),

        ( if not has_fatal_errors(!.Errors) then
            Result = ok(!.Core, !.Errors)
        else
            Result = errors(!.Errors)
        )
    ).

%-----------------------------------------------------------------------%

compile(GeneralOpts, CompileOpts, ast(ModuleName, Context, Entries), Result,
        !IO) :-
    Verbose = GeneralOpts ^ go_verbose,
    some [!Env, !ImportEnv, !Core, !Errors] (
        !:Errors = init,

        check_module_name(GeneralOpts, Context, ModuleName, !Errors),
        filter_entries(Entries, Imports, Resources0, Types0, Funcs),

        setup_env_and_core(ModuleName, !:ImportEnv, !:Env, !:Core),

        map_foldl3(gather_resource(ModuleName), Resources0, Resources,
            !ImportEnv, !Env, !Core),
        map_foldl3(gather_type(ModuleName), Types0, Types,
            !ImportEnv, !Env, !Core),

        ast_to_core_imports(Verbose, ModuleName, interface_import, !.ImportEnv,
            GeneralOpts ^ go_import_whitelist_file, Imports,
            !Env, !Core, !Errors, !IO),

        ast_to_core_declarations(GeneralOpts, Resources, Types, Funcs,
            !Env, !Core, !Errors, !IO),

        ( if not has_fatal_errors(!.Errors) then
            verbose_output(Verbose,
                "pre_to_core: Processing function bodies\n", !IO),
            ast_to_core_funcs(GeneralOpts, ModuleName, Funcs, !.Env,
                !Core, !Errors, !IO),
            ( if not has_fatal_errors(!.Errors) then
                maybe_dump_core_stage(GeneralOpts, "core0_initial", !.Core, !IO),
                semantic_checks(GeneralOpts, CompileOpts, !.Core,
                    CoreResult, !IO),
                ( CoreResult = ok(!:Core),
                    core_to_pz(GeneralOpts ^ go_verbose, CompileOpts, !.Core,
                        PZ, TypeTagMap, ConstructorTagMap, !IO),
                    maybe_dump_stage(GeneralOpts, module_name(!.Core),
                        "pz0_final", pz_pretty, PZ, !IO),
                    maybe_dump_stage(GeneralOpts, module_name(!.Core),
                        "data_rep", data_rep_pretty,
                            {!.Core, TypeTagMap, ConstructorTagMap}, !IO),
                    Result = ok(PZ, !.Errors)
                ; CoreResult = errors(SemErrors),
                    Result = errors(!.Errors ++ SemErrors)
                )
            else
                Result = errors(!.Errors)
            )
        else
            Result = errors(!.Errors)
        )
    ).

%-----------------------------------------------------------------------%

:- pred check_module_name(general_options::in, context::in, q_name::in,
    errors(compile_error)::in, errors(compile_error)::out) is det.

check_module_name(GOptions, Context, ModuleName, !Errors) :-
    % The module name and file name are both converted to an internal
    % representation and then compared lexicographically.  If that matches
    % then they match.  This allows the file name to vary with case and
    % punctuation differences.

    ModuleNameStripped = strip_file_name_punctuation(
        q_name_to_string(ModuleName)),

    InputFileName = GOptions ^ go_input_file,
    file_part(InputFileName, InputFileNameNoPath),
    filename_extension(source_extension, InputFileNameNoPath,
        InputFileNameBase),
    ( if
        strip_file_name_punctuation(InputFileNameBase) \= ModuleNameStripped
    then
        add_error(Context,
            ce_source_file_name_not_match_module(ModuleName, InputFileName),
            !Errors)
    else
        true
    ),

    OutputFileName = GOptions ^ go_output_file,
    ( if
        ( Extension = output_extension
        ; Extension = interface_extension
        ; Extension = typeres_extension
        ),
        filename_extension(Extension, OutputFileName, OutputFileNameBase),
        strip_file_name_punctuation(OutputFileNameBase) = ModuleNameStripped
    then
        true
    else
        add_error(Context, ce_object_file_name_not_match_module(ModuleName,
            OutputFileName), !Errors)
    ).

:- pred setup_env_and_core(q_name::in, env::out, env::out, core::out) is det.

setup_env_and_core(ModuleName, ImportEnv, Env, !:Core) :-
    !:Core = core.init(ModuleName),
    init_builtins_and_env(BuiltinMap, InitEnv, !Core),
    map.foldl(env_add_builtin(q_name), BuiltinMap ^ bm_map, InitEnv, Env),
    % We create a second environment, this one is used only for reading
    % interface files.
    map.foldl(env_add_builtin(func(Name) =
            q_name_append(builtin_module_name, Name)
        ), BuiltinMap ^ bm_map, InitEnv, ImportEnv).

:- pred init_builtins_and_env(builtin_map::out, env::out,
    core::in, core::out) is det.

init_builtins_and_env(BuiltinMap, Env, !Core) :-
    setup_builtins(BuiltinMap, Operators, !Core),
    Env = env.init(Operators).

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

%-----------------------------------------------------------------------%

:- pred gather_resource(q_name::in,
    nq_named(ast_resource)::in, a2c_resource::out,
    env::in, env::out, env::in, env::out, core::in, core::out) is det.

gather_resource(ModuleName, nq_named(Name, Res),
        a2c_resource(Name, ResId, Res), !ImportEnv, !Env, !Core) :-
    core_allocate_resource_id(ResId, !Core),
    ( if
        env_add_resource(q_name(Name), ResId, !Env),
        Sharing = Res ^ ar_sharing,
        ( Sharing = s_public,
            env_add_resource(q_name_append(ModuleName, Name), ResId,
                !ImportEnv)
        ; Sharing = s_private
        )
    then
        true
    else
        compile_error($file, $pred, "Resource already defined")
    ).

:- pred gather_type(q_name::in, nq_named(ast_type(nq_name))::in, a2c_type::out,
    env::in, env::out, env::in, env::out, core::in, core::out) is det.

gather_type(ModuleName, nq_named(Name, Type), a2c_type(Name, TypeId, Type),
        !ImportEnv, !Env, !Core) :-
    core_allocate_type_id(TypeId, !Core),
    Arity = type_arity(Type),
    ( if
        env_add_type(q_name(Name), Arity, TypeId, !Env),
        Sharing = Type ^ at_export,
        (
            ( Sharing = st_public
            ; Sharing = st_public_abstract
            ),
            env_add_type(q_name_append(ModuleName, Name), Arity, TypeId,
                !ImportEnv)
        ; Sharing = st_private
        )
    then
        true
    else
        compile_error($file, $pred, "Type already defined")
    ).

%-----------------------------------------------------------------------%

find_typeres_exports(GeneralOpts, ast(ModuleName, Context, Entries)) =
        Result :-
    some [!Errors] (
        !:Errors = init,

        check_module_name(GeneralOpts, Context, ModuleName, !Errors),
        filter_entries(Entries, _, Resources0, Types0, _),

        filter_map((pred(NamedRes::in, Name::out) is semidet :-
                NamedRes = nq_named(NQName, ast_resource(_, s_public, _)),
                Name = q_name_append(ModuleName, NQName)
            ),
            Resources0, Resources),

        filter_map((pred(NamedRes::in, {Name, Arity}::out) is semidet :-
                NamedRes = nq_named(NQName, ast_type(Params, _, Sharing, _)),
                ( Sharing = st_public
                ; Sharing = st_public_abstract
                ),
                Name = q_name_append(ModuleName, NQName),
                Arity = arity(length(Params))
            ),
            Types0, Types),

        ( if not has_fatal_errors(!.Errors) then
            Result = ok(typeres_exports(Resources, Types), !.Errors)
        else
            Result = errors(!.Errors)
        )
    ).

%-----------------------------------------------------------------------%

filter_entries([], [], [], [], []).
filter_entries([E | Es], !:Is, !:Rs, !:Ts, !:Fs) :-
    filter_entries(Es, !:Is, !:Rs, !:Ts, !:Fs),
    ( E = ast_import(I),
        !:Is = [I | !.Is]
    ; E = ast_resource(N, R),
        !:Rs = [nq_named(N, R) | !.Rs]
    ; E = ast_type(N, T),
        !:Ts = [nq_named(N, T) | !.Ts]
    ; E = ast_function(N, F),
        !:Fs = [nq_named(N, F) | !.Fs]
    ).

%-----------------------------------------------------------------------%

:- pred semantic_checks(general_options::in, compile_options::in,
    core::in, result(core, compile_error)::out, io::di, io::uo) is det.

semantic_checks(GeneralOpts, CompileOpts, !.Core, Result, !IO) :-
    some [!Errors] (
        !:Errors = init,
        Verbose = GeneralOpts ^ go_verbose,

        verbose_output(Verbose, "Core: arity checking\n", !IO),
        arity_check(Verbose, ArityErrors, !Core, !IO),
        maybe_dump_core_stage(GeneralOpts, "core1_arity", !.Core, !IO),
        add_errors(ArityErrors, !Errors),

        Simplify = CompileOpts ^ co_do_simplify,
        ( Simplify = do_simplify_pass,
            verbose_output(Verbose, "Core: simplify pass\n", !IO),
            simplify(Verbose, SimplifyErrors, !Core, !IO),
            maybe_dump_core_stage(GeneralOpts, "core2_simplify", !.Core,
                !IO),
            add_errors(SimplifyErrors, !Errors)
        ; Simplify = skip_simplify_pass
        ),

        ( if not has_fatal_errors(!.Errors) then
            verbose_output(Verbose, "Core: type checking\n", !IO),
            type_check(Verbose, TypecheckErrors, !Core, !IO),
            maybe_dump_core_stage(GeneralOpts, "core3_typecheck", !.Core,
                !IO),
            add_errors(TypecheckErrors, !Errors),

            verbose_output(Verbose, "Core: branch checking\n", !IO),
            branch_check(Verbose, BranchcheckErrors, !Core, !IO),
            maybe_dump_core_stage(GeneralOpts, "core4_branch", !.Core, !IO),
            add_errors(BranchcheckErrors, !Errors),

            verbose_output(Verbose, "Core: resource checking\n", !IO),
            res_check(Verbose, RescheckErrors, !Core, !IO),
            maybe_dump_core_stage(GeneralOpts, "core5_res", !.Core, !IO),
            add_errors(RescheckErrors, !Errors),

            ( if not has_fatal_errors(!.Errors) then
                Result = ok(!.Core)
            else
                Result = errors(!.Errors)
            )
        else
            Result = errors(!.Errors)
        )
    ).

%-----------------------------------------------------------------------%

:- pred maybe_dump_core_stage(general_options::in, string::in,
    core::in, io::di, io::uo) is det.

maybe_dump_core_stage(Opts, Stage, Core, !IO) :-
    maybe_dump_stage(Opts, module_name(Core), Stage, core_pretty, Core,
        !IO).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
