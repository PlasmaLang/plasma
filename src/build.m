%-----------------------------------------------------------------------%
% Plasma builder
% vim: ts=4 sw=4 et
%
% Copyright (C) Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
% This program starts the build process for Plasma projects
%
%-----------------------------------------------------------------------%
:- module build.
%-----------------------------------------------------------------------%

:- interface.

:- import_module io.
:- import_module list.
:- import_module string.

:- import_module q_name.
:- import_module util.
:- import_module util.result.

:- type plzbuild_options
    --->    plzbuild_options(
                pzb_targets         :: list(nq_name),
                pzb_verbose         :: verbose,
                pzb_rebuild         :: rebuild,
                pzb_build_file      :: string,
                pzb_build_dir       :: string,
                pzb_report_timing   :: report_timing,

                % Path to the plasma tools
                pzb_tools_path      :: string,
                % Path to the source code
                pzb_source_path     :: string
            ).

:- type verbose
    --->    verbose
    ;       terse.

:- type rebuild
    --->    need_rebuild
    ;       dont_rebuild.

:- type report_timing
    --->    report_timing
    ;       dont_report_timing.

    % build(Target, Verbose, Rebuild, !IO)
    %
:- pred build(plzbuild_options::in, errors(string)::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
:- implementation.

:- import_module assoc_list.
:- import_module bool.
:- import_module cord.
:- import_module float.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module time.

:- import_module toml.

:- import_module constant.
:- import_module context.
:- import_module file_utils.
:- import_module util.my_exception.
:- import_module util.my_io.
:- import_module util.mercury.
:- import_module util.path.

%-----------------------------------------------------------------------%

build(Options, Result, !IO) :-
    BuildDir = Options ^ pzb_build_dir,
    % This code would make an interesting concurrency example.  There's:
    % + Several calls to fsstat() and readdir() that can occur independent
    %   of anything else.
    % + Reading the project file
    % + 2 independent computations (each is dependent on the project file's
    %   contents), but may be skipped if files are up to date.
    % + mkdir and writing 3 files that depend on various other steps.

    % But ideally they're not broken down that finely as is common with
    % Promises, instead some of them may be threads because their control
    % flow makes sense that way.

    some [!Errors] (
        !:Errors = init,

        file_modification_time(Options ^ pzb_tools_path ++ "/plzbuild",
            PlzBuildMTime, !IO),

        read_project(Options ^ pzb_build_file, ProjRes, ProjFileMTime, !IO),
        ( ProjRes = ok(_)
        ; ProjRes = errors(ReadProjErrors),
            add_errors(ReadProjErrors, !Errors)
        ),

        ( ProjRes = ok(Proj),
            build_dependency_info(Proj, DepInfoRes, init, _, !IO),
            ( DepInfoRes = ok(DepInfo),

                setup_build_dir(Options, PlzBuildMTime, SetupDirRes, !IO),
                ( SetupDirRes = ok
                ; SetupDirRes = error(SetupDirError),
                    add_error(context(Options ^ pzb_build_dir),
                        SetupDirError, !Errors)
                ),

                % For now we have no detection of when the vars file
                % changes, we updated it every time.
                write_vars_file(Options, WriteVarsRes, !IO),
                ( WriteVarsRes = ok
                ; WriteVarsRes = error(WriteVarsError),
                    add_error(
                        context(BuildDir ++ "/" ++ ninja_vars_file),
                        WriteVarsError, !Errors)
                ),

                ProjMTime = latest(ProjFileMTime, PlzBuildMTime),
                maybe_write_dependency_file(Options, ProjMTime, DepInfo,
                    WriteDepsRes, !IO),
                ( WriteDepsRes = ok
                ; WriteDepsRes = error(WriteNinjaBuildError),
                    add_error(context(BuildDir ++ "/" ++ ninja_build_file),
                        WriteNinjaBuildError, !Errors)
                ),

                ImportWhitelist = compute_import_whitelist(Proj),
                maybe_write_import_whitelist(Options, ProjMTime,
                    ImportWhitelist, WhitelistRes, !IO),
                ( WhitelistRes = ok
                ; WhitelistRes = error(WriteWhitelistError),
                    add_error(nil_context, WriteWhitelistError, !Errors)
                ),

                ( if is_empty(!.Errors) then
                    invoke_ninja(Options, Proj, Result0, !IO),
                    ( Result0 = ok
                    ; Result0 = error(Error),
                        add_error(nil_context, Error, !Errors)
                    )
                else
                    true
                )
            ; DepInfoRes = errors(DepInfoErrors),
                add_errors(DepInfoErrors, !Errors)
            )
        ; ProjRes = errors(_)
        ),

        Result = !.Errors
    ).

%-----------------------------------------------------------------------%

:- type target
    --->    target(
                % The base name for the file of the compiled code
                t_name              :: nq_name,

                % The modules that make up the program
                t_modules           :: list(q_name),
                t_modules_context   :: context,
                t_pcflags           :: maybe(string),
                t_c_sources         :: list(string),
                t_c_sources_context :: context
            ).

:- pred read_project(string::in, result(list(target), string)::out,
    time_t::out, io::di, io::uo) is det.

read_project(BuildFile, Result, MTime, !IO) :-
    file_modification_time(BuildFile, MTime, !IO),
    io.open_input(BuildFile, OpenRes, !IO),
    ( OpenRes = ok(File),
        parse_toml(File, BuildFile, TOMLRes, !IO),
        close_input(File, !IO),
        ( TOMLRes = ok(TOML),
            Result0 = result_list_to_result(map(make_target(TOML), keys(TOML))),
            ( Result0 = ok(MaybeTargets),
                Result = ok(filter_map(func(yes(X)) = X is semidet,
                    MaybeTargets))
            ; Result0 = errors(Errors),
                Result = errors(Errors)
            )
        ; TOMLRes = errors(Errors),
            Result = errors(Errors)
        )
    ; OpenRes = error(Error),
        Result = return_error(context(BuildFile), error_message(Error))
    ).

:- func make_target(toml, string) = result(maybe(target), string).

make_target(TOML, TargetStr) = Result :-
    lookup(TOML, TargetStr, TargetVal - TargetContext),
    ( if
        TargetVal = tv_table(Target),
        search(Target, "type", tv_string("program") - _)
    then
        TargetResult = nq_name_from_string(TargetStr),
        ( TargetResult = ok(TargetName),
            ModulesResult = search_toml_q_names(
                not_found_error(TargetContext, "modules"),
                Target, "modules"),
            CSourcesResult = search_toml_filenames(
                toml_search_default([], TargetContext),
                Target, "c_sources"),
            CompilerOptsResult = search_toml_maybe_string(Target, "compiler_opts"),
            (
                ModulesResult = ok(Modules - ModulesContext),
                CSourcesResult = ok(CSources - CSourcesContext),
                CompilerOptsResult = ok(CompilerOpts - _),

                ( if
                    find_duplicates(Modules, DupModules),
                    not is_empty(DupModules)
                then
                    DupModulesStrings = map(func(M) =
                        "'" ++ q_name_to_string(M) ++ "'",
                        to_sorted_list(DupModules)),
                    Result = return_error(TargetContext,
                        format(
                            "The following modules were listed more than once: %s",
                        [s(string_join(", ", DupModulesStrings))]))
                else
                    Result = ok(yes(target(TargetName, Modules, ModulesContext,
                        CompilerOpts, CSources, CSourcesContext)))
                )
            ;
                ModulesResult = ok(_),
                CSourcesResult = ok(_),
                CompilerOptsResult = errors(Errors),

                Result = errors(Errors)
            ;
                ModulesResult = ok(_),
                CSourcesResult = errors(Errors),

                Result = errors(Errors)
            ;
                ModulesResult = errors(Errors),

                Result = errors(Errors)
            )
        ; TargetResult = error(_),
            Result = return_error(TargetContext,
                format("Invalid name '%s'", [s(TargetStr)]))
        )
    else
        Result = ok(no)
    ).

%-----------------------------------------------------------------------%

:- type search_result(T) == result(pair(T, context), string).

    % search_toml_q_names(NotFoundResult, WrapError, Toml, Key) = Result
    %
    % Search the toml for the given key, if not found return an error at
    % Context, if found try to parse it as a list of q_names.  WrapError
    % lets the caller explain the context of the error.
    %
:- func search_toml_q_names(search_result(list(q_name)), toml, toml_key) =
    search_result(list(q_name)).

search_toml_q_names(NotFoundResult, TOML, Key) =
    search_toml_array(NotFoundResult, q_name_from_dotted_string,
        TOML, Key).

    % search_toml_q_names(NotFoundResult, WrapError, Toml, Key) = Result
    %
:- func search_toml_filenames(search_result(list(string)), toml, toml_key) =
    search_result(list(string)).

search_toml_filenames(NotFoundResult, TOML, Key) =
    search_toml_array(NotFoundResult, func(X) = ok(X), TOML, Key).

:- func search_toml_array(search_result(list(T)),
        func(string) = maybe_error(T), toml, toml_key) =
    search_result(list(T)).

search_toml_array(NotFoundResult, MakeResult, TOML, Key) =
        Result :-
    ( if search(TOML, Key, Value - Context) then
        ( if Value = tv_array(Values) then
            Result0 = result_list_to_result(map(
                (func(TV) = R :-
                    ( if TV = tv_string(S) then
                        R0 = MakeResult(S),
                        ( R0 = ok(N),
                            R = ok(N)
                        ; R0 = error(Why),
                            R = return_error(Context,
                                field_error(Key,
                                    format("'%s' %s", [s(S), s(Why)])))
                        )
                    else
                        R = return_error(Context, "Name in array is a string")
                    )
                ),
                Values)),
            ( Result0 = ok(List),
                Result = ok(List - Context)
            ; Result0 = errors(Errors),
                Result = errors(Errors)
            )
        else
            Result = return_error(Context,
                field_error(Key, "Value is not an array"))
        )
    else
        Result = NotFoundResult
    ).

:- func search_toml_maybe_string(toml, toml_key) = search_result(maybe(string)).

search_toml_maybe_string(TOML, Key) = Result :-
    ( if search(TOML, Key, Value - Context) then
        ( if Value = tv_string(String) then
            Result = ok(yes(String) - Context)
        else
            Result = return_error(Context,
                field_error(Key, "Value is not a string"))
        )
    else
        Result = ok(no - nil_context)
    ).

:- func field_error(string, string) = string.

field_error(Field, Msg) = format("Invalid %s field: %s", [s(Field), s(Msg)]).

:- func not_found_error(context, toml_key) = search_result(T).

not_found_error(Context, Key) =
    return_error(Context, format("Key not found '%s'", [s(Key)])).

:- func toml_search_default(T, context) = search_result(T).

toml_search_default(X, C) = ok(X - C).

%-----------------------------------------------------------------------%

:- type dep_info == list(dep_target).

:- type dep_target
    --->    dt_program(
                dtp_name            :: nq_name,
                dtp_output          :: string,
                dtp_inputs          :: list(string)
            )
    ;       dt_object(
                dto_name            :: q_name,
                dto_output          :: string,
                dto_input           :: string,
                dto_depfile         :: string,
                dto_flags           :: string
            )
    ;       dt_interface(
                dti_name            :: q_name,
                dti_output          :: string,
                dti_input           :: string,
                dti_depfile         :: string
            )
    ;       dt_typeres(
                dttr_name           :: q_name,
                dttr_output         :: string,
                dttr_input          :: string
            )
    ;       dt_scan(
                dts_name            :: q_name,
                dts_dep_file        :: string,
                dts_source          :: string,
                dts_interface       :: string,
                dts_bytecode        :: string
            )
    ;       dt_foreign_hooks(
                dtcg_name           :: q_name,
                dtcg_output_code    :: string,
                dtcg_output_header  :: string,
                dtcg_input          :: string
            )
            % Generate an init file for the FFI from the info files.
    ;       dt_gen_init(
                dtgi_name           :: nq_name,
                dtgi_output         :: string,
                dtgi_modules        :: list(q_name)
            )
    ;       dt_c_link(
                dtcl_name           :: nq_name,
                dtcl_output         :: string,
                dtcl_input          :: list(string)
            )
    ;       dt_c_compile(
                dtcc_output         :: string,
                dtcc_input          :: string,
                dtcc_headers        :: list(string),
                dtcc_generated      :: generated
            ).

:- type generated
    --->    was_generated
    ;       hand_written.

:- pred build_dependency_info(list(target)::in,
    result(dep_info, string)::out, dir_info::in, dir_info::out,
    io::di, io::uo) is det.

build_dependency_info(Targets, MaybeDeps, !DirInfo, !IO) :-
    MaybeModules0 = make_module_info(Targets),
    ( MaybeModules0 = ok(Modules0),
        % The term Target is overloaded here, it means both the whole things
        % that plzbuild is trying to build, but also the steps that ninja does
        % to build them.
        map_foldl2(find_module_file, Modules0, MaybeModules1, !DirInfo, !IO),
        MaybeModules = result_list_to_result(MaybeModules1),
        find_foreign_sources(Targets, MaybeForeignSources, !DirInfo, !IO),

        ( MaybeModules = ok(Modules),
          MaybeForeignSources = ok(ForeignSources0),
            ForeignSources = sort_and_remove_dups(ForeignSources0),
            ModuleTargets = map(make_module_targets, Modules),
            ProgramTargets = map(make_program_target, Targets),
            ForeignLinkTargetsRes = result_list_to_result(
                map(make_foreign_link_targets, Targets)),

            ( ForeignLinkTargetsRes = ok(ForeignLinkTargets0),
                ForeignCompileTargets = map(make_foreign_target,
                    ForeignSources),
                ForeignLinkTargets = condense(ForeignLinkTargets0),
                MaybeDeps = ok(condense(ModuleTargets) ++
                    ForeignCompileTargets ++
                    ForeignLinkTargets ++ ProgramTargets)
            ; ForeignLinkTargetsRes = errors(Errors),
                MaybeDeps = errors(Errors)
            )
        ; MaybeModules = ok(_),
          MaybeForeignSources = errors(Errors),
            MaybeDeps = errors(Errors)
        ; MaybeModules = errors(Errors),
          MaybeForeignSources = ok(_),
            MaybeDeps = errors(Errors)
        ; MaybeModules = errors(ErrorsA),
          MaybeForeignSources = errors(ErrorsB),
            MaybeDeps = errors(ErrorsA ++ ErrorsB)
        )
    ; MaybeModules0 = errors(Errors),
        MaybeDeps = errors(Errors)
    ).

:- type module_info
    --->    module_info(
                mi_name         :: q_name,
                mi_context      :: context,
                mi_file         :: string,
                mi_pcflags      :: string
            ).

:- func make_module_info(list(target)) = result(list(module_info), string).

make_module_info(Targets) = Modules :-
    Modules0 = condense(map(target_get_modules, Targets)),
    foldl_result(resolve_duplicate_modules, Modules0, init, MaybeModules1),
    ( MaybeModules1 = ok(Modules1),
        Modules = ok(map.values(Modules1))
    ; MaybeModules1 = errors(Error),
        Modules = errors(Error)
    ).

:- func target_get_modules(target) = list(module_info).

target_get_modules(Target) = Modules :-
    Context = Target ^ t_modules_context,
    PCFlags = maybe_default("", Target ^ t_pcflags),
    Modules = map(func(N) = module_info(N, Context, "", PCFlags),
        Target ^ t_modules).

:- pred resolve_duplicate_modules(module_info::in,
    map(q_name, module_info)::in,
    result(map(q_name, module_info), string)::out) is det.

resolve_duplicate_modules(Module, !Map) :-
    Name = Module ^ mi_name,
    map_set_or_update_result(func(M) = module_merge(M, Module),
            Name, Module, !Map).

:- func module_merge(module_info, module_info) = result(module_info, string).

module_merge(Ma, Mb) =
    ( if Ma ^ mi_pcflags = Mb ^ mi_pcflags then
        ok(module_info(Ma ^ mi_name,
            context_earliest(Ma ^ mi_context, Mb ^ mi_context),
            Ma ^ mi_file,
            Ma ^ mi_pcflags))
    else
        return_error(context_earliest(Ma ^ mi_context, Mb ^ mi_context),
          "Flags set for the same module in different programs do not match")
    ).

:- pred find_module_file(module_info::in,
    result(module_info, string)::out,
    dir_info::in, dir_info::out, io::di, io::uo) is det.

find_module_file(Module, ModuleResult, !DirInfo, !IO) :-
    find_module_file(".", source_extension, Module ^ mi_name, FileRes,
        !DirInfo, !IO),
    ( FileRes = yes(File),
        ModuleResult = ok(Module ^ mi_file := File)
    ; FileRes = no,
        ModuleResult = return_error(Module ^ mi_context,
            format("Can't find source for %s module",
                [s(q_name_to_string(Module ^ mi_name))]))
    ; FileRes = error(Path, Message),
        ModuleResult = return_error(context(Path), Message)
    ).

:- pred find_foreign_sources(list(target)::in,
    result(list(string), string)::out,
    dir_info::in, dir_info::out, io::di, io::uo) is det.

find_foreign_sources(Targets, Result, !DirInfo, !IO) :-
    SourcesList = condense(map((func(T) = T ^ t_c_sources), Targets)),
    % We don't do any filesystem checking, but might in the future.
    Result = ok(SourcesList).

:- func make_program_target(target) = dep_target.

make_program_target(Target) = DepTarget :-
    FileName = nq_name_to_string(Target ^ t_name) ++ library_extension,
    ObjectNames = map(func(M) = canonical_base_name(M) ++ output_extension,
        Target ^ t_modules),
    DepTarget = dt_program(Target ^ t_name, FileName, ObjectNames).

:- func make_module_targets(module_info) = list(dep_target).

make_module_targets(ModuleInfo) = Targets :-
    module_info(ModuleName, _, SourceName, PCFlags) = ModuleInfo,
    BaseName = canonical_base_name(ModuleName),
    TyperesName = BaseName ++ typeres_extension,
    InterfaceName = BaseName ++ interface_extension,
    ObjectName = BaseName ++ output_extension,
    DepFile = BaseName ++ depends_extension,
    Targets = [
        dt_scan(ModuleName, DepFile, SourceName, InterfaceName, ObjectName),
        dt_interface(ModuleName, InterfaceName, SourceName, DepFile),
        dt_object(ModuleName, ObjectName, SourceName, DepFile, PCFlags),
        dt_typeres(ModuleName, TyperesName, SourceName),
        dt_foreign_hooks(ModuleName,
            module_to_foreign_hooks_code(ModuleName),
            module_to_foreign_hooks_header(ModuleName), SourceName),
        dt_c_compile(
            module_to_foreign_object(ModuleName),
            module_to_foreign_hooks_code(ModuleName),
            [module_to_foreign_hooks_header(ModuleName)],
            was_generated)
    ].

:- func make_foreign_link_targets(target) =
    result(list(dep_target), string).

make_foreign_link_targets(Target) = DepsResult :-
    ForeignSources = Target ^ t_c_sources,
    Modules = Target ^ t_modules,
    ( ForeignSources = [],
        DepsResult = ok([])
    ; ForeignSources = [_ | _],
        Output = make_c_library_name(Target),
        map((pred(In::in, Out::out) is det :-
                ( if
                    file_change_extension(cpp_extension,
                        native_object_extension, In, Out0)
                then
                    Out = ok(Out0)
                else
                    Out = error(In)
                )
            ),
            ForeignSources, ForeignObjectsResults),
        ForeignObjectsResult = maybe_error_list(ForeignObjectsResults),
        ( ForeignObjectsResult = ok(ForeignObjects),
            ModuleForeignObjects = map(module_to_foreign_object, Modules),
            InitBaseName = nq_name_to_string(Target ^ t_name) ++ "_init",
            InitSourceName = InitBaseName ++ cpp_extension,
            InitObjectName = InitBaseName ++ native_object_extension,
            InitTargetSource = dt_gen_init(Target ^ t_name, InitSourceName,
                Modules),
            InitTargetObject = dt_c_compile(InitObjectName, InitSourceName,
                map(module_to_foreign_hooks_header, Modules), was_generated),
            LinkTarget = dt_c_link(Target ^ t_name, Output,
                ForeignObjects ++
                ModuleForeignObjects ++
                [InitObjectName]),
            DepsResult =
                ok([InitTargetSource, InitTargetObject, LinkTarget])
        ; ForeignObjectsResult = error(Errors),
            DepsResult = return_error(
                Target ^ t_c_sources_context,
                format("Unrecognised extensions on these files: %s",
                    [s(join_list(", ", Errors))]))
        )
    ).

:- func make_c_library_name(target) = string.

make_c_library_name(Target) = nq_name_to_string(Target ^ t_name) ++
    native_dylib_extension.

:- func make_foreign_target(string) = dep_target.

make_foreign_target(CFileName) = Target :-
    ( if
        file_change_extension(cpp_extension, native_object_extension,
            CFileName, ObjectName)
    then
        Target = dt_c_compile(ObjectName, CFileName, [], hand_written)
    else
        compile_error($file, $pred, "Unrecognised source file extension")
    ).

:- func module_to_foreign_hooks_code(q_name) = string.

module_to_foreign_hooks_code(Module) =
    module_to_foreign_hooks_base(Module) ++ cpp_extension.

:- func module_to_foreign_hooks_header(q_name) = string.

module_to_foreign_hooks_header(Module) =
    module_to_foreign_hooks_base(Module) ++ c_header_extension.

:- func module_to_foreign_hooks_base(q_name) = string.

module_to_foreign_hooks_base(Module) =
    canonical_base_name(Module) ++ "_f".

:- func module_to_foreign_object(q_name) = string.

module_to_foreign_object(Module) =
    canonical_base_name(Module) ++ "_f" ++ native_object_extension.

%-----------------------------------------------------------------------%

    % Write the dependency file if it the build file is newer.
    %
:- pred maybe_write_dependency_file(plzbuild_options::in, time_t::in,
    dep_info::in, maybe_error::out, io::di, io::uo) is det.

maybe_write_dependency_file(Options, ProjMTime, DepInfo, Result, !IO) :-
    update_if_stale(Options ^ pzb_verbose, ProjMTime,
        Options ^ pzb_build_dir ++ "/" ++ ninja_build_file,
        write_dependency_file(Options, DepInfo), Result, !IO).

:- pred write_dependency_file(plzbuild_options::in, dep_info::in,
    maybe_error::out, io::di, io::uo) is det.

write_dependency_file(Options, DepInfo, Result, !IO) :-
    write_file(Options ^ pzb_verbose,
        Options ^ pzb_build_dir ++ "/" ++ ninja_build_file,
        do_write_dependency_file(DepInfo), Result, !IO).

:- pred do_write_dependency_file(dep_info::in, output_stream::in,
    io::di, io::uo) is det.

do_write_dependency_file(DepInfo, BuildFile, !IO) :-
    write_string(BuildFile, "# Auto-generated by plzbuild\n", !IO),
    format(BuildFile, "include %s\n", [s(ninja_rules_file)], !IO),
    format(BuildFile, "include %s\n\n", [s(ninja_vars_file)], !IO),
    foldl(write_target(BuildFile), DepInfo, !IO).

:- pred write_statement(output_stream::in,
    string::in, string::in, string::in, list(string)::in, list(string)::in, maybe(string)::in,
    list(string)::in, maybe(string)::in, list(pair(string, string))::in, io::di, io::uo) is det.

write_statement(File, Command, Name, Output, ImplicitOutputs, Inputs, MaybeBinary,
        ImplicitDeps, MaybeDynDep, Vars, !IO) :-
    ( ImplicitOutputs = [],
        ImplicitOutput = ""
    ; ImplicitOutputs = [_ | _],
        ImplicitOutput = " | " ++ string_join(" ", ImplicitOutputs)
    ),
    InputsStr = string_join(" ", Inputs),
    ( MaybeBinary = yes(Binary),
        BinaryInput = ["$path/" ++ Binary]
    ; MaybeBinary = no,
        BinaryInput = []
    ),
    ExtraDeps = BinaryInput ++ ImplicitDeps,
    ( ExtraDeps = [_ | _],
        ExtraDepsStr = " | " ++ string_join(" ", ExtraDeps)
    ; ExtraDeps = [],
        ExtraDepsStr = ""
    ),
    ( MaybeDynDep = yes(DynDep),
        DynDepStr = " || " ++ DynDep
    ; MaybeDynDep = no,
        DynDepStr = ""
    ),
    write_string(File,
        "build " ++ Output ++ ImplicitOutput ++ " : " ++ Command ++ " " ++
            InputsStr ++ ExtraDepsStr ++ DynDepStr ++ "\n",
        !IO),
    write_var(File, "name" - Name, !IO),
    ( MaybeDynDep = yes(DynDep_),
        write_var(File, "dyndep" - DynDep_, !IO)
    ; MaybeDynDep = no
    ),
    foldl(write_var(File), Vars, !IO),
    nl(File, !IO).

:- pred write_var(output_stream::in, pair(string, string)::in,
    io::di, io::uo) is det.

write_var(File, Var - Val, !IO) :-
    format(File, "    %s = %s\n", [s(Var), s(Val)], !IO).

:- pred write_build_statement(output_stream::in, string::in, string::in,
    string::in, string::in, string::in, maybe(string)::in, io::di, io::uo)
    is det.

write_build_statement(File, Command, Name, Output, Path, Input, MaybeBinary,
        !IO) :-
    write_statement(File, Command, Name, Output, [], [Path ++ Input],
        MaybeBinary, [], no, [], !IO).

:- pred write_c_compile_statement(output_stream::in, string::in,
    string::in, string::in, string::in, list(string)::in,
    io::di, io::uo) is det.

write_c_compile_statement(File, Name, Output, Path, Input, Headers,
        !IO) :-
    write_statement(File, "c_compile", Name, Output, [], [Path ++ Input],
        no, Headers, no, [], !IO).

:- pred write_plzc_statement(output_stream::in, string::in, q_name::in,
    string::in, string::in, string::in, list(pair(string, string))::in,
    io::di, io::uo) is det.

write_plzc_statement(File, Command, Name, Output, Input,
        DepFile, Vars, !IO) :-
    write_statement(File, Command, q_name_to_string(Name),
        Output, [], ["../" ++ Input], yes("plzc"), [], yes(DepFile), Vars, !IO).

:- pred write_link_statement(output_stream::in, string::in, nq_name::in,
    string::in, list(string)::in, maybe(string)::in,
    io::di, io::uo) is det.

write_link_statement(File, Command, Name, Output, Objects, MaybeBinary,
        !IO) :-
    write_statement(File, Command, nq_name_to_string(Name),
        "../" ++ Output, [], Objects, MaybeBinary, [], no, [], !IO).

:- pred write_target(output_stream::in, dep_target::in, io::di, io::uo) is det.

write_target(File, dt_program(ProgName, ProgFile, Objects), !IO) :-
    write_link_statement(File, "plzlink", ProgName, ProgFile, Objects,
        yes("plzlnk"), !IO).
write_target(File,
        dt_object(ModuleName, ObjectFile, SourceFile, DepFile, Flags),
        !IO) :-
    % If we can detect import errors when building dependencies we can
    % remove it from this step and avoid some extra rebuilds.
    ImportWhitelistVar = "import_whitelist" -
        import_whitelist_file_no_directroy,
    PCFlagsVar = "pcflags_file" - Flags,
    write_plzc_statement(File, "plzc", ModuleName, ObjectFile, SourceFile,
        DepFile, [ImportWhitelistVar, PCFlagsVar], !IO).
write_target(File,
        dt_interface(ModuleName, InterfaceFile, SourceFile, DepFile), !IO) :-
    write_plzc_statement(File, "plzi", ModuleName, InterfaceFile,
        SourceFile, DepFile, [], !IO).
write_target(File,
        dt_typeres(ModuleName, TyperesFile, SourceFile), !IO) :-
    write_build_statement(File, "plztyperes", q_name_to_string(ModuleName),
        TyperesFile, "../", SourceFile, yes("plzc"), !IO).
write_target(File,
        dt_scan(ModuleName, DepFile, SourceFile, InterfaceFile, BytecodeFile),
        !IO) :-
    Inputs = ["../" ++ SourceFile],
    write_statement(File, "plzscan", q_name_to_string(ModuleName),
        DepFile, [], Inputs, yes("plzc"), [], no,
        ["target"       - BytecodeFile,
         "interface"    - InterfaceFile],
        !IO).
write_target(File, dt_foreign_hooks(ModuleName, OutCode, OutHeader, Source),
        !IO) :-
    write_statement(File, "plzgf", q_name_to_string(ModuleName),
        OutCode, [OutHeader], ["../" ++ Source], no, [], no,
        ["header"       - OutHeader], !IO).
write_target(File, dt_gen_init(ModuleName, Output, Inputs), !IO) :-
    InputsString = string_join(" ", map(q_name_to_string, Inputs)),
    write_statement(File, "gen_init", nq_name_to_string(ModuleName),
        Output, [], [], yes("plzgeninit"), [], no,
        ["modules" - InputsString], !IO).
write_target(File, dt_c_link(ModuleName, Output, Inputs), !IO) :-
    write_link_statement(File, "c_link", ModuleName, Output, Inputs,
        no, !IO).
write_target(File, dt_c_compile(Object, Source, Headers, SrcWasGenerated),
        !IO) :-
    ( SrcWasGenerated = was_generated,
        Path = ""
    ; SrcWasGenerated = hand_written,
        Path = "../"
    ),
    write_c_compile_statement(File, Source, Object, Path, Source, Headers, !IO).

%-----------------------------------------------------------------------%

:- pred write_vars_file(plzbuild_options::in, maybe_error::out,
    io::di, io::uo) is det.

write_vars_file(Options, Result, !IO) :-
    write_file(Options ^ pzb_verbose,
        Options ^ pzb_build_dir ++ "/" ++ ninja_vars_file,
        do_write_vars_file(Options), Result, !IO).

:- pred do_write_vars_file(plzbuild_options::in, output_stream::in,
    io::di, io::uo) is det.

do_write_vars_file(Options, File, !IO) :-
    Path0 = Options ^ pzb_tools_path,
    ( if is_relative(Path0) then
        Path = "../" ++ Path0
    else
        Path = Path0
    ),

    ReportTiming = Options ^ pzb_report_timing,
    ( ReportTiming = report_timing,
        PCFlags = "--report-timing"
    ; ReportTiming = dont_report_timing,
        PCFlags = ""
    ),
    % All options are the same for now.
    PLFlags = PCFlags,

    write_string(File, "# Auto-generated by plzbuild\n", !IO),
    format(File, "path = %s\n", [s(Path)], !IO),
    format(File, "source_path  = %s\n\n", [s(Options ^ pzb_source_path)], !IO),
    format(File, "pcflags_global = %s\n", [s(PCFlags)], !IO),
    format(File, "plflags_global = %s\n", [s(PLFlags)], !IO),
    format(File, "cxx = c++ -fpic\n", [], !IO),
    format(File, "cc = cc -fpic -shared\n", [], !IO).

%-----------------------------------------------------------------------%

%
% Use a whitelist to inform the compiler which modules may import which
% other modules based on the module lists in the project file.
%

    % Rather than actually compute the whitelist and store it, which could
    % be large, store the information used to compute it.  The set of sets
    % of modules that may import each-other.
    %
:- type whitelist == set(set(q_name)).

:- func compute_import_whitelist(list(target)) = whitelist.

compute_import_whitelist(Proj) =
    list_to_set(map(func(T) = list_to_set(T ^ t_modules), Proj)).

:- pred maybe_write_import_whitelist(plzbuild_options::in, time_t::in,
    whitelist::in, maybe_error::out, io::di, io::uo) is det.

maybe_write_import_whitelist(Options, ProjMTime, DepInfo, Result, !IO) :-
    update_if_stale(Options ^ pzb_verbose, ProjMTime,
        Options ^ pzb_build_dir ++ "/" ++ import_whitelist_file_no_directroy,
        write_import_whitelist(Options, DepInfo), Result, !IO).

:- pred write_import_whitelist(plzbuild_options::in, whitelist::in,
    maybe_error::out, io::di, io::uo) is det.

write_import_whitelist(Options, Whitelist, Result, !IO) :-
    write_file(Options ^ pzb_verbose,
        Options ^ pzb_build_dir ++ "/" ++ import_whitelist_file_no_directroy,
        do_write_import_whitelist(Whitelist), Result, !IO).

:- pred do_write_import_whitelist(whitelist::in, text_output_stream::in,
    io::di, io::uo) is det.

do_write_import_whitelist(Whitelist, File, !IO) :-
    write(File, map(to_sorted_list, to_sorted_list(Whitelist)) `with_type`
        list(list(q_name)), !IO),
    write_string(File, ".\n", !IO).

%-----------------------------------------------------------------------%

:- pred ensure_ninja_rules_file(plzbuild_options::in, time_t::in,
    maybe_error::out, io::di, io::uo) is det.

ensure_ninja_rules_file(Options, MTime, Result, !IO) :-
    Rebuild = Options ^ pzb_rebuild,
    ( Rebuild = need_rebuild,
        write_ninja_rules_file(Options, Result, !IO)
    ; Rebuild = dont_rebuild,
        update_if_stale(Options ^ pzb_verbose, MTime,
            Options ^ pzb_build_dir ++ "/" ++ ninja_rules_file,
            write_ninja_rules_file(Options), Result, !IO)
    ).

:- pred write_ninja_rules_file(plzbuild_options::in, maybe_error::out,
    io::di, io::uo) is det.

write_ninja_rules_file(Options, Result, !IO) :-
    write_file(Options ^ pzb_verbose,
        Options ^ pzb_build_dir ++ "/" ++ ninja_rules_file,
        (pred(File::in, IO0::di, IO::uo) is det :-
            write_string(File, rules_contents, IO0, IO)
        ),
        Result, !IO).

:- func rules_contents = string.

rules_contents =
"# Auto-generated by plzbuild
ninja_required_version = 1.10

rule plztyperes
    command = $path/plzc $pcflags_global $pcflags_file $
        --mode make-typeres-exports $
        --module-name-check $name $
        --source-path $source_path $
        $in -o $out
    description = Calculating type & resource exports for $name

rule plzi
    command = $path/plzc $pcflags_global $pcflags_file $
        --mode make-interface $
        --module-name-check $name $
        --source-path $source_path $
        $in -o $out
    description = Making interface for $name

rule plzscan
    command = $path/plzc $pcflags_global $pcflags_file $
        --mode scan $
        --target-bytecode $target --target-interface $interface $
        --module-name-check $name $
        --source-path $source_path $
        $in -o $out
    description = Scanning $name for dependencies

rule plzc
    command = $path/plzc $pcflags_global $pcflags_file $
        --mode compile $
        --import-whitelist $import_whitelist $
        --module-name-check $name $
        --source-path $source_path $
        $in -o $out
    description = Compiling $name

rule plzgf
    command = $path/plzc $pcflags_global $pcflags_file $
        --mode generate-foreign $
        --module-name-check $name $
        --source-path $source_path $
        --output-header $header $
        $in -o $out
    description = Generating foreign hooks for $name

rule gen_init
    command = $path/plzgeninit $
        $modules -o $out
    description = Generating foreign initialisation code for $name

rule plzlink
    command = $path/plzlnk $plflags_global -n $name -o $out $in
    description = Linking $name

rule c_link
    command = $cc -o $out $in
    description = Linking foreign code for $name

rule c_compile
    command = $cxx -o $out -c $in
    description = Compiling $name
".

%-----------------------------------------------------------------------%

:- pred invoke_ninja(plzbuild_options::in, list(target)::in,
    maybe_error::out, io::di, io::uo) is det.

invoke_ninja(Options, Proj, Result, !IO) :-
    Verbose = Options ^ pzb_verbose,
    Targets0 = Options ^ pzb_targets,
    ( Targets0 = [_ | _],
        TargetSet = list_to_set(Targets0),
        Targets = filter(pred(T::in) is semidet :-
            member(T ^ t_name, TargetSet), Proj)
    ; Targets0 = [],
        Targets = Proj
    ),
    NinjaTargets = map(
        (func(T) = [PZTarget] ++ CTarget :-
            Name = T ^ t_name,
            PZTarget = ninja_target_path(Name, library_extension),
            CSources = T ^ t_c_sources,
            ( CSources = [],
                CTarget = []
            ; CSources = [_ | _],
                % Need to build the foreign code
                CTarget = [ninja_target_path(Name, native_dylib_extension)]
            )
        ), Targets),
    NinjaTargetsStr = string_join(" ", condense(NinjaTargets)),
    invoke_command(Verbose, format("ninja %s -C %s %s",
        [s(verbose_opt_str(Verbose)), s(Options ^ pzb_build_dir),
            s(NinjaTargetsStr)]),
        Result, !IO).

:- func ninja_target_path(nq_name, string) = string.

ninja_target_path(Name, Extension) =
    "../" ++ nq_name_to_string(Name) ++ Extension.

:- pred clean(plzbuild_options::in, io::di, io::uo) is det.

clean(Options, !IO) :-
    Verbose = Options ^ pzb_verbose,
    BuildDir = Options ^ pzb_build_dir,
    ( Verbose = verbose,
        format("Removing build directory %s\n",
            [s(BuildDir)], !IO)
    ; Verbose = terse
    ),
    remove_file_recursively(BuildDir, Result, !IO),
    ( Result = ok
    ; Result = error(Error),
        format("%s: %s",
            [s(BuildDir), s(error_message(Error))], !IO)
    ).

:- func verbose_opt_str(verbose) = string.

verbose_opt_str(terse) = "".
verbose_opt_str(verbose) = "-v".

:- pred invoke_command(verbose::in, string::in, maybe_error::out,
    io::di, io::uo) is det.

invoke_command(Verbose, Command, Result, !IO) :-
    ( Verbose = verbose,
        format(stderr_stream, "Invoking: %s\n", [s(Command)], !IO),
        write_string(stderr_stream, "-----\n", !IO)
    ; Verbose = terse
    ),
    call_system(Command, SysResult, !IO),
    ( Verbose = verbose,
        write_string(stderr_stream, "-----\n", !IO)
    ; Verbose = terse
    ),
    ( SysResult = ok(Status),
        ( if Status = 0 then
            Result = ok
        else
            Result = error(format("Sub-command '%s' exited with exit-status %d",
                [s(Command), i(Status)]))
        )
    ; SysResult = error(Error),
        Result = error(format("Could not execute sub-command '%s': %s",
                [s(Command), s(error_message(Error))]))
    ).

%-----------------------------------------------------------------------%

:- pred setup_build_dir(plzbuild_options::in, time_t::in, maybe_error::out,
    io::di, io::uo) is det.

setup_build_dir(Options, MTime, Result, !IO) :-
    ensure_directory(Options, Result0, FreshBuildDir, !IO),
    ( Result0 = ok,
        ( FreshBuildDir = fresh,
            % We know that we ust mkdir'd the build directory, so we can
            % skip a stat() call.
            write_ninja_rules_file(Options, Result, !IO)
        ; FreshBuildDir = stale,
            ensure_ninja_rules_file(Options, MTime, Result, !IO)
        )
    ; Result0 = error(_),
        Result = Result0
    ).

:- type fresh
    --->    fresh
    ;       stale.

:- pred ensure_directory(plzbuild_options::in, maybe_error::out,
    fresh::out, io::di, io::uo) is det.

ensure_directory(Options, Result, Fresh, !IO) :-
    Rebuild = Options ^ pzb_rebuild,
    BuildDir = Options ^ pzb_build_dir,
    file_type(yes, BuildDir, StatResult, !IO),
    ( StatResult = ok(Stat),
        ( Stat = directory,
            ( Rebuild = need_rebuild,
                clean(Options, !IO),
                mkdir_build_directory(Options, Result, !IO),
                Fresh = fresh
            ; Rebuild = dont_rebuild,
                Result = ok,
                Fresh = stale
            )
        ;
            ( Stat = regular_file
            ; Stat = symbolic_link
            ; Stat = named_pipe
            ; Stat = socket
            ; Stat = character_device
            ; Stat = block_device
            ; Stat = message_queue
            ; Stat = semaphore
            ; Stat = shared_memory
            ; Stat = unknown
            ),
            ( Rebuild = need_rebuild,
                clean(Options, !IO),
                mkdir_build_directory(Options, Result, !IO),
                Fresh = fresh
            ; Rebuild = dont_rebuild,
                Result = error(format(
                    "Cannot create build directory, " ++
                        "'%s' already exists as non-directory",
                    [s(BuildDir)])),
                Fresh = stale
            )
        )
    ; StatResult = error(_),
        mkdir_build_directory(Options, Result, !IO),
        Fresh = fresh
    ).

:- pred mkdir_build_directory(plzbuild_options::in, maybe_error::out,
    io::di, io::uo) is det.

mkdir_build_directory(Options, Result, !IO) :-
    Verbose = Options ^ pzb_verbose,
    BuildDir = Options ^ pzb_build_dir,
    ( Verbose = verbose,
        format(stderr_stream, "mkdir %s\n", [s(BuildDir)], !IO)
    ; Verbose = terse
    ),
    mkdir(BuildDir, MkdirResult, Error, !IO),
    ( MkdirResult = yes,
        Result = ok
    ; MkdirResult = no,
        Result = error(
            format("Cannot create build directory '%s': %s",
                [s(BuildDir), s(Error)]))
    ).

:- pragma foreign_decl("C", local,
"
#include <string.h>
").

:- pred mkdir(string::in, bool::out, string::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    mkdir(Name::in, Result::out, Error::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, will_not_throw_exception],
    "
        int ret = mkdir(Name, 0755);
        if (ret == 0) {
            Result = MR_YES;
            // Error really is const
            Error = (char *)"""";
        } else {
            Result = MR_NO;
            char *error_msg = MR_GC_NEW_ARRAY(char, 128);
            ret = strerror_r(errno, error_msg, 128);
            if (ret == 0) {
                Error = error_msg;
            } else {
                Error = (char *)""Buffer too small for error message"";
            }
        }
    ").

%-----------------------------------------------------------------------%

:- pred update_if_stale(verbose, time_t, string,
    pred(maybe_error, io, io), maybe_error, io, io).
:- mode update_if_stale(in, in, in,
    pred(out, di, uo) is det, out, di, uo).

update_if_stale(Verbose, ProjMTime, File, Update, Result, !IO) :-
    io.file_modification_time(File, MTimeResult, !IO),
    ( MTimeResult = ok(MTime),
        ( if difftime(ProjMTime, MTime) > 0.0 then
            % Project file is newer.
            Update(Result, !IO)
        else
            ( Verbose = verbose,
                format(stderr_stream,
                    "Not writing %s, it is already current\n",
                    [s(File)], !IO)
            ; Verbose = terse
            ),
            Result = ok
        )
    ; MTimeResult = error(_),
        % Always write the file.
        Update(Result, !IO)
    ).

:- func latest(time_t, time_t) = time_t.

latest(A, B) =
    ( if difftime(A, B) > 0.0 then
        A
    else
        B
    ).

    % Get a file modification time or now if unknown.
    %
:- pred file_modification_time(string::in, time_t::out, io::di, io::uo) is
    det.

file_modification_time(File, MTime, !IO) :-
    io.file_modification_time(File, TimeRes, !IO),
    ( TimeRes = ok(MTime)
    ; TimeRes = error(_),
        % Assume the file was modified now, causing other files to be
        % updated.
        time(MTime, !IO)
    ).

:- pred write_file(verbose, string,
    pred(text_output_stream, io, io), maybe_error, io, io).
:- mode write_file(in, in,
    pred(in, di, uo) is det, out, di, uo) is det.

write_file(Verbose, Filename, Writer, Result, !IO) :-
    ( Verbose = verbose,
        format(stderr_stream, "Writing %s\n", [s(Filename)], !IO)
    ; Verbose = terse
    ),
    io.open_output(Filename, FileResult, !IO),
    ( FileResult = ok(File),
        Writer(File, !IO),
        close_output(File, !IO),
        Result = ok
    ; FileResult = error(Error),
        Result = error(
            format("Cannot write '%s': %s",
                [s(Filename), s(error_message(Error))]))
    ).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
