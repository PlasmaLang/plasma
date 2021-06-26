%-----------------------------------------------------------------------%
% Plasma builder
% vim: ts=4 sw=4 et
%
% Copyright (C) 2020-2021 Plasma Team
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
:- import_module util.exception.
:- import_module util.io.
:- import_module util.mercury.
:- import_module util.path.

%-----------------------------------------------------------------------%

build(Options, Result, !IO) :-
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

        read_project(Options ^ pzb_build_file, ProjRes, ProjMTime, !IO),
        ( ProjRes = ok(_)
        ; ProjRes = errors(ReadProjErrors),
            add_errors(ReadProjErrors, !Errors)
        ),

        ( ProjRes = ok(Proj),
            build_dependency_info(Proj, DepInfoRes, init, _, !IO),
            ( DepInfoRes = ok(DepInfo),

                setup_build_dir(Options, SetupDirRes, !IO),
                ( SetupDirRes = ok
                ; SetupDirRes = error(SetupDirError),
                    add_error(context(build_directory), SetupDirError, !Errors)
                ),

                % For now we have no detection of when the vars file
                % changes, we updated it every time.
                write_vars_file(Options, WriteVarsRes, !IO),
                ( WriteVarsRes = ok
                ; WriteVarsRes = error(WriteVarsError),
                    add_error(context(ninja_vars_file_path), WriteVarsError,
                        !Errors)
                ),

                maybe_write_dependency_file(Options, ProjMTime, DepInfo,
                    WriteDepsRes, !IO),
                ( WriteDepsRes = ok
                ; WriteDepsRes = error(WriteNinjaBuildError),
                    add_error(context(ninja_build_file), WriteNinjaBuildError,
                        !Errors)
                ),

                ImportWhitelist = compute_import_whitelist(Proj),
                maybe_write_import_whitelist(Options ^ pzb_verbose,
                    ProjMTime, ImportWhitelist, WhitelistRes, !IO),
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
                t_modules_context   :: context
            ).

:- pred read_project(string::in, result(list(target), string)::out,
    time_t::out, io::di, io::uo) is det.

read_project(BuildFile, Result, MTime, !IO) :-
    io.file_modification_time(BuildFile, TimeRes, !IO),
    ( TimeRes = ok(MTime)
    ; TimeRes = error(_),
        % Assume the file was modified now, causing the ninja file to be
        % re-written.
        time(MTime, !IO)
    ),
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
            ModulesResult = search_toml_q_names(TargetContext,
                func(E) = "Invalid modules field: " ++ E,
                Target, "modules"),
            ( ModulesResult = ok(Modules - ModulesContext),
                Result = ok(yes(target(TargetName, Modules, ModulesContext)))
            ; ModulesResult = errors(Errors),
                Result = errors(Errors)
            )
        ; TargetResult = error(_),
            Result = return_error(TargetContext,
                format("Invalid name '%s'", [s(TargetStr)]))
        )
    else
        Result = ok(no)
    ).

:- func search_toml_q_names(context, func(string) = string, toml, toml_key) =
    result(pair(list(q_name), context), string).

search_toml_q_names(Context, WrapError, TOML, Key) =
    search_toml(Context, WrapError, q_name_from_dotted_string, TOML, Key).

:- func search_toml(context, func(string) = string,
        func(string) = maybe_error(T), toml, toml_key) =
    result(pair(list(T), context), string).

search_toml(NotFoundContext, WrapError, MakeResult, TOML, Key) =
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
                            R = return_error(Context, WrapError(
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
                WrapError("Value is not an array"))
        )
    else
        Result = return_error(NotFoundContext,
            WrapError(format("Key not found '%s'", [s(Key)])))
    ).

%-----------------------------------------------------------------------%

:- type dep_info == list(dep_target).

:- type dep_target
    --->    dt_program(
                dtp_name    :: nq_name,
                dtp_output  :: string,
                dtp_inputs  :: list(string)
            )
    ;       dt_object(
                dto_name    :: q_name,
                dto_output  :: string,
                dto_input   :: string,
                dto_depfile :: string
            )
    ;       dt_interface(
                dti_name    :: q_name,
                dti_output  :: string,
                dti_input   :: string,
                dti_depfile :: string
            )
    ;       dt_typeres(
                dttr_name   :: q_name,
                dttr_output :: string,
                dttr_input  :: string
            ).

:- pred build_dependency_info(list(target)::in,
    result(dep_info, string)::out, dir_info::in, dir_info::out,
    io::di, io::uo) is det.

build_dependency_info(Targets, MaybeDeps, !DirInfo, !IO) :-
    % The term Target is overloaded here, it means both the whole things
    % that plzbuild is trying to build, but also the steps that ninja does
    % to build them.
    ModulesList = condense(map((func(T) = L :-
            C0 = T ^ t_modules_context,
            L = map(func(M0) = M0 - C0, T ^ t_modules)
        ), Targets)) `with_type` list(pair(q_name, context)),

    foldl((pred((M1 - C1)::in, Ma0::in, Ma::out) is det :-
            ( if search(Ma0, M1, C1P) then
                % Replace it if C is before C0.
                ( if compare((>), C1, C1P) then
                    Ma = Ma0
                else
                    det_update(M1, C1, Ma0, Ma)
                )
            else
                det_insert(M1, C1, Ma0, Ma)
            )
        ), ModulesList, init, Modules),

    map_foldl2(
        (pred(M - Context::in, R::out, Di0::in, Di::out, IO0::di, IO::uo)
                is det :-
            find_module_file(".", source_extension, M, R0, Di0, Di, IO0, IO),
            ( R0 = yes(F),
                R = ok(M - F)
            ; R0 = no,
                R = return_error(Context,
                    format("Can't find source for %s module",
                        [s(q_name_to_string(M))]))
            ; R0 = error(Path, Message),
                R = return_error(context(Path), Message)
            )
        ),
        to_assoc_list(Modules), MaybeModuleFiles0, !DirInfo, !IO),
    MaybeModuleFiles = result_list_to_result(MaybeModuleFiles0),

    ( MaybeModuleFiles = ok(ModuleFiles),
        ModuleTargets = map(make_module_targets, ModuleFiles),
        ProgramTargets = map(make_program_target, Targets),

        MaybeDeps = ok(condense(ModuleTargets) ++ ProgramTargets)

    ; MaybeModuleFiles = errors(Errors),
        MaybeDeps = errors(Errors)
    ).

:- func make_program_target(target) = dep_target.

make_program_target(Target) = DepTarget :-
    FileName = nq_name_to_string(Target ^ t_name) ++ library_extension,
    ObjectNames = map(func(M) = canonical_base_name(M) ++ output_extension,
        Target ^ t_modules),
    DepTarget = dt_program(Target ^ t_name, FileName, ObjectNames).

:- func make_module_targets(pair(q_name, string)) = list(dep_target).

make_module_targets(ModuleName - SourceName) = Targets :-
    BaseName = canonical_base_name(ModuleName),
    TyperesName = BaseName ++ typeres_extension,
    InterfaceName = BaseName ++ interface_extension,
    ObjectName = BaseName ++ output_extension,
    DepFile = BaseName ++ depends_extension,
    IDepFile = BaseName ++ interface_depends_extension,
    Targets = [
        dt_interface(ModuleName, InterfaceName, SourceName, IDepFile),
        dt_object(ModuleName, ObjectName, SourceName, DepFile),
        dt_typeres(ModuleName, TyperesName, SourceName)
    ].

%-----------------------------------------------------------------------%

    % Write the dependency file if it the build file is newer.
    %
:- pred maybe_write_dependency_file(plzbuild_options::in, time_t::in,
    dep_info::in, maybe_error::out, io::di, io::uo) is det.

maybe_write_dependency_file(Options, ProjMTime, DepInfo, Result, !IO) :-
    update_if_stale(Options ^ pzb_verbose, ProjMTime, ninja_build_file,
        write_dependency_file(Options, DepInfo), Result, !IO).

:- pred write_dependency_file(plzbuild_options::in, dep_info::in,
    maybe_error::out, io::di, io::uo) is det.

write_dependency_file(Options, DepInfo, Result, !IO) :-
    write_file(Options ^ pzb_verbose, ninja_build_file,
        do_write_dependency_file(Options, DepInfo), Result, !IO).

:- pred do_write_dependency_file(plzbuild_options::in, dep_info::in,
    output_stream::in, io::di, io::uo) is det.

do_write_dependency_file(Options, DepInfo, BuildFile, !IO) :-
    write_string(BuildFile, "# Auto-generated by plzbuild\n", !IO),
    format(BuildFile, "include %s\n", [s(ninja_rules_file)], !IO),
    format(BuildFile, "include %s\n\n", [s(ninja_vars_file)], !IO),
    Path0 = Options ^ pzb_tools_path,
    ( if is_relative(Path0) then
        Path = "../" ++ Path0
    else
        Path = Path0
    ),
    format(BuildFile, "path = %s\n", [s(Path)], !IO),
    format(BuildFile, "source_path  = %s\n\n", [s(Options ^ pzb_source_path)],
        !IO),
    foldl(write_target(BuildFile), DepInfo, !IO).

:- pred write_target(output_stream::in, dep_target::in, io::di, io::uo) is det.

write_target(File, dt_program(ProgName, ProgFile, Objects), !IO) :-
    format(File, "build %s : plzlink %s\n",
        [s(ProgFile), s(string_join(" ", Objects))], !IO),
    format(File, "    name = %s\n\n",
        [s(nq_name_to_string(ProgName))], !IO),
    format(File, "build ../%s : copy_out %s\n",
        [s(ProgFile), s(ProgFile)], !IO),
    format(File, "    name = %s\n\n",
        [s(nq_name_to_string(ProgName))], !IO).
write_target(File, dt_object(ModuleName, ObjectFile, SourceFile, DepFile),
        !IO) :-
    % If we can detect import errors when building dependencies we can
    % remove it from this step and avoid some extra rebuilds.
    format(File, "build %s : plzc ../%s | %s || %s\n",
        [s(ObjectFile), s(SourceFile),
         s(import_whitelist_file_no_directroy), s(DepFile)], !IO),
    format(File, "    dyndep = %s\n",
        [s(DepFile)], !IO),
    format(File, "    import_whitelist = %s\n",
        [s(import_whitelist_file_no_directroy)], !IO),
    format(File, "    name = %s\n\n",
        [s(q_name_to_string(ModuleName))], !IO),
    format(File, "build %s : plzdep ../%s | %s\n",
        [s(DepFile), s(SourceFile), s(import_whitelist_file_no_directroy)],
        !IO),
    format(File, "    import_whitelist = %s\n",
        [s(import_whitelist_file_no_directroy)], !IO),
    format(File, "    target = %s\n",
        [s(ObjectFile)], !IO),
    format(File, "    name = %s\n\n",
        [s(q_name_to_string(ModuleName))], !IO).
write_target(File,
        dt_interface(ModuleName, InterfaceFile, SourceFile, DepFile), !IO) :-
    format(File, "build %s : plzi ../%s || %s\n",
        [s(InterfaceFile), s(SourceFile), s(DepFile)], !IO),
    format(File, "    dyndep = %s\n",
        [s(DepFile)], !IO),
    format(File, "    name = %s\n\n",
        [s(q_name_to_string(ModuleName))], !IO),
    format(File, "build %s : plzidep ../%s\n",
        [s(DepFile), s(SourceFile)], !IO),
    format(File, "    target = %s\n",
        [s(InterfaceFile)], !IO),
    format(File, "    name = %s\n\n",
        [s(q_name_to_string(ModuleName))], !IO).
write_target(File,
        dt_typeres(ModuleName, TyperesFile, SourceFile), !IO) :-
    format(File, "build %s : plztyperes ../%s\n",
        [s(TyperesFile), s(SourceFile)], !IO),
    format(File, "    name = %s\n\n",
        [s(q_name_to_string(ModuleName))], !IO).

%-----------------------------------------------------------------------%

:- pred write_vars_file(plzbuild_options::in, maybe_error::out,
    io::di, io::uo) is det.

write_vars_file(Options, Result, !IO) :-
    write_file(Options ^ pzb_verbose, ninja_vars_file_path,
        do_write_vars_file(Options), Result, !IO).

:- pred do_write_vars_file(plzbuild_options::in, output_stream::in,
    io::di, io::uo) is det.

do_write_vars_file(Options, File, !IO) :-
    ReportTiming = Options ^ pzb_report_timing,
    ( ReportTiming = report_timing,
        PCFlags = "--report-timing"
    ; ReportTiming = dont_report_timing,
        PCFlags = ""
    ),
    % All options are the same for now.
    PLFlags = PCFlags,

    write_string(File, "# Auto-generated by plzbuild\n", !IO),
    format(File, "pcflags = %s\n", [s(PCFlags)], !IO),
    format(File, "plflags = %s\n", [s(PLFlags)], !IO).

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

:- pred maybe_write_import_whitelist(verbose::in, time_t::in, whitelist::in,
    maybe_error::out, io::di, io::uo) is det.

maybe_write_import_whitelist(Verbose, ProjMTime, DepInfo, Result, !IO) :-
    update_if_stale(Verbose, ProjMTime, import_whitelist_file,
        write_import_whitelist(Verbose, DepInfo), Result, !IO).

:- pred write_import_whitelist(verbose::in, whitelist::in, maybe_error::out,
    io::di, io::uo) is det.

write_import_whitelist(Verbose, Whitelist, Result, !IO) :-
    write_file(Verbose, import_whitelist_file,
        do_write_import_whitelist(Whitelist), Result, !IO).

:- pred do_write_import_whitelist(whitelist::in, text_output_stream::in,
    io::di, io::uo) is det.

do_write_import_whitelist(Whitelist, File, !IO) :-
    write(File, map(to_sorted_list, to_sorted_list(Whitelist)) `with_type`
        list(list(q_name)), !IO),
    write_string(File, ".\n", !IO).

%-----------------------------------------------------------------------%

:- pred ensure_ninja_rules_file(plzbuild_options::in, maybe_error::out,
    io::di, io::uo) is det.

ensure_ninja_rules_file(Options, Result, !IO) :-
    Rebuild = Options ^ pzb_rebuild,
    Verbose = Options ^ pzb_verbose,
    ( Rebuild = need_rebuild,
        write_ninja_rules_file(Verbose, Result, !IO)
    ; Rebuild = dont_rebuild,
        file_type(yes, ninja_rules_file_path, StatResult, !IO),
        ( StatResult = ok(Stat),
            ( Stat = regular_file,
                Result = ok
            ;
                ( Stat = directory
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
                Result = error(
                    format("Cannot create rules file, '%s' already exists",
                        [s(ninja_rules_file_path)]))
            )
        ; StatResult = error(_),
            write_ninja_rules_file(Verbose, Result, !IO)
        )
    ).

:- pred write_ninja_rules_file(verbose::in, maybe_error::out, io::di, io::uo)
    is det.

write_ninja_rules_file(Verbose, Result, !IO) :-
    write_file(Verbose, ninja_rules_file_path,
        (pred(File::in, IO0::di, IO::uo) is det :-
            write_string(File, rules_contents, IO0, IO)
        ),
        Result, !IO).

:- func rules_contents = string.

rules_contents =
"# Auto-generated by plzbuild
ninja_required_version = 1.10

rule plztyperes
    command = $path/plzc $pcflags --mode make-typeres-exports $in -o $out
    description = Calculating type & resource exports for $name

rule plzidep
    command = $path/plzc $pcflags --mode make-interface-depends --target-file $target $in -o $out
    description = Calculating interface dependencies for $name

rule plzi
    command = $path/plzc $pcflags --mode make-interface $in -o $out
    description = Making interface for $name

rule plzdep
    command = $path/plzc $pcflags --mode make-depends --target-file $target --import-whitelist $import_whitelist --source-path $source_path $in -o $out
    description = Calculating dependencies for $name

rule plzc
    command = $path/plzc $pcflags --mode compile --import-whitelist $import_whitelist $in -o $out
    description = Compiling $name

rule plzlink
    command = $path/plzlnk $plflags -n $name -o $out $in
    description = Linking $name

rule copy_out
    command = cp $in $out
    description = Copying $name bytecode
".

%-----------------------------------------------------------------------%

:- pred invoke_ninja(plzbuild_options::in, list(target)::in,
    maybe_error::out, io::di, io::uo) is det.

invoke_ninja(Options, Proj, Result, !IO) :-
    Verbose = Options ^ pzb_verbose,
    Targets0 = Options ^ pzb_targets,
    ( Targets0 = [_ | _],
        Targets = Targets0
    ; Targets0 = [],
        Targets = map(func(T) = T ^ t_name, Proj)
    ),
    TargetsStr = string_join(" ", map(
        func(T) = "../" ++ nq_name_to_string(T) ++ library_extension,
        Targets)),
    invoke_command(Verbose, format("ninja %s -C %s %s",
        [s(verbose_opt_str(Verbose)), s(build_directory), s(TargetsStr)]),
        Result, !IO).

:- pred clean(plzbuild_options::in, io::di, io::uo) is det.

clean(Options, !IO) :-
    Verbose = Options ^ pzb_verbose,
    ( Verbose = verbose,
        format("Removing build directory %s\n",
            [s(build_directory)], !IO)
    ; Verbose = terse
    ),
    remove_file_recursively(build_directory, Result, !IO),
    ( Result = ok
    ; Result = error(Error),
        format("%s: %s",
            [s(build_directory), s(error_message(Error))], !IO)
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

:- pred setup_build_dir(plzbuild_options::in, maybe_error::out,
    io::di, io::uo) is det.

setup_build_dir(Options, Result, !IO) :-
    ensure_directory(Options, Result0, FreshBuildDir, !IO),
    ( Result0 = ok,
        ( FreshBuildDir = fresh,
            % We know that we ust mkdir'd the build directory, so we can
            % skip a stat() call.
            write_ninja_rules_file(Options ^ pzb_verbose, Result, !IO)
        ; FreshBuildDir = stale,
            ensure_ninja_rules_file(Options, Result, !IO)
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
    file_type(yes, build_directory, StatResult, !IO),
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
                    [s(build_directory)])),
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
    ( Verbose = verbose,
        format(stderr_stream, "mkdir %s\n", [s(build_directory)], !IO)
    ; Verbose = terse
    ),
    mkdir(build_directory, MkdirResult, Error, !IO),
    ( MkdirResult = yes,
        Result = ok
    ; MkdirResult = no,
        Result = error(
            format("Cannot create build directory '%s': %s",
                [s(build_directory), s(Error)]))
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
    file_modification_time(File, MTimeResult, !IO),
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
