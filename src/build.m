%-----------------------------------------------------------------------%
% Plasma builder
% vim: ts=4 sw=4 et
%
% Copyright (C) 2020 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
% This program starts the build process for Plasma projects
%
%-----------------------------------------------------------------------%
:- module build.
%-----------------------------------------------------------------------%

:- interface.

:- import_module bool.
:- import_module io.
:- import_module list.

:- import_module q_name.
:- import_module util.
:- import_module util.result.

:- type plzbuild_options
    --->    plzbuild_options(
                pzb_targets         :: list(nq_name),
                pzb_verbose         :: bool,
                pzb_rebuild         :: bool,
                pzb_build_file      :: string
            ).

    % build(Target, Verbose, Rebuild, !IO)
    %
:- pred build(plzbuild_options::in, errors(string)::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
:- implementation.

:- import_module cord.
:- import_module float.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module string.
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
    read_project(Options ^ pzb_build_file, ProjRes, ProjMTime, !IO),
    ( ProjRes = ok(Proj),
        setup_build_dir(Options, SetupDirRes, !IO),
        ( SetupDirRes = ok,
            get_dir_list(".", MaybeDirList, !IO),
            ( MaybeDirList = ok(DirList),
                DepInfoRes = build_dependency_info(Proj, DirList),
                ( DepInfoRes = ok(DepInfo),
                    maybe_write_dependency_file(Options ^ pzb_verbose,
                        ProjMTime, DepInfo, WriteDepsRes, !IO),
                    ( WriteDepsRes = ok,
                        invoke_ninja(Options, Proj, Result0, !IO),
                        ( Result0 = ok,
                            Result = init
                        ; Result0 = error(Error),
                            Result = error(nil_context, Error)
                        )
                    ; WriteDepsRes = error(Error),
                        Result = error(context(ninja_build_file), Error)
                    )
                ; DepInfoRes = errors(Errors),
                    Result = Errors
                )
            ; MaybeDirList = error(Error),
                Result = error(context("."), Error)
            )
        ; SetupDirRes = error(Error),
            Result = error(context(build_directory), Error)
        )
    ; ProjRes = errors(Errors),
        Result = Errors
    ).

%-----------------------------------------------------------------------%

:- type target
    --->    target(
                % The base name for the file of the compiled code
                t_name          :: nq_name,

                % The modules that make up the program
                t_modules       :: list(nq_name)
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
    lookup(TOML, TargetStr, TargetVal - Context),
    ( if
        TargetVal = tv_table(Target),
        search(Target, "type", tv_string("program") - _)
    then
        TargetResult = nq_name_from_string(TargetStr),
        ( TargetResult = ok(TargetName),
            ModulesResult = search_toml_nq_names(Context,
                func(E) = "Invalid modules field: " ++ E,
                Target, "modules"),
            ( ModulesResult = ok(Modules),
                Result = ok(yes(target(TargetName, Modules)))
            ; ModulesResult = errors(Errors),
                Result = errors(Errors)
            )
        ; TargetResult = error(_),
            Result = return_error(Context,
                format("Invalid name '%s'", [s(TargetStr)]))
        )
    else
        Result = ok(no)
    ).

:- func search_toml_nq_names(context, func(string) = string, toml, toml_key) =
    result(list(nq_name), string).

search_toml_nq_names(NotFoundContext, WrapError, TOML, Key) = Result :-
    ( if search(TOML, Key, Value - Context) then
        ( if Value = tv_array(Values) then
            Result = result_list_to_result(map(
                (func(TV) = R :-
                    ( if TV = tv_string(S) then
                        R0 = nq_name_from_string(S),
                        R = maybe_to_result(Context, WrapError, R0)
                    else
                        R = return_error(Context, "Name in array is a string")
                    )
                ),
                Values))
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
                dto_name    :: nq_name,
                dto_output  :: string,
                dto_input   :: string,
                dto_depfile :: string
            )
    ;       dt_interface(
                dti_name    :: nq_name,
                dti_output  :: string,
                dti_input   :: string
            ).

:- func build_dependency_info(list(target), list(string)) =
    result(dep_info, string).

build_dependency_info(Targets, DirList) = MaybeDeps :-
    % The term Target is overloaded here, it means both the whole things
    % that plzbuild is trying to build, but also the steps that ninja does
    % to build them.
    MaybeModuleFiles = result_list_to_result(map(
        (func(M) = R :-
            R0 = find_module_file(DirList, source_extension, q_name(M)),
            ( R0 = yes(F),
                R = ok(M - F)
            ; R0 = no,
                R = return_error(nil_context,
                    format("Can't find source for %s module",
                        [s(nq_name_to_string(M))]))
            )
        ),
       sort_and_remove_dups(condense(map(func(T) = T ^ t_modules, Targets))))),

    ( MaybeModuleFiles = ok(ModuleFiles),
        ModuleTargets = map(make_module_targets, ModuleFiles),
        ModuleFilesMap = map.from_assoc_list(ModuleFiles),
        ProgramTargets = map(make_program_target(ModuleFilesMap), Targets),

        MaybeDeps = ok(condense(ModuleTargets) ++ ProgramTargets)

    ; MaybeModuleFiles = errors(Errors),
        MaybeDeps = errors(Errors)
    ).

:- func make_program_target(map(nq_name, string), target) = dep_target.

make_program_target(ModuleFiles, Target) = DepTarget :-
    FileName = nq_name_to_string(Target ^ t_name) ++ library_extension,
    ObjectNames = map((func(M) = F :-
            SF = lookup(ModuleFiles, M),
            file_change_extension(source_extension, output_extension,
                SF, F)
        ), Target ^ t_modules),
    DepTarget = dt_program(Target ^ t_name, FileName, ObjectNames).

:- func make_module_targets(pair(nq_name, string)) = list(dep_target).

make_module_targets(ModuleName - SourceName) = Targets :-
    filename_extension(source_extension, SourceName, BaseName),
    InterfaceName = BaseName ++ interface_extension,
    ObjectName = BaseName ++ output_extension,
    DepFile = BaseName ++ dep_info_extension,
    Targets = [
        dt_interface(ModuleName, InterfaceName, SourceName),
        dt_object(ModuleName, ObjectName, SourceName, DepFile)
    ].

%-----------------------------------------------------------------------%

    % Write the dependency file if it the build file is newer.
    %
:- pred maybe_write_dependency_file(bool::in, time_t::in, dep_info::in,
    maybe_error::out, io::di, io::uo) is det.

maybe_write_dependency_file(Verbose, ProjMTime, DepInfo, Result, !IO) :-
    file_modification_time(ninja_build_file, MTimeResult, !IO),
    ( MTimeResult = ok(NinjaMTime),
        ( if difftime(ProjMTime, NinjaMTime) > 0.0 then
            % Project file is newer.
            write_dependency_file(Verbose, DepInfo, Result, !IO)
        else
            ( Verbose = yes,
                format(stderr_stream,
                    "Not writing %s, it is already current\n",
                    [s(ninja_build_file)], !IO)
            ; Verbose = no
            ),
            Result = ok
        )
    ; MTimeResult = error(_),
        % Always write the file.
        write_dependency_file(Verbose, DepInfo, Result, !IO)
    ).

:- pred write_dependency_file(bool::in, dep_info::in, maybe_error::out,
    io::di, io::uo) is det.

write_dependency_file(Verbose, DepInfo, Result, !IO) :-
    ( Verbose = yes,
        format(stderr_stream, "Writing %s\n", [s(ninja_build_file)], !IO)
    ; Verbose = no
    ),
    io.open_output(ninja_build_file, BuildFileResult, !IO),
    ( BuildFileResult = ok(BuildFile),
        write_string(BuildFile, "# Auto-generated by plzbuild\n", !IO),
        format(BuildFile, "include %s\n\n",
            [s(ninja_rules_file_no_directory)], !IO),
        foldl(write_target(BuildFile), DepInfo, !IO),
        close_output(BuildFile, !IO),
        Result = ok
    ; BuildFileResult = error(Error),
        Result = error(
            format("Cannot write '%s': %s",
                [s(ninja_build_file), s(error_message(Error))]))
    ).

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
    format(File, "build %s : plzc ../%s || %s\n",
        [s(ObjectFile), s(SourceFile), s(DepFile)], !IO),
    format(File, "    dyndep = %s\n",
        [s(DepFile)], !IO),
    format(File, "    name = %s\n\n",
        [s(nq_name_to_string(ModuleName))], !IO),
    format(File, "build %s : plzdep ../%s\n",
        [s(DepFile), s(SourceFile)], !IO),
    format(File, "    target = %s\n",
        [s(ObjectFile)], !IO),
    format(File, "    name = %s\n\n",
        [s(nq_name_to_string(ModuleName))], !IO).
write_target(File, dt_interface(ModuleName, InterfaceFile, SourceFile), !IO) :-
    format(File, "build %s : plzi ../%s\n",
        [s(InterfaceFile), s(SourceFile)], !IO),
    format(File, "    name = %s\n\n",
        [s(nq_name_to_string(ModuleName))], !IO).

:- pred ensure_ninja_rules_file(plzbuild_options::in, maybe_error::out,
    io::di, io::uo) is det.

ensure_ninja_rules_file(Options, Result, !IO) :-
    Rebuild = Options ^ pzb_rebuild,
    Verbose = Options ^ pzb_verbose,
    ( Rebuild = yes,
        write_ninja_rules_file(Verbose, Result, !IO)
    ; Rebuild = no,
        file_type(yes, ninja_rules_file, StatResult, !IO),
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
                        [s(ninja_rules_file)]))
            )
        ; StatResult = error(_),
            write_ninja_rules_file(Verbose, Result, !IO)
        )
    ).

:- pred write_ninja_rules_file(bool::in, maybe_error::out, io::di, io::uo)
    is det.

write_ninja_rules_file(Verbose, Result, !IO) :-
    ( Verbose = yes,
        format(stderr_stream, "Writing %s\n", [s(ninja_rules_file)], !IO)
    ; Verbose = no
    ),
    open_output(ninja_rules_file, OpenResult, !IO),
    ( OpenResult = ok(File),
        write_string(File, rules_contents, !IO),
        close_output(File, !IO),
        Result = ok
    ; OpenResult = error(Error),
        Result = error(
            format("Cannot create '%s': %s",
                [s(ninja_rules_file), s(error_message(Error))]))
    ).

:- func rules_contents = string.

rules_contents =
"# Auto-generated by plzbuild
ninja_required_version = 1.10

rule plzdep
    command = plzc --make-depend-info $target $in -o $out
    description = Calculating dependencies for $name

rule plzi
    command = plzc --make-interface $in -o $out
    description = Making interface for $name

rule plzc
    command = plzc $in -o $out
    description = Compiling $name

rule plzlink
    command = plzlnk -n $name -o $out $in
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
    ( Verbose = yes,
        format("Removing build directory %s\n",
            [s(build_directory)], !IO)
    ; Verbose = no
    ),
    remove_file_recursively(build_directory, Result, !IO),
    ( Result = ok
    ; Result = error(Error),
        format("%s: %s",
            [s(build_directory), s(error_message(Error))], !IO)
    ).

:- func verbose_opt_str(bool) = string.

verbose_opt_str(no) = "".
verbose_opt_str(yes) = "-v".

:- pred invoke_command(bool::in, string::in, maybe_error::out,
    io::di, io::uo) is det.

invoke_command(Verbose, Command, Result, !IO) :-
    ( Verbose = yes,
        format(stderr_stream, "Invoking: %s\n", [s(Command)], !IO),
        write_string(stderr_stream, "-----\n", !IO)
    ; Verbose = no
    ),
    call_system(Command, SysResult, !IO),
    ( Verbose = yes,
        write_string(stderr_stream, "-----\n", !IO)
    ; Verbose = no
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
            ( Rebuild = yes,
                clean(Options, !IO),
                mkdir_build_directory(Options, Result, !IO),
                Fresh = fresh
            ; Rebuild = no,
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
            ( Rebuild = yes,
                clean(Options, !IO),
                mkdir_build_directory(Options, Result, !IO),
                Fresh = fresh
            ; Rebuild = no,
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
    ( Verbose = yes,
        format(stderr_stream, "mkdir %s\n", [s(build_directory)], !IO)
    ; Verbose = no
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
%-----------------------------------------------------------------------%
