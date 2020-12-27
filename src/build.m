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
:- import_module maybe.

:- import_module q_name.

:- type plzbuild_options
    --->    plzbuild_options(
                pzb_targets         :: list(nq_name),
                pzb_verbose         :: bool,
                pzb_rebuild         :: bool
            ).

    % build(Target, Verbose, Rebuild, !IO)
    %
:- pred build(plzbuild_options::in, maybe_error::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
:- implementation.

:- import_module map.
:- import_module pair.
:- import_module require.
:- import_module string.

:- import_module toml.

:- import_module constant.
:- import_module file_utils.
:- import_module util.
:- import_module util.exception.
:- import_module util.io.
:- import_module util.mercury.
:- import_module util.path.
:- import_module util.result.

%-----------------------------------------------------------------------%

build(Options, Result, !IO) :-
    read_project(ProjRes, !IO),
    ( ProjRes = ok(Proj),
        setup_build_dir(Options, SetupDirRes, !IO),
        ( SetupDirRes = ok,
            get_dir_list(".", MaybeDirList, !IO),
            ( MaybeDirList = ok(DirList),
                DepInfoRes = build_dependency_info(Proj, DirList),
                ( DepInfoRes = ok(DepInfo),
                    write_dependency_file(Options ^ pzb_verbose, DepInfo,
                        WriteDepsRes, !IO),
                    ( WriteDepsRes = ok,
                        invoke_ninja(Options, Proj, Result, !IO)
                    ; WriteDepsRes = error(_),
                        Result = WriteDepsRes
                    )
                ; DepInfoRes = error(Error),
                    Result = error(Error)
                )
            ; MaybeDirList = error(Error),
                Result = error(Error)
            )
        ; SetupDirRes = error(_),
            Result = SetupDirRes
        )
    ; ProjRes = error(Errors),
        Result = error(string_join(",\n", Errors))
    ).

%-----------------------------------------------------------------------%

:- type project
    --->    project(
                % The base name for the file of the compiled code
                p_name          :: nq_name,

                % The modules that make up the program
                p_modules       :: list(nq_name)
            ).

:- pred read_project(maybe_error(list(project), list(string))::out,
    io::di, io::uo) is det.

read_project(Result, !IO) :-
    BuildFile = "BUILD.plz",
    io.open_input(BuildFile, OpenRes, !IO),
    ( OpenRes = ok(File),
        parse_toml(File, TOMLRes, !IO),
        close_input(File, !IO),
        ( TOMLRes = ok(TOML),
            Result = maybe_error_list(map(make_project(TOML), keys(TOML)))
        ; TOMLRes = error(Error),
            Result = error([BuildFile ++ ": " ++ Error])
        )
    ; OpenRes = error(Error),
        Result = error([BuildFile ++ ": " ++ error_message(Error)])
    ).

:- func make_project(toml, string) = maybe_error(project).

make_project(TOML, TargetStr) = Result :-
    ( if
        lookup(TOML, TargetStr, tv_table(Program)),
        ok(Target) = nq_name_from_string(TargetStr),
        search_toml_nq_names(Program, "modules", Modules)
    then
        Result = ok(project(Target, Modules))
    else
        % XXX: Report project file errors better.
        Result = error("No/bad name or module")
    ).

:- pred search_toml_nq_names(toml::in, toml_key::in, list(nq_name)::out)
    is semidet.

search_toml_nq_names(TOML, Key, Values) :-
    search(TOML, Key, tv_array(Values0)),
    map((pred(tv_string(V0)::in, V::out) is semidet :-
            ok(V) = nq_name_from_string(V0)
        ), Values0, Values).

%-----------------------------------------------------------------------%

:- type dep_info == list(dep_target).

:- type dep_target
    --->    dt_program(
                dtp_name    :: nq_name,
                dtp_output  :: string,
                dtp_inputs  :: list(string)
            )
    ;       dt_object(
                dto_output  :: string,
                dto_input   :: string,
                dto_depfile :: string
            )
    ;       dt_interface(
                dti_output  :: string,
                dti_input   :: string
            ).

:- func build_dependency_info(list(project), list(string)) =
    maybe_error(dep_info).

build_dependency_info(Programs, DirList) = MaybeDeps :-
    MaybeModuleFiles = maybe_error_list(map(
        (func(M) = R :-
            R0 = find_module_file(DirList, source_extension, q_name(M)),
            ( R0 = yes(F),
                R = ok(M - F)
            ; R0 = no,
                R = error(format("Can't find source for %s module",
                    [s(nq_name_to_string(M))]))
            )
        ),
       sort_and_remove_dups(condense(map(func(P) = P ^ p_modules, Programs))))),

    ( MaybeModuleFiles = ok(ModuleFiles),
        ModuleTargets = map(make_module_targets, ModuleFiles),
        ModuleFilesMap = map.from_assoc_list(ModuleFiles),
        ProgramTargets = map(make_program_target(ModuleFilesMap), Programs),

        MaybeDeps = ok(condense(ModuleTargets) ++ ProgramTargets)

    ; MaybeModuleFiles = error(Errors),
        MaybeDeps = error(
            string_join("\n", map(to_string, Errors)))
    ).

:- func make_program_target(map(nq_name, string), project) = dep_target.

make_program_target(ModuleFiles, Program) = Target :-
    ProgramName = nq_name_to_string(Program ^ p_name) ++ library_extension,
    ObjectNames = map((func(M) = F :-
            SF = lookup(ModuleFiles, M),
            file_change_extension(source_extension, output_extension,
                SF, F)
        ), Program ^ p_modules),
    Target = dt_program(Program ^ p_name, ProgramName, ObjectNames).

:- func make_module_targets(pair(T, string)) = list(dep_target).

make_module_targets(_ - SourceName) = Targets :-
    filename_extension(source_extension, SourceName, BaseName),
    InterfaceName = BaseName ++ interface_extension,
    ObjectName = BaseName ++ output_extension,
    DepFile = BaseName ++ dep_info_extension,
    Targets = [
        dt_interface(InterfaceName, SourceName),
        dt_object(ObjectName, SourceName, DepFile)
    ].

%-----------------------------------------------------------------------%

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
    format(File, "    name = %s\n\n", [s(nq_name_to_string(ProgName))], !IO).
write_target(File, dt_object(ObjectFile, SourceFile, DepFile), !IO) :-
    format(File, "build %s : plzc ../%s || %s\n",
        [s(ObjectFile), s(SourceFile), s(DepFile)], !IO),
    format(File, "    dyndep = %s\n\n",
        [s(DepFile)], !IO),
    format(File, "build %s : plzdep ../%s\n",
        [s(DepFile), s(SourceFile)], !IO),
    format(File, "    target = %s\n\n",
        [s(ObjectFile)], !IO).
write_target(File, dt_interface(InterfaceFile, SourceFile), !IO) :-
    format(File, "build %s : plzi ../%s\n\n",
        [s(InterfaceFile), s(SourceFile)], !IO).

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
    description = Calculating dependencies for $in

rule plzi
    command = plzc --make-interface $in -o $out
    description = Making interface for $in

rule plzc
    command = plzc $in -o $out
    description = Compiling $in

rule plzlink
    command = plzlnk -n $name -o $out $in
    description = Linking $name
".

%-----------------------------------------------------------------------%

:- pred invoke_ninja(plzbuild_options::in, list(project)::in,
    maybe_error::out, io::di, io::uo) is det.

invoke_ninja(Options, Proj, Result, !IO) :-
    Verbose = Options ^ pzb_verbose,
    Targets0 = Options ^ pzb_targets,
    ( Targets0 = [_ | _],
        Targets = Targets0
    ; Targets0 = [],
        Targets = map(func(P) = P ^ p_name, Proj)
    ),
    TargetsStr = string_join(" ", map(
        func(T) = nq_name_to_string(T) ++ library_extension,
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
