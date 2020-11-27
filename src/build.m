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
:- import_module maybe.

:- import_module q_name.

:- type plzbuild_options
    --->    plzbuild_options(
                pzb_target          :: nq_name,
                pzb_verbose         :: bool,
                pzb_rebuild         :: bool
            ).

    % build(Target, Verbose, Rebuild, !IO)
    %
:- pred build(plzbuild_options::in, maybe_error::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
:- implementation.

:- import_module list.
:- import_module map.
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
        % * Check project (does module exist?)
        ensure_directory(Options, EnsureDirRes, !IO),
        ( EnsureDirRes = ok,
            get_dir_list(MaybeDirList, !IO),
            ( MaybeDirList = ok(DirList),
                DepInfoRes = build_dependency_info(Proj, DirList),
                ( DepInfoRes = ok(DepInfo),
                    write_dependency_file(Options ^ pzb_verbose, DepInfo,
                        WriteDepsRes, !IO),
                    ( WriteDepsRes = ok,
                        invoke_ninja(Options, Result, !IO)
                    ; WriteDepsRes = error(_),
                        Result = WriteDepsRes
                    )
                ; DepInfoRes = error(Error),
                    Result = error(Error)
                )
            ; MaybeDirList = error(Error),
                Result = error(Error)
            )
        ; EnsureDirRes = error(_),
            Result = EnsureDirRes
        )
    ; ProjRes = error(Error),
        Result = error(Error)
    ).

%-----------------------------------------------------------------------%

:- type project
    --->    project(
                % The base name for the file of the compiled code
                p_name          :: nq_name,

                % The module name that contains the entrypoint.
                p_entry_module  :: nq_name
            ).

:- pred read_project(maybe_error(project)::out, io::di, io::uo) is det.

read_project(Result, !IO) :-
    BuildFile = "BUILD.plz",
    io.open_input(BuildFile, OpenRes, !IO),
    ( OpenRes = ok(File),
        parse_toml(File, TOMLRes, !IO),
        close_input(File, !IO),
        ( TOMLRes = ok(TOML),
            ( if
                search_toml_nq_name(TOML, "name", Target),
                search_toml_nq_name(TOML, "module", Module)
            then
                Result = ok(project(Target, Module))
            else
                % XXX: Report project file errors better.
                Result = error("No/bad name or module")
            )
        ; TOMLRes = error(Error),
            Result = error(BuildFile ++ ": " ++ Error)
        )
    ; OpenRes = error(Error),
        Result = error(BuildFile ++ ": " ++ error_message(Error))
    ).

:- pred search_toml_nq_name(toml::in, toml_key::in, nq_name::out) is semidet.

search_toml_nq_name(TOML, Key, Value) :-
    search(TOML, Key, tv_string(ValueStr)),
    ok(Value) = nq_name_from_string(ValueStr).

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
                dto_input   :: string
            ).

:- func build_dependency_info(project, list(string)) = maybe_error(dep_info).

build_dependency_info(Project, Filenames) = MaybeDeps :-
    MaybeFile = find_module_file(Filenames, source_extension,
        q_name(Project ^ p_entry_module)),
    ( MaybeFile = ok(SourceName),
        filename_extension(source_extension, SourceName, BaseName),
        ObjectName = BaseName ++ output_extension,
        ProgramName = BaseName ++ library_extension,
        MaybeDeps = ok(
            [dt_program(Project ^ p_name, ProgramName, [ObjectName]),
            dt_object(ObjectName, SourceName)])
    ; MaybeFile = error(Error),
        MaybeDeps = error(to_string(Error))
    ).

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
        [s(ProgFile), s(string_join(", ", Objects))], !IO),
    format(File, "    name = %s\n\n", [s(nq_name_to_string(ProgName))], !IO).
write_target(File, dt_object(ObjectFile, SourceFile), !IO) :-
    format(File, "build %s : plzc ../%s\n\n",
        [s(ObjectFile), s(SourceFile)], !IO).

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

rule plzc
    command = plzc $in -o $out
    description = Compiling $in

rule plzlink
    command = plzlnk -n $name -o $out $in
    description = Linking $name
".

%-----------------------------------------------------------------------%

:- pred invoke_ninja(plzbuild_options::in, maybe_error::out, io::di, io::uo)
    is det.

invoke_ninja(Options, Result, !IO) :-
    Rebuild = Options ^ pzb_rebuild,
    Verbose = Options ^ pzb_verbose,
    ( Rebuild = yes,
        invoke_ninja_clean(Verbose, Result0, !IO)
    ; Rebuild = no,
        Result0 = ok
    ),
    ( Result0 = ok,
        invoke_command(Verbose, format("ninja %s -C %s",
            [s(verbose_opt_str(Verbose)), s(build_directory)]), Result, !IO)
    ; Result0 = error(_),
        Result = Result0
    ).

:- pred invoke_ninja_clean(bool::in, maybe_error::out, io::di, io::uo) is det.

invoke_ninja_clean(Verbose, Result, !IO) :-
    invoke_command(Verbose, format("ninja %s -C %s -t clean",
        [s(verbose_opt_str(Verbose)), s(build_directory)]), Result, !IO).

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

:- pred ensure_directory(plzbuild_options::in, maybe_error::out,
    io::di, io::uo) is det.

ensure_directory(Options, Result, !IO) :-
    file_type(yes, build_directory, StatResult, !IO),
    ( StatResult = ok(Stat),
        ( Stat = directory,
            ensure_ninja_rules_file(Options, Result, !IO)
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
            Result = error(format(
                "Cannot create build directory, " ++
                    "'%s' already exists as non-directory",
                [s(build_directory)]))
        )
    ; StatResult = error(_),
        Verbose = Options ^ pzb_verbose,
        ( Verbose = yes,
            format(stderr_stream, "mkdir %s\n", [s(build_directory)], !IO)
        ; Verbose = no
        ),
        mkdir(build_directory, MkdirResult, Error, !IO),
        ( MkdirResult = yes,
            write_ninja_rules_file(Options ^ pzb_verbose, Result, !IO)
        ; MkdirResult = no,
            Result = error(
                format("Cannot create build directory '%s': %s",
                    [s(build_directory), s(Error)]))
        )
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
