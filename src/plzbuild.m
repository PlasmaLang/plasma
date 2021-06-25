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
:- module plzbuild.
%-----------------------------------------------------------------------%

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module char.
:- import_module getopt.
:- import_module list.
:- import_module maybe.
:- import_module string.

:- import_module build.
:- import_module constant.
:- import_module q_name.
:- import_module util.
:- import_module util.exception.
:- import_module util.mercury.
:- import_module util.path.
:- import_module util.result.

%-----------------------------------------------------------------------%

main(!IO) :-
    io.command_line_arguments(Args0, !IO),
    process_options(Args0, OptionsResult, !IO),
    ( OptionsResult = ok(Mode),
        ( Mode = build(Options),
            build(Options, Errors, !IO),
            report_errors("", Errors, !IO),
            ( if has_fatal_errors(Errors) then
                set_exit_status(1, !IO)
            else
                true
            )
        ; Mode = help,
            usage(!IO)
        ; Mode = version,
            version("Plasma Builder", !IO)
        )
    ; OptionsResult = error(ErrMsg),
        exit_error(ErrMsg, !IO)
    ).

%-----------------------------------------------------------------------%

:- type plzbuild_mode
    --->    build(plzbuild_options)
    ;       help
    ;       version.

:- pred process_options(list(string)::in, maybe_error(plzbuild_mode)::out,
    io::di, io::uo) is det.

process_options(Args0, Result, !IO) :-
    OptionOpts = option_ops_multi(short_option, long_option, option_default),
    getopt.process_options(OptionOpts, Args0, Args, MaybeOptions),
    ( MaybeOptions = ok(OptionTable),
        lookup_bool_option(OptionTable, help, Help),
        lookup_bool_option(OptionTable, version, Version),

        ( if Help = yes then
            Result = ok(help)
        else if Version = yes then
            Result = ok(version)
        else
            lookup_bool_option(OptionTable, verbose, Verbose),
            lookup_bool_option(OptionTable, rebuild, Rebuild),
            lookup_string_option(OptionTable, build_file, BuildFile),
            lookup_bool_option(OptionTable, report_timing,
                ReportTimingBool),
            ( ReportTimingBool = yes,
                ReportTiming = report_timing
            ; ReportTimingBool = no,
                ReportTiming = dont_report_timing
            ),

            discover_tools_path(MaybeToolsPath, !IO),
            ( MaybeToolsPath = yes(ToolsPath)
            ; MaybeToolsPath = no,
                util.exception.sorry($file, $pred,
                  "We don't know how to determine plzbuild's path " ++
                    "(OS incompatibility?)")
            ),

            MaybeModuleNames = maybe_error_list(map(
                string_to_module_name, Args)),
            ( MaybeModuleNames = ok(ModuleNames),
                Result = ok(build(plzbuild_options(ModuleNames, Verbose,
                    Rebuild, BuildFile, ReportTiming, ToolsPath, "../")))
            ; MaybeModuleNames = error(Errors),
                Result = error(string_join("\n", Errors))
            )
        )
    ; MaybeOptions = error(ErrMsg),
        Result = error("Error processing command line options: " ++ ErrMsg)
    ).

:- func string_to_module_name(string) = maybe_error(nq_name, string).

string_to_module_name(String) = Result :-
    MaybeName = nq_name_from_string(String),
    ( MaybeName = ok(Name),
        Result = ok(Name)
    ; MaybeName = error(Error),
        Result = error(format("Plasma program name '%s' is invalid: %s.",
            [s(String), s(Error)]))
    ).

:- pred discover_tools_path(maybe(string)::out, io::di, io::uo) is det.

discover_tools_path(MaybePath, !IO) :-
    progname("", ProgramName, !IO),
    ( if ProgramName \= "" then
        ( if file_and_dir(ProgramName, Dir, _) then
            MaybePath = yes(Dir)
        else
            get_environment_var("PATH", MaybePathVar, !IO),
            ( MaybePathVar = yes(PathVar),
                Paths = words_separator(unify(':'), PathVar),
                search_path(Paths, ProgramName, MaybePath, !IO)
            ; MaybePathVar = no,
                MaybePath = no
            )
        )
    else
        MaybePath = no
    ).

:- pred search_path(list(string)::in, string::in, maybe(string)::out,
    io::di, io::uo) is det.

search_path([], _, no, !IO).
search_path([Path | Paths], File, Result, !IO) :-
    file_and_dir(FullPath, Path, File),
    check_file_accessibility(FullPath, [execute], Res, !IO),
    ( Res = ok,
        Result = yes(Path)
    ; Res = error(_),
        search_path(Paths, File, Result, !IO)
    ).

:- pred usage(io::di, io::uo) is det.

usage(!IO) :-
    io.write_string("Plasma builder\n\n", !IO),
    io.write_string(
        "    The Plasma builder is used to build Plasma programs and\n" ++
        "    libraries.  It runs the other tools (compiler and linker)\n" ++
        "    to build an link the modules based on a `BUILD.plz` file.\n\n",
        !IO),

    io.write_string("Usage:\n\n", !IO),

    io.progname_base("plzbuild", ProgName, !IO),
    io.format("    %s [options] <program>\n",
        [s(ProgName)], !IO),
    io.format("    %s -h | --help>\n", [s(ProgName)], !IO),
    io.format("    %s --version>\n", [s(ProgName)], !IO),
    io.nl(!IO),
    io.write_string("Options may include:\n\n", !IO),
    io.write_string("    -v | --verbose\n", !IO),
    io.write_string("        Write verbose output\n\n", !IO),
    io.write_string("    --rebuild\n", !IO),
    io.write_string("        Regenerate/rebuild everything regardless of timestamps\n\n", !IO),
    io.write_string("Developer options:\n\n", !IO),
    io.write_string("    --build-file FILE\n", !IO),
    io.write_string("        Use this build file.\n\n", !IO),
    io.write_string("    --report-timing\n", !IO),
    io.write_string("        Report the elapsed and CPU time for each sub-command.\n\n", !IO).

:- type option
    --->    rebuild
    ;       build_file
    ;       report_timing
    ;       help
    ;       verbose
    ;       version.

:- pred short_option(char::in, option::out) is semidet.

short_option('h', help).
short_option('v', verbose).

:- pred long_option(string::in, option::out) is semidet.

long_option("rebuild",          rebuild).
long_option("build-file",       build_file).
long_option("report-timing",    report_timing).
long_option("help",             help).
long_option("verbose",          verbose).
long_option("version",          version).

:- pred option_default(option::out, option_data::out) is multi.

option_default(rebuild,         bool(no)).
option_default(build_file,      string(build_file)).
option_default(report_timing,   bool(no)).
option_default(help,            bool(no)).
option_default(verbose,         bool(no)).
option_default(version,         bool(no)).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
