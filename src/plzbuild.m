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
:- import_module q_name.
:- import_module util.
:- import_module util.exception.
:- import_module util.mercury.

%-----------------------------------------------------------------------%

main(!IO) :-
    io.command_line_arguments(Args0, !IO),
    process_options(Args0, OptionsResult, !IO),
    ( OptionsResult = ok(PZBuildOpts),
        Mode = PZBuildOpts ^ pzo_mode,
        ( Mode = build(ModuleName, Rebuild),
            build(ModuleName, Rebuild, !IO)
        ; Mode = help,
            usage(!IO)
        ; Mode = version,
            version(!IO)
        )
    ; OptionsResult = error(ErrMsg),
        exit_error(ErrMsg, !IO)
    ).

%-----------------------------------------------------------------------%

:- type plzbuild_options
    --->    plzbuild_options(
                pzo_mode            :: pzo_mode,
                pzo_verbose         :: bool
            ).

:- type pzo_mode
    --->    build(
                pzb_target          :: nq_name,
                pzb_rebuild         :: bool
            )
    ;       help
    ;       version.

:- pred process_options(list(string)::in, maybe_error(plzbuild_options)::out,
    io::di, io::uo) is det.

process_options(Args0, Result, !IO) :-
    OptionOpts = option_ops_multi(short_option, long_option, option_default),
    getopt.process_options(OptionOpts, Args0, Args, MaybeOptions),
    ( MaybeOptions = ok(OptionTable),
        lookup_bool_option(OptionTable, help, Help),
        lookup_bool_option(OptionTable, version, Version),
        lookup_bool_option(OptionTable, verbose, Verbose),

        ( if Help = yes then
            Result = ok(plzbuild_options(help, Verbose))
        else if Version = yes then
            Result = ok(plzbuild_options(version, Verbose))
        else
            lookup_bool_option(OptionTable, rebuild, Rebuild),
            ( Args = [],
                util.exception.sorry($file, $pred, "implicit target")
            ; Args = [Arg],
                MaybeModuleName = string_to_module_name(Arg),
                ( MaybeModuleName = ok(ModuleName),
                    Result = ok(plzbuild_options(
                        build(ModuleName, Rebuild), Verbose))
                ; MaybeModuleName = error(Error),
                    compile_error($file, $pred,
                        format("Bad module name '%s': %s",
                            [s(Arg), s(Error)]))
                )
            ; Args = [_, _ | _],
                util.exception.sorry($file, $pred, "Multiple targets")
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
        Result = error(format(
            "Plasma program name (%s) is missing or invalid: %s",
            [s(String), s(Error)]))
    ).

:- pred version(io::di, io::uo) is det.

version(!IO) :-
    io.write_string("Plasma builder version: dev\n", !IO),
    io.write_string("https://plasmalang.org\n", !IO),
    io.write_string("Copyright (C) 2020 The Plasma Team\n", !IO),
    io.write_string("Distributed under the MIT License\n", !IO).

:- pred usage(io::di, io::uo) is det.

usage(!IO) :-
    io.write_string("Plasma builder usage:\n\n", !IO),

    io.progname_base("plzbuild", ProgName, !IO),
    io.format("    %s <program>\n",
        [s(ProgName)], !IO),
    io.format("    %s -h | --help>\n", [s(ProgName)], !IO),
    io.format("    %s --version>\n", [s(ProgName)], !IO),
    io.nl(!IO).

:- type option
    --->    rebuild
    ;       help
    ;       verbose
    ;       version.

:- pred short_option(char::in, option::out) is semidet.

short_option('h', help).
short_option('v', verbose).

:- pred long_option(string::in, option::out) is semidet.

long_option("rebuild",      rebuild).
long_option("help",         help).
long_option("verbose",      verbose).
long_option("version",      version).

:- pred option_default(option::out, option_data::out) is multi.

option_default(rebuild,     bool(no)).
option_default(help,        bool(no)).
option_default(verbose,     bool(no)).
option_default(version,     bool(no)).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
