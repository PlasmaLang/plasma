%-----------------------------------------------------------------------%
% Plasma bytecode execution
% vim: ts=4 sw=4 et
%
% Copyright (C) 2015 Paul Bone
% Distributed under the terms of the GPLv2 see ../LICENSE.tools
%
% This program executes plasma bytecode.
%
%-----------------------------------------------------------------------%
:- module pzrun.
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

:- import_module pz.
:- import_module pz.read.
:- import_module util.

%-----------------------------------------------------------------------%

main(!IO) :-
    io.command_line_arguments(Args0, !IO),
    process_options(Args0, Result, !IO),
    ( Result = ok(PZRunOpts),
        Mode = PZRunOpts ^ pzr_mode,
        ( Mode = run,
            read_pz(PZRunOpts ^ pzr_input_file, PZ, !IO),
            run(PZ, !IO)
        ; Mode = help,
            usage(!IO)
        )
    ; Result = error(ErrMsg),
        exit_error(ErrMsg, !IO)
    ).

%-----------------------------------------------------------------------%

:- type pzrun_options
    --->    pzrun_options(
                pzr_mode            :: pzr_mode,
                pzr_input_file      :: string,
                pzr_verbose         :: bool
            ).

:- type pzr_mode
    --->    run
    ;       help.

:- pred process_options(list(string)::in, maybe_error(pzrun_options)::out,
    io::di, io::uo) is det.

process_options(Args0, Result, !IO) :-
    OptionOpts = option_ops_multi(short_option, long_option, option_default),
    getopt.process_options(OptionOpts, Args0, Args, MaybeOptions),
    ( MaybeOptions = ok(OptionTable),
        ( Args = [File] ->
            lookup_bool_option(OptionTable, help, Help),
            ( Help = yes,
                Mode = help
            ; Help = no,
                Mode = run
            ),
            lookup_bool_option(OptionTable, verbose, Verbose),

            Options = pzrun_options(Mode, File, Verbose),
            Result = ok(Options)
        ;
            Result = error("Error processing command line options: " ++
                "Expected exactly one input file")
        )
    ; MaybeOptions = error(ErrMsg),
        Result = error("Error processing command line options: " ++ ErrMsg)
    ).

:- pred usage(io::di, io::uo) is det.

usage(!IO) :-
    io.progname_base("pzrun", ProgName, !IO),
    io.format("%s [-v] <input>",
        [s(ProgName)], !IO),
    io.format("%s -h", [s(ProgName)], !IO).

:- type option
    --->    help
    ;       verbose.

:- pred short_option(char::in, option::out) is semidet.

short_option('h', help).
short_option('v', verbose).

:- pred long_option(string::in, option::out) is semidet.

long_option("help",         help).
long_option("verbose",      verbose).

:- pred option_default(option::out, option_data::out) is multi.

option_default(help,        bool(no)).
option_default(verbose,     bool(no)).

%-----------------------------------------------------------------------%

:- pred run(pz::in, io::di, io::uo) is det.

run(_PZ, !IO).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
