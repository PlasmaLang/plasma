%-----------------------------------------------------------------------%
% Plasma assembler
% vim: ts=4 sw=4 et
%
% Copyright (C) 2020-2021 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
% This program disassembles pz intermediate representation.
%
%-----------------------------------------------------------------------%
:- module plzdisasm.
%-----------------------------------------------------------------------%

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module char.
:- import_module cord.
:- import_module getopt.
:- import_module list.
:- import_module maybe.
:- import_module string.

:- import_module pz.
:- import_module pz.pretty.
:- import_module pz.read.
:- import_module util.
:- import_module util.exception.
:- import_module util.mercury.

%-----------------------------------------------------------------------%

main(!IO) :-
    io.command_line_arguments(Args0, !IO),
    process_options(Args0, OptionsResult, !IO),
    ( OptionsResult = ok(PZDisOpts),
        Mode = PZDisOpts ^ pzo_mode,
        ( Mode = disasm(InputFile),
            promise_equivalent_solutions [!:IO] (
                run_and_catch(do_dump(InputFile), plzasm,
                    HadErrors, !IO),
                ( HadErrors = had_errors,
                    io.set_exit_status(1, !IO)
                ; HadErrors = did_not_have_errors
                )
            )
        ; Mode = help,
            usage(!IO)
        ; Mode = version,
            version(!IO)
        )
    ; OptionsResult = error(ErrMsg),
        exit_error(ErrMsg, !IO)
    ).

:- pred do_dump(string::in, io::di, io::uo) is det.

do_dump(InputFile, !IO) :-
    read_pz(InputFile, Result, !IO),
    ( Result = ok(pz_read_result(Type, PZ)),
        Pretty =
            from_list(["// Plasma file type: ", string(Type), "\n\n"]) ++
            pz_pretty(PZ),
        write_string(append_list(list(Pretty)), !IO)
    ; Result = error(Error),
        exit_error(Error, !IO)
    ).

%-----------------------------------------------------------------------%

:- type pzdis_options
    --->    pzdis_options(
                pzo_mode            :: pzo_mode
            ).

:- type pzo_mode
    --->    disasm(
                pzmd_input_file     :: string
            )
    ;       help
    ;       version.

:- pred process_options(list(string)::in, maybe_error(pzdis_options)::out,
    io::di, io::uo) is det.

process_options(Args0, Result, !IO) :-
    OptionOpts = option_ops_multi(short_option, long_option, option_default),
    getopt.process_options(OptionOpts, Args0, Args, MaybeOptions),
    ( MaybeOptions = ok(OptionTable),
        lookup_bool_option(OptionTable, help, Help),
        lookup_bool_option(OptionTable, version, Version),
        ( if Help = yes then
            Result = ok(pzdis_options(help))
        else if Version = yes then
            Result = ok(pzdis_options(version))
        else
            ( if Args = [InputFile] then
                Result = ok(pzdis_options(disasm(InputFile)))
            else
                Result = error("Error processing command line options: " ++
                    "Expected exactly one input file")
            )
        )
    ; MaybeOptions = error(ErrMsg),
        Result = error("Error processing command line options: " ++ ErrMsg)
    ).

:- pred version(io::di, io::uo) is det.

version(!IO) :-
    io.write_string("Plasma abstract machine dis-assembler verison: dev\n",
        !IO),
    io.write_string("https://plasmalang.org\n", !IO),
    io.write_string("Copyright (C) 2015-2021 The Plasma Team\n", !IO),
    io.write_string("Distributed under the MIT License\n", !IO).

:- pred usage(io::di, io::uo) is det.

usage(!IO) :-
    io.progname_base("plzdisasm", ProgName, !IO),
    io.format("\t%s <input>\n", [s(ProgName)], !IO),
    io.format("\t%s -h | --help\n", [s(ProgName)], !IO),
    io.format("\t%s --version\n\n", [s(ProgName)], !IO).

:- type option
    --->    help
    ;       version.

:- pred short_option(char::in, option::out) is semidet.

short_option('h', help).

:- pred long_option(string::in, option::out) is semidet.

long_option("help",         help).
long_option("version",      version).

:- pred option_default(option::out, option_data::out) is multi.

option_default(help,        bool(no)).
option_default(version,     bool(no)).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
