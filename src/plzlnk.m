%-----------------------------------------------------------------------%
% Plasma linker
% vim: ts=4 sw=4 et
%
% Copyright (C) 2020 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
% This program links the pz intermediate representation.
%
%-----------------------------------------------------------------------%
:- module plzlnk.
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

:- import_module asm.
:- import_module asm_ast.
:- import_module constant.
:- import_module pz.
:- import_module pz.pz_ds.
:- import_module pz.read.
:- import_module pz.write.
:- import_module pz.link.
:- import_module pzt_parse.
:- import_module q_name.
:- import_module result.
:- import_module util.
:- import_module util.mercury.

%-----------------------------------------------------------------------%

main(!IO) :-
    io.command_line_arguments(Args0, !IO),
    process_options(Args0, OptionsResult, !IO),
    ( OptionsResult = ok(PZAsmOpts),
        Mode = PZAsmOpts ^ pzo_mode,
        ( Mode = link(BallName, MaybeEntryPoint, InputFile, OutputFile),
            link(BallName, MaybeEntryPoint, InputFile, OutputFile, !IO)
        ; Mode = help,
            usage(!IO)
        ; Mode = version,
            version(!IO)
        )
    ; OptionsResult = error(ErrMsg),
        exit_error(ErrMsg, !IO)
    ).

:- pred link(nq_name::in, maybe(q_name)::in, list(string)::in, string::in,
    io::di, io::uo) is det.

link(BallName, MaybeEntryPoint, InputFilenames, OutputFilename, !IO) :-
    read_inputs(InputFilenames, [], MaybeInputs, !IO),
    ( MaybeInputs = ok(Inputs),
        do_link(BallName, MaybeEntryPoint, Inputs, PZ),
        write_pz(pzft_ball, OutputFilename, PZ, WriteResult, !IO),
        ( WriteResult = ok
        ; WriteResult = error(ErrMsg),
            exit_error(ErrMsg, !IO)
        )
    ; MaybeInputs = error(Error),
        exit_error(Error, !IO)
    ).

:- pred read_inputs(list(string)::in, list(pz)::in, maybe_error(list(pz))::out,
    io::di, io::uo) is det.

read_inputs([], PZs0, ok(PZs), !IO) :-
    reverse(PZs0, PZs).
read_inputs([InputFilename | InputFilenames], PZs0, Result, !IO) :-
    read_pz(InputFilename, MaybeInput, !IO),
    ( MaybeInput = ok(pz_read_result(Type, PZ)),
        ( Type = pzf_object,
            read_inputs(InputFilenames, [PZ | PZs0], Result, !IO)
        ; Type = pzf_ball,
            Result = error("Expected Plasma Object, not Plasma Ball")
        )
    ; MaybeInput = error(Error),
        Result = error(Error)
    ).

%-----------------------------------------------------------------------%

:- type pzlnk_options
    --->    pzlnk_options(
                pzo_mode            :: pzo_mode,
                pzo_verbose         :: bool
            ).

:- type pzo_mode
    --->    link(
                pzml_ball_name      :: nq_name,
                pzml_entry_point    :: maybe(q_name),
                pzml_input_files    :: list(string),
                pzml_output_file    :: string
            )
    ;       help
    ;       version.

:- pred process_options(list(string)::in, maybe_error(pzlnk_options)::out,
    io::di, io::uo) is det.

process_options(Args0, Result, !IO) :-
    OptionOpts = option_ops_multi(short_option, long_option, option_default),
    getopt.process_options(OptionOpts, Args0, Args, MaybeOptions),
    ( MaybeOptions = ok(OptionTable),
        lookup_bool_option(OptionTable, help, Help),
        lookup_bool_option(OptionTable, version, Version),
        lookup_bool_option(OptionTable, verbose, Verbose),
        ( if Help = yes then
            Result = ok(pzlnk_options(help, Verbose))
        else if Version = yes then
            Result = ok(pzlnk_options(version, Verbose))
        else
            lookup_string_option(OptionTable, output, OutputFile),

            lookup_string_option(OptionTable, name, BallName0),

            lookup_string_option(OptionTable, entrypoint, EntryPoint0),
            ( if EntryPoint0 \= "" then
                MaybeEntryPoint = yes(q_name_from_dotted_string(EntryPoint0))
            else
                MaybeEntryPoint = no
            ),

            MaybeBallName = nq_name_from_string(BallName0),
            ( if Args = [] then
                Result = error("Provide one or more input files")
            else if OutputFile = "" then
                Result = error(
                    "Output file argument is missing or not understood")
            else
                ( MaybeBallName = error(Error),
                    Result = error(
                        format(
                            "Plasma Ball name (%s) is missing or invalid: %s",
                            [s(BallName0), s(Error)]))
                ; MaybeBallName = ok(BallName),
                    Result = ok(pzlnk_options(
                        link(BallName, MaybeEntryPoint, Args, OutputFile),
                        Verbose))
                )
            )
        )
    ; MaybeOptions = error(ErrMsg),
        Result = error("Error processing command line options: " ++ ErrMsg)
    ).

:- pred version(io::di, io::uo) is det.

version(!IO) :-
    io.write_string("Plasma abstract machine linker verison: dev\n", !IO),
    io.write_string("https://plasmalang.org\n", !IO),
    io.write_string("Copyright (C) 2020 The Plasma Team\n", !IO),
    io.write_string("Distributed under the MIT License\n", !IO).

:- pred usage(io::di, io::uo) is det.

usage(!IO) :-
    io.progname_base("plzlnk", ProgName, !IO),
    io.format("%s [-v] [-o <output> | --output <output>] <inputs>\n",
        [s(ProgName)], !IO),
    io.format("%s -h\n", [s(ProgName)], !IO).

:- type option
    --->    help
    ;       verbose
    ;       version
    ;       output
    ;       name
    ;       entrypoint.

:- pred short_option(char::in, option::out) is semidet.

short_option('h', help).
short_option('v', verbose).
short_option('o', output).
short_option('n', name).
short_option('e', entrypoint).

:- pred long_option(string::in, option::out) is semidet.

long_option("help",         help).
long_option("verbose",      verbose).
long_option("version",      version).
long_option("output",       output).
long_option("name",         name).
long_option("entrypoint",   entrypoint).

:- pred option_default(option::out, option_data::out) is multi.

option_default(help,        bool(no)).
option_default(verbose,     bool(no)).
option_default(version,     bool(no)).
option_default(output,      string("")).
option_default(name,        string("")).
option_default(entrypoint,  string("")).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
