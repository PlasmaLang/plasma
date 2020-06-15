%-----------------------------------------------------------------------%
% Plasma assembler
% vim: ts=4 sw=4 et
%
% Copyright (C) 2015, 2017-2020 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
% This program assembles and links the pz intermediate representation.
%
%-----------------------------------------------------------------------%
:- module plzasm.
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
:- import_module pz.write.
:- import_module pzt_parse.
:- import_module result.
:- import_module util.

%-----------------------------------------------------------------------%

main(!IO) :-
    io.command_line_arguments(Args0, !IO),
    process_options(Args0, OptionsResult, !IO),
    ( OptionsResult = ok(PZAsmOpts),
        Mode = PZAsmOpts ^ pzo_mode,
        ( Mode = assemble(InputFile, OutputFile),
            promise_equivalent_solutions [!:IO] (
                run_and_catch(do_assemble(InputFile, OutputFile), pzasm,
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

:- pred do_assemble(string::in, string::in, io::di, io::uo) is det.

do_assemble(InputFile, OutputFile, !IO) :-
    pzt_parse.parse(InputFile, MaybePZAst, !IO),
    ( MaybePZAst = ok(PZAst),
        assemble(PZAst, MaybePZ),
        ( MaybePZ = ok(PZ),
            write_pz(pzft_object, OutputFile, PZ, Result, !IO),
            ( Result = ok
            ; Result = error(ErrMsg),
                exit_error(ErrMsg, !IO)
            )
        ; MaybePZ = errors(Errors),
            report_errors(Errors, !IO)
        )
    ; MaybePZAst = errors(Errors),
        report_errors(Errors, !IO)
    ).

%-----------------------------------------------------------------------%

:- type pzasm_options
    --->    pzasm_options(
                pzo_mode            :: pzo_mode,
                pzo_verbose         :: bool
            ).

:- type pzo_mode
    --->    assemble(
                pzma_input_file     :: string,
                pzma_output_file    :: string
            )
    ;       help
    ;       version.

:- pred process_options(list(string)::in, maybe_error(pzasm_options)::out,
    io::di, io::uo) is det.

process_options(Args0, Result, !IO) :-
    OptionOpts = option_ops_multi(short_option, long_option, option_default),
    getopt.process_options(OptionOpts, Args0, Args, MaybeOptions),
    ( MaybeOptions = ok(OptionTable),
        lookup_bool_option(OptionTable, help, Help),
        lookup_bool_option(OptionTable, version, Version),
        lookup_bool_option(OptionTable, verbose, Verbose),
        ( if Help = yes then
            Result = ok(pzasm_options(help, Verbose))
        else if Version = yes then
            Result = ok(pzasm_options(version, Verbose))
        else
            ( if Args = [InputFile] then
                ( if
                    lookup_string_option(OptionTable, output, Output0),
                    Output0 \= ""
                then
                    Output = Output0
                else
                    file_change_extension(constant.pz_text_extension,
                        constant.output_extension, InputFile, Output)
                ),

                Result = ok(pzasm_options(assemble(InputFile, Output),
                    Verbose))
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
    io.write_string("Plasma abstract machine assembler verison: dev\n", !IO),
    io.write_string("https://plasmalang.org\n", !IO),
    io.write_string("Copyright (C) 2015-2020 The Plasma Team\n", !IO),
    io.write_string("Distributed under the MIT License\n", !IO).

:- pred usage(io::di, io::uo) is det.

usage(!IO) :-
    io.progname_base("plzasm", ProgName, !IO),
    io.format("%s [-v] [-o <output> | --output <output>] <input>\n",
        [s(ProgName)], !IO),
    io.format("%s -h\n", [s(ProgName)], !IO).

:- type option
    --->    help
    ;       verbose
    ;       version
    ;       output.

:- pred short_option(char::in, option::out) is semidet.

short_option('h', help).
short_option('v', verbose).
short_option('o', output).

:- pred long_option(string::in, option::out) is semidet.

long_option("help",         help).
long_option("verbose",      verbose).
long_option("version",      version).
long_option("output",       output).

:- pred option_default(option::out, option_data::out) is multi.

option_default(help,        bool(no)).
option_default(verbose,     bool(no)).
option_default(version,     bool(no)).
option_default(output,      string("")).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
