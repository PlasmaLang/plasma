%-----------------------------------------------------------------------%
% Plasma compiler
% vim: ts=4 sw=4 et
%
% Copyright (C) 2015 Paul Bone
% Distributed under the terms of the GPLv2 see ../LICENSE.tools
%
% This program compiles plasma modules.
%
%-----------------------------------------------------------------------%
:- module plasmac.
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

:- import_module ast.
:- import_module ast_to_core.
:- import_module compile_error.
:- import_module core.
:- import_module core_to_pz.
:- import_module parse.
:- import_module pz.
:- import_module pz.write.
:- import_module result.
:- import_module util.

%-----------------------------------------------------------------------%

main(!IO) :-
    io.command_line_arguments(Args0, !IO),
    process_options(Args0, OptionsResult, !IO),
    ( OptionsResult = ok(PlasmaCOpts),
        Mode = PlasmaCOpts ^ pco_mode,
        ( Mode = compile,
            parse(PlasmaCOpts ^ pco_input_file, MaybePlasmaAst,
                !IO),
            ( MaybePlasmaAst = ok(PlasmaAst),
                compile(PlasmaAst, MaybePZ),
                ( MaybePZ = ok(PZ),
                    write_pz(PlasmaCOpts ^ pco_output_file, PZ, Result, !IO),
                    ( Result = ok
                    ; Result = error(ErrMsg),
                        exit_error(ErrMsg, !IO)
                    )
                ; MaybePZ = errors(Errors),
                    report_errors(Errors, !IO)
                )
            ; MaybePlasmaAst = errors(Errors),
                report_errors(Errors, !IO)
            )
        ; Mode = help,
            usage(!IO)
        )
    ; OptionsResult = error(ErrMsg),
        exit_error(ErrMsg, !IO)
    ).

%-----------------------------------------------------------------------%

:- type plasmac_options
    --->    plasmac_options(
                pco_mode            :: pco_mode,
                pco_input_file      :: string,
                pco_output_file     :: string,
                pco_verbose         :: bool
            ).

:- type pco_mode
    --->    compile
    ;       help.

:- pred process_options(list(string)::in, maybe_error(plasmac_options)::out,
    io::di, io::uo) is det.

process_options(Args0, Result, !IO) :-
    OptionOpts = option_ops_multi(short_option, long_option, option_default),
    getopt.process_options(OptionOpts, Args0, Args, MaybeOptions),
    ( MaybeOptions = ok(OptionTable),
        ( Args = [InputFile] ->
            lookup_bool_option(OptionTable, help, Help),
            ( Help = yes,
                Mode = help
            ; Help = no,
                Mode = compile
            ),
            (
                lookup_string_option(OptionTable, output, Output0),
                Output0 \= ""
            ->
                Output = Output0
            ;
                ( remove_suffix(InputFile, ".p", Base) ->
                    Output = Base ++ ".pz"
                ;
                    Output = InputFile ++ ".pz"
                )
            ),

            lookup_bool_option(OptionTable, verbose, Verbose),

            Options = plasmac_options(Mode, InputFile, Output, Verbose),
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
    io.progname_base("plasmac", ProgName, !IO),
    io.format("%s [-v] [-o <output> | --output <output>] <inputs>",
        [s(ProgName)], !IO),
    io.format("%s -h", [s(ProgName)], !IO).

:- type option
    --->    help
    ;       verbose
    ;       output.

:- pred short_option(char::in, option::out) is semidet.

short_option('h', help).
short_option('v', verbose).
short_option('o', output).

:- pred long_option(string::in, option::out) is semidet.

long_option("help",         help).
long_option("verbose",      verbose).
long_option("output",       output).

:- pred option_default(option::out, option_data::out) is multi.

option_default(help,        bool(no)).
option_default(verbose,     bool(no)).
option_default(output,      string("")).

%-----------------------------------------------------------------------%

:- pred compile(plasma_ast::in, result(pz, compile_error)::out) is det.

compile(AST, Result) :-
    ast_to_core(AST, CoreResult),
    ( CoreResult = ok(Core),
        core_to_pz(Core, PZ),
        Result = ok(PZ)
    ; CoreResult = errors(Errors),
        Result = errors(Errors)
    ).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
