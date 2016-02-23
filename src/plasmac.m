%-----------------------------------------------------------------------%
% Plasma compiler
% vim: ts=4 sw=4 et
%
% Copyright (C) 2015 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
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
:- import_module cord.
:- import_module getopt.
:- import_module list.
:- import_module maybe.
:- import_module string.

:- import_module ast.
:- import_module ast_to_core.
:- import_module compile_error.
:- import_module core.
:- import_module core.typecheck.
:- import_module core_to_pz.
:- import_module parse.
:- import_module pz.
:- import_module pz.write.
:- import_module pz.pretty.
:- import_module result.
:- import_module util.

%-----------------------------------------------------------------------%

main(!IO) :-
    io.command_line_arguments(Args0, !IO),
    process_options(Args0, OptionsResult, !IO),
    ( OptionsResult = ok(PlasmaCOpts),
        Mode = PlasmaCOpts ^ pco_mode,
        ( Mode = compile(CompileOpts),
            parse(CompileOpts ^ co_input_file, MaybePlasmaAst,
                !IO),
            ( MaybePlasmaAst = ok(PlasmaAst),
                compile(PlasmaAst, MaybePZ),
                ( MaybePZ = ok(PZ),
                    maybe_pretty_print(CompileOpts, PZ, !IO),
                    write_pz(CompileOpts ^ co_output_file, PZ, Result, !IO),
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

:- pred maybe_pretty_print(compile_options::in, pz::in, io::di, io::uo)
    is det.

maybe_pretty_print(CompileOpts, PZ, !IO) :-
    PrettyOutput = CompileOpts ^ co_pretty_output,
    ( PrettyOutput = yes,
        write_string(append_list(list(pz_pretty(PZ))), !IO)
    ; PrettyOutput = no
    ).

%-----------------------------------------------------------------------%

:- type plasmac_options
    --->    plasmac_options(
                pco_mode            :: pco_mode_options,
                pco_verbose         :: bool
            ).

:- type pco_mode_options
    --->    compile(
                pmo_compile_opts    :: compile_options
            )
    ;       help.

:- type compile_options
    --->    compile_options(
                co_input_file       :: string,
                co_output_file      :: string,
                co_pretty_output    :: bool
            ).

:- pred process_options(list(string)::in, maybe_error(plasmac_options)::out,
    io::di, io::uo) is det.

process_options(Args0, Result, !IO) :-
    OptionOpts = option_ops_multi(short_option, long_option, option_default),
    getopt.process_options(OptionOpts, Args0, Args, MaybeOptions),
    ( MaybeOptions = ok(OptionTable),
        lookup_bool_option(OptionTable, help, Help),
        lookup_bool_option(OptionTable, verbose, Verbose),
        ( Help = yes,
            Result = ok(plasmac_options(help, Verbose))
        ; Help = no,
            ( Args = [InputFile] ->
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

                lookup_bool_option(OptionTable, pretty_output, PrettyOutput),

                Result = ok(plasmac_options(compile(
                        compile_options(InputFile, Output, PrettyOutput)),
                    Verbose))
            ;
                Result = error("Error processing command line options: " ++
                    "Expected exactly one input file")
            )
        )
    ; MaybeOptions = error(ErrMsg),
        Result = error("Error processing command line options: " ++ ErrMsg)
    ).

:- pred usage(io::di, io::uo) is det.

usage(!IO) :-
    io.progname_base("plasmac", ProgName, !IO),
    io.format("%s <options> <input>\n", [s(ProgName)], !IO),
    io.write_string("\nOptions may include:\n", !IO),
    io.write_string("\t-v\n\t\tVerbose output\n\n", !IO),
    io.write_string("\t-o <output>  --output <output>\n" ++
        "\t\tSpecify output file\n\n", !IO),
    io.write_string("\t--pretty-output\n" ++
        "\t\tOutput the compiled code as pretty printed bytecode\n\n", !IO).

:- type option
    --->    help
    ;       verbose
    ;       output
    ;       pretty_output.

:- pred short_option(char::in, option::out) is semidet.

short_option('h', help).
short_option('v', verbose).
short_option('o', output).

:- pred long_option(string::in, option::out) is semidet.

long_option("help",             help).
long_option("verbose",          verbose).
long_option("output",           output).
long_option("pretty-output",    pretty_output).

:- pred option_default(option::out, option_data::out) is multi.

option_default(help,            bool(no)).
option_default(verbose,         bool(no)).
option_default(output,          string("")).
option_default(pretty_output,   bool(no)).

%-----------------------------------------------------------------------%

:- pred compile(plasma_ast::in, result(pz, compile_error)::out) is det.

compile(AST, Result) :-
    ast_to_core(AST, Core0Result),
    ( Core0Result = ok(Core0),
        semantic_checks(Core0, CoreResult),
        ( CoreResult = ok(Core),
            core_to_pz(Core, PZ),
            Result = ok(PZ)
        ; CoreResult = errors(Errors),
            Result = errors(Errors)
        )
    ; Core0Result = errors(Errors),
        Result = errors(Errors)
    ).

:- pred semantic_checks(core::in, result(core, compile_error)::out) is det.

semantic_checks(!.Core, Result) :-
    typecheck(TypecheckErrors, !Core),
    Errors = TypecheckErrors,
    ( if is_empty(Errors) then
        Result = ok(!.Core)
    else
        Result = errors(Errors)
    ).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
