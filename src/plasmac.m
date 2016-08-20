%-----------------------------------------------------------------------%
% Plasma compiler
% vim: ts=4 sw=4 et
%
% Copyright (C) 2015-2016 Plasma Team
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
:- import_module int.
:- import_module list.
:- import_module maybe.
:- import_module string.

:- import_module ast.
:- import_module compile_error.
:- import_module core.
:- import_module core.pretty.
:- import_module core.typecheck.
:- import_module core_to_pz.
:- import_module dump_stage.
:- import_module options.
:- import_module parse.
:- import_module pre.
:- import_module pre.ast_to_core.
:- import_module pz.
:- import_module pz.write.
:- import_module pz.pretty.
:- import_module result.
:- import_module q_name.
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
                compile(CompileOpts, PlasmaAst, MaybePZ, !IO),
                ( MaybePZ = ok(PZ),
                    OutputFile = CompileOpts ^ co_dir ++ "/" ++
                        CompileOpts ^ co_output_file,
                    write_pz(OutputFile, PZ, Result, !IO),
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
                pco_mode            :: pco_mode_options,
                pco_verbose         :: bool
            ).

:- type pco_mode_options
    --->    compile(
                pmo_compile_opts    :: compile_options
            )
    ;       help.

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
                FilePartLength = suffix_length((pred(C::in) is semidet :-
                        C \= ('/')
                    ), InputFile),
                ( if
                    lookup_string_option(OptionTable, output_dir,
                        OutputDir0),
                    OutputDir0 \= ""
                then
                    OutputDir = OutputDir0
                else
                    % This length is in code units.
                    left(InputFile, length(InputFile) - FilePartLength - 1,
                        OutputDir0),
                    ( if OutputDir0 \= "" then
                        OutputDir = OutputDir0
                    else
                        OutputDir = "."
                    )
                ),
                ( if
                    right(InputFile, FilePartLength, InputFilePart),
                    remove_suffix(InputFilePart, ".p", Base)
                then
                    Output = Base ++ ".pz"
                else
                    Output = InputFile ++ ".pz"
                ),

                lookup_bool_option(OptionTable, dump_stages, DumpStagesBool),
                ( DumpStagesBool = yes,
                    DumpStages = dump_stages
                ; DumpStagesBool = no,
                    DumpStages = dont_dump_stages
                ),

                Result = ok(plasmac_options(compile(
                        compile_options(OutputDir, InputFile, Output,
                            DumpStages)),
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
    io.write_string("\t-o <output-dir>  --output-dir <output-dir>\n" ++
        "\t\tSpecify location for output file\n\n", !IO),
    io.write_string("\t--dump-stages\n" ++
        "\t\tDump the program representation at each stage of\n" ++
        "\t\tcompilation, each stage is saved to a seperate file in\n" ++
        "\t\tthe output directory\n\n", !IO).

:- type option
    --->    help
    ;       verbose
    ;       output_dir
    ;       dump_stages.

:- pred short_option(char::in, option::out) is semidet.

short_option('h', help).
short_option('v', verbose).
short_option('o', output_dir).

:- pred long_option(string::in, option::out) is semidet.

long_option("help",             help).
long_option("verbose",          verbose).
long_option("output-dir",       output_dir).
long_option("dump-stages",      dump_stages).

:- pred option_default(option::out, option_data::out) is multi.

option_default(help,            bool(no)).
option_default(verbose,         bool(no)).
option_default(output_dir,      string("")).
option_default(dump_stages,     bool(no)).

%-----------------------------------------------------------------------%

:- pred compile(compile_options::in, ast::in,
    result(pz, compile_error)::out, io::di, io::uo) is det.

compile(CompileOpts, AST, Result, !IO) :-
    ast_to_core(CompileOpts, AST, Core0Result, !IO),
    ( Core0Result = ok(Core0),
        maybe_dump_core_stage(CompileOpts, "core0_initial", Core0, !IO),
        semantic_checks(CompileOpts, Core0, CoreResult, !IO),
        ( CoreResult = ok(Core),
            core_to_pz(Core, PZ),
            maybe_dump_stage(CompileOpts, module_name(Core),
                "pz0_final", pz_pretty, PZ, !IO),
            Result = ok(PZ)
        ; CoreResult = errors(Errors),
            Result = errors(Errors)
        )
    ; Core0Result = errors(Errors),
        Result = errors(Errors)
    ).

:- pred semantic_checks(compile_options::in, core::in,
    result(core, compile_error)::out, io::di, io::uo) is det.

semantic_checks(CompileOpts, !.Core, Result, !IO) :-
    typecheck(TypecheckErrors, !Core),
    maybe_dump_core_stage(CompileOpts, "core1_final", !.Core, !IO),
    Errors = TypecheckErrors,
    ( if is_empty(Errors) then
        Result = ok(!.Core)
    else
        Result = errors(Errors)
    ).

%-----------------------------------------------------------------------%

:- pred maybe_dump_core_stage(compile_options::in, string::in,
    core::in, io::di, io::uo) is det.

maybe_dump_core_stage(CompileOpts, Stage, Core, !IO) :-
    maybe_dump_stage(CompileOpts, module_name(Core), Stage,
        core_pretty, Core, !IO).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
