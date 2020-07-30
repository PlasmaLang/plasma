%-----------------------------------------------------------------------%
% Plasma compiler
% vim: ts=4 sw=4 et
%
% Copyright (C) 2015-2020 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
% This program compiles plasma modules.
%
%-----------------------------------------------------------------------%
:- module plzc.
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
:- import_module require.
:- import_module string.

:- import_module ast.
:- import_module compile_error.
:- import_module constant.
:- import_module core.
:- import_module core.arity_chk.
:- import_module core.branch_chk.
:- import_module core.pretty.
:- import_module core.res_chk.
:- import_module core.simplify.
:- import_module core.type_chk.
:- import_module core_to_pz.
:- import_module dump_stage.
:- import_module options.
:- import_module parse.
:- import_module pre.
:- import_module pre.ast_to_core.
:- import_module pz.
:- import_module pz.pz_ds.
:- import_module pz.write.
:- import_module pz.pretty.
:- import_module q_name.
:- import_module util.
:- import_module util.exception.
:- import_module util.mercury.
:- import_module util.path.
:- import_module util.result.
:- import_module write_interface.

%-----------------------------------------------------------------------%

main(!IO) :-
    io.command_line_arguments(Args0, !IO),
    process_options(Args0, OptionsResult, !IO),
    ( OptionsResult = ok(PlasmaCOpts),
        ( PlasmaCOpts = plasmac_options(GeneralOpts, Mode),
            parse(GeneralOpts ^ go_input_file, MaybePlasmaAst, !IO),
            ( MaybePlasmaAst = ok(PlasmaAst),
                promise_equivalent_solutions [!:IO, HadErrors] (
                    ( Mode = compile(CompileOpts),
                        run_and_catch(do_compile(GeneralOpts, CompileOpts,
                                PlasmaAst),
                            plasmac, HadErrors, !IO)
                    ; Mode = make_interface,
                        run_and_catch(do_make_interface(GeneralOpts, PlasmaAst),
                            plasmac, HadErrors, !IO)
                    )
                ),
                ( HadErrors = had_errors,
                    io.set_exit_status(2, !IO)
                ; HadErrors = did_not_have_errors
                )
            ; MaybePlasmaAst = errors(Errors),
                report_errors(Errors, !IO),
                set_exit_status(1, !IO)
            )
        ; PlasmaCOpts = plasmac_help,
            usage(!IO)
        ; PlasmaCOpts = plasmac_version,
            version(!IO)
        )
    ; OptionsResult = error(ErrMsg),
        exit_error(ErrMsg, !IO)
    ).

:- pred do_compile(general_options::in, compile_options::in, ast::in,
    io::di, io::uo) is det.

do_compile(GeneralOpts, CompileOpts, PlasmaAst, !IO) :-
    compile(GeneralOpts, CompileOpts, PlasmaAst, MaybePZ, !IO),
    ( MaybePZ = ok(PZ, Errors),
        report_errors(Errors, !IO),
        ( if has_fatal_errors(Errors) then
            unexpected($file, $pred, "Fatal errors returned with result")
        else
            true
        ),

        ( if
            ( GeneralOpts ^ go_warn_as_error = no
            ; is_empty(Errors)
            )
        then
            WriteOutput = GeneralOpts ^ go_write_output,
            ( WriteOutput = write_output,
                OutputFile = GeneralOpts ^ go_dir ++ "/" ++
                    GeneralOpts ^ go_output_file,
                write_pz(OutputFile, pzft_object, PZ, Result, !IO),
                ( Result = ok
                ; Result = error(ErrMsg),
                    exit_error(ErrMsg, !IO)
                )
            ; WriteOutput = dont_write_output
            )
        else
            set_exit_status(1, !IO)
        )
    ; MaybePZ = errors(Errors),
        report_errors(Errors, !IO),
        set_exit_status(1, !IO)
    ).

:- pred do_make_interface(general_options::in, ast::in, io::di, io::uo) is det.

do_make_interface(GeneralOpts, PlasmaAst, !IO) :-
    make_interface(GeneralOpts, PlasmaAst, MaybeCore, !IO),
    ( MaybeCore = ok(Core, Errors),
        report_errors(Errors, !IO),
        ( if has_fatal_errors(Errors) then
            unexpected($file, $pred, "Fatal errors returned with result")
        else
            true
        ),

        ( if
            ( GeneralOpts ^ go_warn_as_error = no
            ; is_empty(Errors)
            )
        then
            WriteOutput = GeneralOpts ^ go_write_output,
            ( WriteOutput = write_output,
                % The interface is within the core representation. We will
                % extract and pretty print the parts we need.
                OutputFile = GeneralOpts ^ go_dir ++ "/" ++
                    GeneralOpts ^ go_output_file,
                write_interface(OutputFile, Core, Result, !IO),
                ( Result = ok
                ; Result = error(ErrMsg),
                    exit_error(ErrMsg, !IO)
                )
            ; WriteOutput = dont_write_output
            )
        else
            set_exit_status(1, !IO)
        )
    ; MaybeCore = errors(Errors),
        report_errors(Errors, !IO),
        set_exit_status(1, !IO)
    ).

%-----------------------------------------------------------------------%

:- type plasmac_options
    --->    plasmac_options(
                pco_general         :: general_options,
                pco_mode            :: pco_mode_options
            )
    ;       plasmac_help
    ;       plasmac_version.

:- type pco_mode_options
    --->    compile(
                pmo_compile_opts    :: compile_options
            )
    ;       make_interface.

:- pred process_options(list(string)::in, maybe_error(plasmac_options)::out,
    io::di, io::uo) is det.

process_options(Args0, Result, !IO) :-
    OptionOpts = option_ops_multi(short_option, long_option, option_default),
    getopt.process_options(OptionOpts, Args0, Args, MaybeOptions),
    ( MaybeOptions = ok(OptionTable),
        lookup_bool_option(OptionTable, help, Help),
        lookup_bool_option(OptionTable, version, Version),
        ( if Help = yes then
            Result = ok(plasmac_help)
        else if Version = yes then
            Result = ok(plasmac_version)
        else
            ( if Args = [InputPath] then
                lookup_bool_option(OptionTable, make_interface,
                    MakeInterfaceBool),

                ( MakeInterfaceBool = yes,
                    CompileOpts = make_interface,
                    OutputExtension = constant.interface_extension
                ; MakeInterfaceBool = no,
                    lookup_bool_option(OptionTable, simplify,
                        DoSimplifyBool),
                    ( DoSimplifyBool = yes,
                        DoSimplify = do_simplify_pass
                    ; DoSimplifyBool = no,
                        DoSimplify = skip_simplify_pass
                    ),

                    lookup_bool_option(OptionTable, tailcalls,
                        EnableTailcallsBool),
                    ( EnableTailcallsBool = yes,
                        EnableTailcalls = enable_tailcalls
                    ; EnableTailcallsBool = no,
                        EnableTailcalls = dont_enable_tailcalls
                    ),
                    CompileOpts = compile(
                        compile_options(DoSimplify, EnableTailcalls)),
                    OutputExtension = constant.output_extension
                ),

                file_and_dir(InputPath, InputDir, InputFile),
                file_change_extension(constant.source_extension,
                    OutputExtension, InputFile, Output),

                ( if
                    lookup_string_option(OptionTable, output_dir,
                        OutputDir0),
                    OutputDir0 \= ""
                then
                    OutputDir = OutputDir0
                else
                    OutputDir = InputDir
                ),

                lookup_bool_option(OptionTable, verbose, Verbose),
                lookup_bool_option(OptionTable, warn_as_error, WError),

                lookup_bool_option(OptionTable, dump_stages, DumpStagesBool),
                ( DumpStagesBool = yes,
                    DumpStages = dump_stages
                ; DumpStagesBool = no,
                    DumpStages = dont_dump_stages
                ),

                lookup_bool_option(OptionTable, write_output,
                    WriteOutputBool),
                ( WriteOutputBool = yes,
                    WriteOutput = write_output
                ; WriteOutputBool = no,
                    WriteOutput = dont_write_output
                ),

                GeneralOpts = general_options(OutputDir, InputPath, Output,
                    WError, Verbose, DumpStages, WriteOutput),
                Result = ok(plasmac_options(GeneralOpts, CompileOpts))
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
    io.write_string("Plasma Compiler verison: dev\n", !IO),
    io.write_string("https://plasmalang.org\n", !IO),
    io.write_string("Copyright (C) 2015-2020 The Plasma Team\n", !IO),
    io.write_string("Distributed under the MIT License\n", !IO).

:- pred usage(io::di, io::uo) is det.

usage(!IO) :-
    io.progname_base("plzc", ProgName, !IO),
    io.format("%s <options> <input>\n", [s(ProgName)], !IO),
    io.write_string("\nOptions may include:\n", !IO),
    io.write_string("\t-h\n\t\tHelp text (you're looking at it)\n\n", !IO),
    io.write_string("\t-v\n\t\tVerbose output\n\n", !IO),
    io.write_string("\t--version\n\t\tVersion information\n\n", !IO),
    io.write_string("\t--make-interface\n\t\tGenerate interface\n\n", !IO),
    io.write_string("\t-o <output-dir>  --output-dir <output-dir>\n" ++
        "\t\tSpecify location for output file\n\n", !IO),
    io.write_string("\t--warnings-as-errors\n\t\tAll warnings are fatal\n\n",
        !IO),
    io.write_string("\t--dump-stages\n" ++
        "\t\tDump the program representation at each stage of\n" ++
        "\t\tcompilation, each stage is saved to a seperate file in\n" ++
        "\t\tthe output directory\n\n", !IO).

% Developer only options:
%  --no-write-output
%   Don't actually write the output file - for testing.

:- type option
    --->    help
    ;       verbose
    ;       version
    ;       make_interface
    ;       output_dir
    ;       warn_as_error
    ;       dump_stages
    ;       write_output
    ;       simplify
    ;       tailcalls.

:- pred short_option(char::in, option::out) is semidet.

short_option('h', help).
short_option('v', verbose).
short_option('o', output_dir).

:- pred long_option(string::in, option::out) is semidet.

long_option("help",                 help).
long_option("verbose",              verbose).
long_option("version",              version).
long_option("make-interface",       make_interface).
long_option("output-dir",           output_dir).
long_option("warnings-as-errors",   warn_as_error).
long_option("dump-stages",          dump_stages).
long_option("write-output",         write_output).
long_option("simplify",             simplify).
long_option("tailcalls",            tailcalls).

:- pred option_default(option::out, option_data::out) is multi.

option_default(help,            bool(no)).
option_default(verbose,         bool(no)).
option_default(version,         bool(no)).
option_default(make_interface,  bool(no)).
option_default(output_dir,      string("")).
option_default(warn_as_error,   bool(no)).
option_default(dump_stages,     bool(no)).
option_default(write_output,    bool(yes)).
option_default(simplify,        bool(yes)).
option_default(tailcalls,       bool(yes)).

%-----------------------------------------------------------------------%

:- pred make_interface(general_options::in, ast::in,
    result_partial(core, compile_error)::out, io::di, io::uo) is det.

make_interface(GeneralOpts, AST, Result, !IO) :-
    ast_to_core(GeneralOpts, process_only_declarations, AST, Result, !IO).

%-----------------------------------------------------------------------%

:- pred compile(general_options::in, compile_options::in, ast::in,
    result_partial(pz, compile_error)::out, io::di, io::uo) is det.

compile(GeneralOpts, CompileOpts, AST, Result, !IO) :-
    ast_to_core(GeneralOpts, process_declarations_and_definitions, AST,
        Core0Result, !IO),
    ( Core0Result = ok(Core0, ErrorsA),
        maybe_dump_core_stage(GeneralOpts, "core0_initial", Core0, !IO),
        semantic_checks(GeneralOpts, CompileOpts, Core0, CoreResult, !IO),
        ( CoreResult = ok(Core),
            core_to_pz(CompileOpts, Core, PZ),
            maybe_dump_stage(GeneralOpts, module_name(Core),
                "pz0_final", pz_pretty, PZ, !IO),
            Result = ok(PZ, ErrorsA)
        ; CoreResult = errors(ErrorsB),
            Result = errors(ErrorsA ++ ErrorsB)
        )
    ; Core0Result = errors(Errors),
        Result = errors(Errors)
    ).

:- pred semantic_checks(general_options::in, compile_options::in, core::in,
    result(core, compile_error)::out, io::di, io::uo) is det.

semantic_checks(GeneralOpts, CompileOpts, !.Core, Result, !IO) :-
    some [!Errors] (
        !:Errors = init,
        Simplify = CompileOpts ^ co_do_simplify,
        ( Simplify = do_simplify_pass,
            simplify(SimplifyErrors, !Core),
            maybe_dump_core_stage(GeneralOpts, "core1_simplify", !.Core,
                !IO),
            add_errors(SimplifyErrors, !Errors)
        ; Simplify = skip_simplify_pass
        ),

        arity_check(ArityErrors, !Core),
        maybe_dump_core_stage(GeneralOpts, "core2_arity", !.Core, !IO),
        add_errors(ArityErrors, !Errors),

        ( if not has_fatal_errors(!.Errors) then
            type_check(TypecheckErrors, !Core),
            maybe_dump_core_stage(GeneralOpts, "core3_typecheck", !.Core,
                !IO),
            add_errors(TypecheckErrors, !Errors),

            branch_check(BranchcheckErrors, !Core),
            maybe_dump_core_stage(GeneralOpts, "core4_branch", !.Core, !IO),
            add_errors(BranchcheckErrors, !Errors),

            res_check(RescheckErrors, !Core),
            maybe_dump_core_stage(GeneralOpts, "core5_res", !.Core, !IO),
            add_errors(RescheckErrors, !Errors),

            ( if not has_fatal_errors(!.Errors) then
                Result = ok(!.Core)
            else
                Result = errors(!.Errors)
            )
        else
            Result = errors(!.Errors)
        )
    ).

%-----------------------------------------------------------------------%

:- pred maybe_dump_core_stage(general_options::in, string::in,
    core::in, io::di, io::uo) is det.

maybe_dump_core_stage(Opts, Stage, Core, !IO) :-
    maybe_dump_stage(Opts, module_name(Core), Stage, core_pretty, Core,
        !IO).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
