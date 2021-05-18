%-----------------------------------------------------------------------%
% Plasma compiler
% vim: ts=4 sw=4 et
%
% Copyright (C) 2015-2021 Plasma Team
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
:- import_module compile.
:- import_module compile_error.
:- import_module constant.
:- import_module context.
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
:- import_module pre.import.
:- import_module pz.
:- import_module pz.pz_ds.
:- import_module pz.write.
:- import_module pz.pretty.
:- import_module q_name.
:- import_module util.
:- import_module util.exception.
:- import_module util.log.
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
            verbose_output(GeneralOpts ^ go_verbose,
                format("Parsing %s\n", [s(GeneralOpts ^ go_input_file)]),
                !IO),
            parse(GeneralOpts ^ go_input_file, MaybePlasmaAst, !IO),
            ( MaybePlasmaAst = ok(PlasmaAst),
                promise_equivalent_solutions [!:IO, HadErrors] (
                    ( Mode = compile(CompileOpts),
                        run_and_catch(do_compile(GeneralOpts, CompileOpts,
                                PlasmaAst),
                            plzc, HadErrors, !IO)
                    ; Mode = make_interface,
                        run_and_catch(do_make_interface(GeneralOpts, PlasmaAst),
                            plzc, HadErrors, !IO)
                    ; Mode = make_depends(Target),
                        run_and_catch(do_make_dep_info(GeneralOpts, Target,
                                PlasmaAst),
                            plzc, HadErrors, !IO)
                    )
                ),
                ( HadErrors = had_errors,
                    io.set_exit_status(2, !IO)
                ; HadErrors = did_not_have_errors
                )
            ; MaybePlasmaAst = errors(Errors),
                report_errors(GeneralOpts ^ go_source_dir, Errors, !IO),
                set_exit_status(1, !IO)
            )
        ; PlasmaCOpts = plasmac_help,
            usage(!IO)
        ; PlasmaCOpts = plasmac_version,
            version("Plasma Compiler", !IO)
        )
    ; OptionsResult = error(ErrMsg),
        exit_error(ErrMsg, !IO)
    ).

:- pred do_compile(general_options::in, compile_options::in, ast::in,
    io::di, io::uo) is det.

do_compile(GeneralOpts, CompileOpts, PlasmaAst, !IO) :-
    compile(GeneralOpts, CompileOpts, PlasmaAst, MaybePZ, !IO),
    ( MaybePZ = ok(PZ, Errors),
        report_errors(GeneralOpts ^ go_source_dir, Errors, !IO),
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
                OutputFile = GeneralOpts ^ go_output_file,
                write_pz(OutputFile, PZ, Result, !IO),
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
        report_errors(GeneralOpts ^ go_source_dir, Errors, !IO),
        set_exit_status(1, !IO)
    ).

:- pred do_make_interface(general_options::in, ast::in, io::di, io::uo) is det.

do_make_interface(GeneralOpts, PlasmaAst, !IO) :-
    process_declarations(GeneralOpts, PlasmaAst, MaybeCore, !IO),
    ( MaybeCore = ok(Core, Errors),
        report_errors(GeneralOpts ^ go_source_dir, Errors, !IO),
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
                OutputFile = GeneralOpts ^ go_output_file,
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
        report_errors(GeneralOpts ^ go_source_dir, Errors, !IO),
        set_exit_status(1, !IO)
    ).

:- pred do_make_dep_info(general_options::in, string::in, ast::in,
    io::di, io::uo) is det.

do_make_dep_info(GeneralOpts, Target, PlasmaAst, !IO) :-
    filter_entries(PlasmaAst ^ a_entries, Imports0, _, _, _),
    ast_to_import_list(PlasmaAst ^ a_module_name, "..",
        GeneralOpts ^ go_import_whitelist_file, Imports0, Imports, !IO),

    WriteOutput = GeneralOpts ^ go_write_output,
    ( WriteOutput = write_output,
        % The interface is within the core representation. We will
        % extract and pretty print the parts we need.
        OutputFile = GeneralOpts ^ go_output_file,
        write_dep_info(OutputFile, Target, Imports, Result, !IO),
        ( Result = ok
        ; Result = error(ErrMsg),
            exit_error(ErrMsg, !IO)
        )
    ; WriteOutput = dont_write_output
    ).

:- pred write_dep_info(string::in, string::in, list(import_info)::in,
    maybe_error::out, io::di, io::uo) is det.

write_dep_info(Filename, Target, Info, Result, !IO) :-
    open_output(Filename, OpenRes, !IO),
    ( OpenRes = ok(File),
        Result = ok,
        write_string(File, "ninja_dyndep_version = 1\n\n", !IO),
        Deps = string_join(" ", filter_map(ii_potential_interface_file, Info)),
        format(File, "build %s : dyndep | %s\n\n", [s(Target), s(Deps)],
            !IO),
        close_output(File, !IO)
    ; OpenRes = error(Error),
        Result = error(format("%s: %s", [s(Filename), s(error_message(Error))]))
    ).

    % Return the interface file for this module if it exists or we source
    % exists so it can be built.
    %
:- func ii_potential_interface_file(import_info) = string is semidet.

ii_potential_interface_file(ImportInfo) = File :-
    File = ImportInfo ^ ii_interface_file,
    ( file_exists = ImportInfo ^ ii_interface_exists
    ; yes(_) = ImportInfo ^ ii_source_file
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
    ;       make_interface
    ;       make_depends(string).

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
                process_options_mode(OptionTable, OutputExtension,
                    ModeResult),
                GeneralOpts = process_options_general(OptionTable, InputPath,
                    OutputExtension),

                ( ModeResult = ok(ModeOpts),
                    Result = ok(plasmac_options(GeneralOpts, ModeOpts))
                ; ModeResult = error(Error),
                    Result = error(Error)
                )
            else
                Result = error("Error processing command line options: " ++
                    "Expected exactly one input file")
            )
        )
    ; MaybeOptions = error(ErrMsg),
        Result = error("Error processing command line options: " ++ ErrMsg)
    ).

:- pred process_options_mode(option_table(option)::in, string::out,
    maybe_error(pco_mode_options)::out) is det.

process_options_mode(OptionTable, OutputExtension, Result) :-
    lookup_string_option(OptionTable, mode_, Mode),
    lookup_string_option(OptionTable, target_file, TargetFile),
    ( if Mode = "compile" then
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
        Result = ok(compile(
            compile_options(DoSimplify, EnableTailcalls))),
        OutputExtension = constant.output_extension
    else if Mode = "make-depends" then
        Result = ok(make_depends(TargetFile)),
        OutputExtension = constant.depends_extension
    else if Mode = "make-interface" then
        Result = ok(make_interface),
        OutputExtension = constant.interface_extension
    else
        Result = error(
            format("Error processing command line options, " ++
                    "unknown mode `%s`.",
                [s(Mode)])),
        OutputExtension = ".error" % This is never seen
    ).

:- func process_options_general(option_table(option), string, string) =
    general_options.

process_options_general(OptionTable, InputPath, OutputExtension) =
        GeneralOpts :-
    lookup_string_option(OptionTable, source_path,
        SourcePath),
    file_and_dir_det(".", InputPath, InputDir, InputFile),

    ( if
        lookup_string_option(OptionTable, output_file,
            OutputFile0),
        OutputFile0 \= ""
    then
        OutputFile = OutputFile0
    else
        file_change_extension(constant.source_extension,
            OutputExtension, InputFile, OutputFile)
    ),

    lookup_string_option(OptionTable, import_whitelist,
        ImportWhitelist),
    ( if ImportWhitelist = "" then
        MbImportWhitelist = no
    else
        MbImportWhitelist = yes(ImportWhitelist)
    ),

    lookup_bool_option(OptionTable, verbose, VerboseBool),
    ( VerboseBool = yes,
        Verbose = verbose
    ; VerboseBool = no,
        Verbose = silent
    ),
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

    GeneralOpts = general_options(InputDir, SourcePath, InputPath,
        OutputFile, MbImportWhitelist, WError, Verbose,
        DumpStages, WriteOutput).

:- pred usage(io::di, io::uo) is det.

usage(!IO) :-
    io.write_string("Plasma compiler\n\n", !IO),
    io.write_string(
        "    The plasma compiler compiles plasma source code modules and\n" ++
        "    generates bytecode.  It also has other modes to generate\n" ++
        "    interface files or dependency information.\n\n", !IO),

    io.write_string("Usage:\n\n", !IO),
    io.progname_base("plzc", ProgName, !IO),
    io.format("    %s -h | --help\n", [s(ProgName)], !IO),
    io.format("    %s --version\n\n", [s(ProgName)], !IO),
    io.format("    %s [-v] -o <output> [compilation opts] <input>\n",
        [s(ProgName)], !IO),
    io.write_string("        Compilation mode.\n\n", !IO),
    io.format("    %s [-v] --make-interface -o <output> <input>\n",
        [s(ProgName)], !IO),
    io.write_string("        Make interface mode.\n\n", !IO),
    io.format("    %s [-v] --make-depend-info <target> -o <output> <input>\n",
        [s(ProgName)], !IO),
    io.write_string("        Make depend info mode.\n\n", !IO),
    io.write_string("General options:\n\n", !IO),
    io.write_string("    -h | --help\n" ++
        "        Help text (you're looking at it)\n\n", !IO),
    io.write_string("    -v | --verbose\n" ++
        "        Verbose output\n\n", !IO),
    io.write_string("    --version\n" ++
        "        Version information\n\n", !IO),
    io.write_string(
        "    -o <output-file> | --output-file <output-file>\n" ++
        "        Specify output file (compiler will guess otherwise)\n\n", !IO),
    io.write_string("    --mode MODE\n" ++
        "        Specify what the compiler should do:\n" ++
        "        make-depend-info  - Generate dependency info for ninja,\n" ++
        "        make-interface    - Generate the interface file,\n" ++
        "        compile (default) - Compile the module,\n\n", !IO),

    io.write_string("Make depend info options:\n\n", !IO),
    io.write_string("    --target-file <target>\n" ++
        "        <target> is the name of the target in the ninja file\n\n",
        !IO),

    io.write_string("Compilation options:\n\n", !IO),
    io.write_string("    --warnings-as-errors\n" ++
        "        All warnings are fatal\n\n", !IO),
    io.write_string("    --no-simplify\n" ++
        "        Disable the simplification optimisations\n\n", !IO),

    io.write_string("Developer options:\n\n", !IO),
    io.write_string("    --dump-stages\n" ++
        "        Dump the program representation at each stage of\n" ++
        "        compilation, each stage is saved to a seperate file in\n" ++
        "        the output directory\n\n", !IO),
    io.write_string("    --no-write-output\n" ++
        "        Skip writing the output file (for testing)\n\n", !IO),
    io.write_string("    --no-tailcalls\n" ++
        "        Do not generate tailcalls\n\n", !IO),

    io.write_string("Internal options:\n\n", !IO),
    io.write_string("    --import-whitelist <file>\n" ++
        "        Imports are checked against the Mercury term in this file\n" ++
        "        generated by plzbuild.\n\n", !IO),
    io.write_string("    --source-path <path>\n" ++
        "        Subtract this path from source filenames when printing\n" ++
        "        errors.\n\n", !IO).

:- type option
    --->    help
    ;       verbose
    ;       version
    ;       mode_
    ;       output_file
    ;       target_file
    ;       import_whitelist
    ;       source_path
    ;       warn_as_error
    ;       dump_stages
    ;       write_output
    ;       simplify
    ;       tailcalls.

:- pred short_option(char::in, option::out) is semidet.

short_option('h', help).
short_option('v', verbose).
short_option('o', output_file).

:- pred long_option(string::in, option::out) is semidet.

long_option("help",                 help).
long_option("verbose",              verbose).
long_option("version",              version).
long_option("mode",                 mode_).
long_option("output-file",          output_file).
long_option("target-file",          target_file).
long_option("import-whitelist",     import_whitelist).
long_option("source-path",          source_path).
long_option("warnings-as-errors",   warn_as_error).
long_option("dump-stages",          dump_stages).
long_option("write-output",         write_output).
long_option("simplify",             simplify).
long_option("tailcalls",            tailcalls).

:- pred option_default(option::out, option_data::out) is multi.

option_default(help,                bool(no)).
option_default(verbose,             bool(no)).
option_default(version,             bool(no)).
option_default(mode_,               string("compile")).
option_default(output_file,         string("")).
option_default(target_file,         string("")).
option_default(import_whitelist,    string("")).
option_default(source_path,         string("")).
option_default(warn_as_error,       bool(no)).
option_default(dump_stages,         bool(no)).
option_default(write_output,        bool(yes)).
option_default(simplify,            bool(yes)).
option_default(tailcalls,           bool(yes)).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
