%-----------------------------------------------------------------------%
% Plasma compiler
% vim: ts=4 sw=4 et
%
% Copyright (C) Plasma Team
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
:- import_module common_types.
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
:- import_module foreign.
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
:- import_module util.my_exception.
:- import_module util.log.
:- import_module util.mercury.
:- import_module util.path.
:- import_module util.result.
:- import_module util.my_time.
:- import_module write_interface.

%-----------------------------------------------------------------------%

main(!IO) :-
    now(StartTime, !IO),
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
                    ; Mode = make_typeres_exports,
                        run_and_catch(
                            do_make_typeres_exports(GeneralOpts, PlasmaAst),
                            plzc, HadErrors, !IO)
                    ; Mode = scan(TargetBytecode, TargetInterface),
                        run_and_catch(
                            do_make_dep_info(GeneralOpts, TargetBytecode,
                                TargetInterface, PlasmaAst),
                            plzc, HadErrors, !IO)
                    ; Mode = make_foreign(OutputHeader),
                        run_and_catch(do_make_foreign(GeneralOpts,
                                OutputHeader, PlasmaAst),
                            plzc, HadErrors, !IO)
                    ),
                    ReportTiming = GeneralOpts ^ go_report_timing,
                    ( ReportTiming = report_command_times,
                        now(EndTime, !IO),
                        format("%s\n",
                            [s(format_duration(diff_time(EndTime, StartTime)))],
                            !IO)
                    ; ReportTiming = no_timing
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

%-----------------------------------------------------------------------%

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

%-----------------------------------------------------------------------%

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

%-----------------------------------------------------------------------%

:- pred do_make_dep_info(general_options::in,
    string::in, string::in, ast::in, io::di, io::uo) is det.

do_make_dep_info(GeneralOpts, TargetBytecode, TargetInterface, PlasmaAst,
        !IO) :-
    check_module_name(GeneralOpts, PlasmaAst ^ a_context,
        PlasmaAst ^ a_module_name, init, ModuleNameErrors),
    ( if has_fatal_errors(ModuleNameErrors) then
        report_errors(GeneralOpts ^ go_source_dir, ModuleNameErrors, !IO),
        set_exit_status(1, !IO)
    else
        filter_entries(PlasmaAst ^ a_entries, Imports0, _, _, _, _),
        % TODO: Include only dependencies required to build interface files,
        % that is those that are used by types and resources only.
        ast_to_import_list(PlasmaAst ^ a_module_name, "..",
            GeneralOpts ^ go_import_whitelist_file, Imports0, Imports, !IO),

        WriteOutput = GeneralOpts ^ go_write_output,
        ( WriteOutput = write_output,
            % The interface is within the core representation. We will
            % extract and pretty print the parts we need.
            OutputFile = GeneralOpts ^ go_output_file,
            write_dep_info(OutputFile, TargetBytecode, TargetInterface,
                Imports, Result, !IO),
            ( Result = ok
            ; Result = error(ErrMsg),
                exit_error(ErrMsg, !IO)
            )
        ; WriteOutput = dont_write_output
        )
    ).

:- pred write_dep_info(string::in, string::in, string::in,
    list(import_info)::in, maybe_error::out, io::di, io::uo) is det.

write_dep_info(Filename, TargetBytecode, TargetInterface, Info, Result, !IO) :-
    open_output(Filename, OpenRes, !IO),
    ( OpenRes = ok(File),
        Result = ok,
        write_string(File, "ninja_dyndep_version = 1\n\n", !IO),
        BytecodeDeps = string_join(" ",
            filter_map(ii_potential_interface_file(interface_import), Info)),
        InterfaceDeps = string_join(" ",
            filter_map(ii_potential_interface_file(typeres_import), Info)),
        format(File, "build %s : dyndep | %s\n\n",
            [s(TargetBytecode), s(BytecodeDeps)], !IO),
        format(File, "build %s : dyndep | %s\n\n",
            [s(TargetInterface), s(InterfaceDeps)], !IO),
        close_output(File, !IO)
    ; OpenRes = error(Error),
        Result = error(format("%s: %s", [s(Filename), s(error_message(Error))]))
    ).

    % Return the interface file for this module if it exists or we source
    % exists so it can be built.
    %
:- func ii_potential_interface_file(import_type, import_info) = string
    is semidet.

ii_potential_interface_file(ImportType, ImportInfo) = File :-
    ( ImportType = interface_import,
        File = ImportInfo ^ ii_interface_file
    ; ImportType = typeres_import,
        File = ImportInfo ^ ii_typeres_file
    ),
    ( file_exists = ImportInfo ^ ii_interface_exists
    ; yes(_) = ImportInfo ^ ii_source_file
    ).

%-----------------------------------------------------------------------%

:- pred do_make_typeres_exports(general_options::in, ast::in, io::di, io::uo)
    is det.

do_make_typeres_exports(GeneralOpts, PlasmaAst, !IO) :-
    ExportsRes = find_typeres_exports(GeneralOpts, PlasmaAst),
    SourcePath = GeneralOpts ^ go_source_dir,
    ( ExportsRes = ok(Exports, Errors),
        WriteOutput = GeneralOpts ^ go_write_output,
        ( WriteOutput = write_output,
            OutputFile = GeneralOpts ^ go_output_file,
            write_typeres_exports(OutputFile, PlasmaAst ^ a_module_name,
                Exports, Result, !IO),
            ( Result = ok
            ; Result = error(ErrMsg),
                exit_error(ErrMsg, !IO)
            )
        ; WriteOutput = dont_write_output
        ),
        report_errors(SourcePath, Errors, !IO)
    ; ExportsRes = errors(Errors),
        report_errors(SourcePath, Errors, !IO),
        exit_error("Failed", !IO)
    ).

:- pred write_typeres_exports(string::in, q_name::in, typeres_exports::in,
    maybe_error::out, io::di, io::uo) is det.

write_typeres_exports(Filename, ModuleName, Exports, Result, !IO) :-
    io.open_output(Filename, OpenRes, !IO),
    ( OpenRes = ok(File),
        format(File, "module %s\n\n", [s(q_name_to_string(ModuleName))],
            !IO),
        write_string(File, append_list(
            map(func(R) = format("resource %s\n", [s(q_name_to_string(R))]),
                Exports ^ te_resources)),
            !IO),
        nl(File, !IO),
        write_string(File, append_list(
            map(func({N, A}) = format("type %s/%d\n",
                [s(q_name_to_string(N)), i(A ^ a_num)]),
                Exports ^ te_types)),
            !IO),
        close_output(File, !IO),
        Result = ok
    ; OpenRes = error(Error),
        Result = error(format("%s: %s\n",
            [s(Filename), s(error_message(Error))]))
    ).

%-----------------------------------------------------------------------%

:- pred do_make_foreign(general_options::in, string::in, ast::in,
    io::di, io::uo) is det.

do_make_foreign(GeneralOpts, OutputHeader, PlasmaAst, !IO) :-
    MaybeForeignInfo = make_foreign(PlasmaAst),
    ( MaybeForeignInfo = ok(ForeignInfo),
        write_foreign(GeneralOpts, OutputHeader, ForeignInfo, !IO)
    ; MaybeForeignInfo = errors(Errors),
        report_errors(GeneralOpts ^ go_source_dir, Errors, !IO),
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
    ;       make_interface
    ;       make_typeres_exports
    ;       scan(
                pmo_d_output        :: string,
                pmo_d_interface     :: string
            )
    ;       make_foreign(
                pmo_f_output_header :: string
            ).

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
        Result = error("Error processing command line options: " ++
		option_error_to_string(ErrMsg))
    ).

:- pred process_options_mode(option_table(option)::in, string::out,
    maybe_error(pco_mode_options)::out) is det.

process_options_mode(OptionTable, OutputExtension, Result) :-
    lookup_string_option(OptionTable, mode_, Mode),
    ( if Mode = "compile" then
        DoSimplify = handle_bool_option(OptionTable, simplify,
            do_simplify_pass, skip_simplify_pass),
        EnableTailcalls = handle_bool_option(OptionTable, tailcalls,
            enable_tailcalls, dont_enable_tailcalls),
        Result = ok(compile(
            compile_options(DoSimplify, EnableTailcalls))),
        OutputExtension = constant.output_extension
    else if Mode = "make-interface" then
        Result = ok(make_interface),
        OutputExtension = constant.interface_extension
    else if Mode = "make-typeres-exports" then
        Result = ok(make_typeres_exports),
        OutputExtension = constant.typeres_extension
    else if Mode = "scan" then
        lookup_string_option(OptionTable, target_bytecode, TargetBytecode),
        lookup_string_option(OptionTable, target_interface, TargetInterface),
        Result = ok(scan(TargetBytecode, TargetInterface)),
        OutputExtension = constant.depends_extension
    else if Mode = "generate-foreign" then
        lookup_string_option(OptionTable, output_header, OutputHeader),
        Result = ok(make_foreign(OutputHeader)),
        OutputExtension = constant.cpp_extension
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
        file_change_extension_det(constant.source_extension,
            OutputExtension, InputFile, OutputFile)
    ),

    lookup_string_option(OptionTable, import_whitelist,
        ImportWhitelist),
    ( if ImportWhitelist = "" then
        MbImportWhitelist = no
    else
        MbImportWhitelist = yes(ImportWhitelist)
    ),

    lookup_string_option(OptionTable, module_name_check,
        ModuleNameCheck),
    ( if ModuleNameCheck = "" then
        MbModuleNameCheck = no
    else
        MbModuleNameCheck = yes(ModuleNameCheck)
    ),

    Verbose = handle_bool_option(OptionTable, verbose, verbose, silent),
    lookup_bool_option(OptionTable, warn_as_error, WError),
    DumpStages = handle_bool_option(OptionTable, dump_stages,
        dump_stages, dont_dump_stages),
    WriteOutput = handle_bool_option(OptionTable, write_output,
        write_output, dont_write_output),
    ReportTiming = handle_bool_option(OptionTable, report_timing,
        report_command_times, no_timing),

    GeneralOpts = general_options(InputDir, SourcePath, InputPath,
        OutputFile, MbImportWhitelist, MbModuleNameCheck, WError, Verbose,
        DumpStages, WriteOutput, ReportTiming).

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
    io.format("    %s [-v] --mode make-interface -o <output> <input>\n",
        [s(ProgName)], !IO),
    io.write_string("        Make interface mode.\n\n", !IO),
    io.format("    %s [-v] --mode make-typeres-exports -o <output> <input>\n",
        [s(ProgName)], !IO),
    io.write_string("        Make the typeres interface file.\n\n", !IO),
    io.format("    %s [-v] --mode scan \n",
        [s(ProgName)], !IO),
    io.write_string("            --target-bytecode $bytecode\n", !IO),
    io.write_string("            --target-interface $interface\n", !IO),
    io.write_string("            -o <output> <input>\n", !IO),
    io.write_string("        Scan source for dependencies.\n\n", !IO),
    io.format("    %s [-v] --mode generate-foreign -o <output> <input>\n",
        [s(ProgName)], !IO),
    io.write_string("        Generate runtime code required to register foeign functions.\n\n", !IO),

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
        "        scan                 - " ++
        "Scan code for dependency information,\n" ++
        "        make-interface       - Generate the interface file,\n" ++
        "        make-typeres-exports - " ++
        "Generate the typeres interface file.\n" ++
        "        compile (default)    - Compile the module,\n" ++
        "        generate-foreign     - " ++
        "Generate foreign code registration.\n\n", !IO),

    io.write_string("Scan options:\n\n", !IO),
    io.write_string("    --target-bytecode <file>\n" ++
        "        <file> is the name of the bytecode file in the ninja\n" ++
        "        build file\n\n",
        !IO),
    io.write_string("   --target-interface <file>\n" ++
        "        <file> is the name of the interface file in the ninja\n" ++
        "        build file\n\n",
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
    io.write_string("    --module-name-check <name>\n" ++
        "        Check that this is the module name in the source file.\n\n",
        !IO),
    io.write_string("    --source-path <path>\n" ++
        "        Subtract this path from source filenames when printing\n" ++
        "        errors.\n\n", !IO),
    io.write_string("    --report-timing\n" ++
        "        Report the time taken to execute the compiler.\n\n", !IO).

:- type option
    --->    help
    ;       verbose
    ;       version
    ;       mode_
    ;       output_file
    ;       output_header
    ;       target_bytecode
    ;       target_interface
    ;       import_whitelist
    ;       module_name_check
    ;       source_path
    ;       warn_as_error
    ;       dump_stages
    ;       write_output
    ;       report_timing
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
long_option("output-header",        output_header).
long_option("target-bytecode",      target_bytecode).
long_option("target-interface",     target_interface).
long_option("import-whitelist",     import_whitelist).
long_option("module-name-check",    module_name_check).
long_option("source-path",          source_path).
long_option("warnings-as-errors",   warn_as_error).
long_option("dump-stages",          dump_stages).
long_option("write-output",         write_output).
long_option("report-timing",        report_timing).
long_option("simplify",             simplify).
long_option("tailcalls",            tailcalls).

:- pred option_default(option::out, option_data::out) is multi.

option_default(help,                bool(no)).
option_default(verbose,             bool(no)).
option_default(version,             bool(no)).
option_default(mode_,               string("compile")).
option_default(output_file,         string("")).
option_default(output_header,       string("")).
option_default(target_bytecode,     string("")).
option_default(target_interface,    string("")).
option_default(import_whitelist,    string("")).
option_default(module_name_check,   string("")).
option_default(source_path,         string("")).
option_default(warn_as_error,       bool(no)).
option_default(dump_stages,         bool(no)).
option_default(write_output,        bool(yes)).
option_default(report_timing,       bool(no)).
option_default(simplify,            bool(yes)).
option_default(tailcalls,           bool(yes)).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
