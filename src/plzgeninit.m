%-----------------------------------------------------------------------%
% Plasma foreign initialisation generation
% vim: ts=4 sw=4 et
%
% Copyright (C) Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
% This program assembles and links the pz intermediate representation.
%
%-----------------------------------------------------------------------%
:- module plzgeninit.
%-----------------------------------------------------------------------%

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module char.
:- import_module list.
:- import_module getopt.
:- import_module maybe.
:- import_module string.

:- import_module constant.
:- import_module q_name.
:- import_module util.
:- import_module util.mercury.
:- import_module util.my_exception.
:- import_module util.my_io.

%-----------------------------------------------------------------------%

main(!IO) :-
    io.command_line_arguments(Args0, !IO),
    process_options(Args0, OptionsResult, !IO),
    ( OptionsResult = ok(PZGIOpts),
        Mode = PZGIOpts ^ pzo_mode,
        ( Mode = gen_init(OutputFile, Modules),
            promise_equivalent_solutions [!:IO] (
                run_and_catch(do_gen_init(OutputFile, Modules), plzgeninit,
                    HadErrors, !IO),
                ( HadErrors = had_errors,
                    io.set_exit_status(1, !IO)
                ; HadErrors = did_not_have_errors
                )
            )
        ; Mode = help,
            usage(!IO)
        ; Mode = version,
            version("Plasma Foreign Interface Generator", !IO)
        )
    ; OptionsResult = error(ErrMsg),
        exit_error(ErrMsg, !IO)
    ).

:- pred do_gen_init(string::in, list(q_name)::in, io::di, io::uo) is det.

do_gen_init(OutputFile, Modules, !IO) :-
    write_temp_and_move(open_output, close_output,
        pred(F::in, R::out, IO0::di, IO::uo) is det :-
            write_gen_init(F, Modules, R, IO0, IO),
        OutputFile, Result, !IO),
    ( Result = ok
    ; Result = error(ErrMsg),
        exit_error(ErrMsg, !IO)
    ).

:- pred write_gen_init(output_stream::in, list(q_name)::in, maybe_error::out,
    io::di, io::uo) is det.

write_gen_init(File, Modules, Result, !IO) :-
    write_string(File, "// Foreign initialisation\n\n", !IO),
    write_string(File, "extern \"C\" {\n", !IO),
    write_string(File, "  bool pz_init_foreign_code(void *f, void *gc);\n", !IO),
    write_string(File, "}\n\n", !IO),

    % Forward declarations.
    foldl(write_declaration(File), Modules, !IO),

    write_string(File, "bool pz_init_foreign_code(void *f, void *gc) {\n", !IO),
    foldl(write_call(File), Modules, !IO),
    write_string(File, "  return true;\n", !IO),
    write_string(File, "}\n", !IO),

    Result = ok.

:- pred write_declaration(output_stream::in, q_name::in,
    io::di, io::uo) is det.

write_declaration(File, Module, !IO) :-
    format(File, "bool pz_init_foreign_code_%s(void *f, void *gc);\n",
        [s(q_name_clobber(Module))], !IO).

:- pred write_call(output_stream::in, q_name::in,
    io::di, io::uo) is det.

write_call(File, Module, !IO) :-
    format(File, "  if (!pz_init_foreign_code_%s(f, gc)) return false;\n",
        [s(q_name_clobber(Module))], !IO).

%%-----------------------------------------------------------------------%

:- type pzgi_options
    --->    pzgeninit_options(
                pzo_mode            :: pzgi_mode,
                pzo_verbose         :: bool
            ).

:- type pzgi_mode
    --->    gen_init(
                pzgi_output_file    :: string,
                pzgi_modules        :: list(q_name)
            )
    ;       help
    ;       version.

:- pred process_options(list(string)::in, maybe_error(pzgi_options)::out,
    io::di, io::uo) is det.

process_options(Args0, Result, !IO) :-
    OptionOpts = option_ops_multi(short_option, long_option, option_default),
    getopt.process_options(OptionOpts, Args0, Args, MaybeOptions),
    ( MaybeOptions = ok(OptionTable),
        lookup_bool_option(OptionTable, help, Help),
        lookup_bool_option(OptionTable, version, Version),
        lookup_bool_option(OptionTable, verbose, Verbose),
        ( if Help = yes then
            Result = ok(pzgeninit_options(help, Verbose))
        else if Version = yes then
            Result = ok(pzgeninit_options(version, Verbose))
        else
            ( if
                lookup_string_option(OptionTable, output, Output),
                Output \= ""
            then
                MaybeInputs =
                    maybe_error_list(map(q_name_from_dotted_string, Args)),
                ( MaybeInputs = ok(Inputs),
                    Result = ok(pzgeninit_options(
                        gen_init(Output, Inputs), Verbose))
                ; MaybeInputs = error(Errors),
                    Result = error("Invalid module name: " ++
                        first_item(Errors))
                )
            else
                Result = error("No output file specified")
            )
        )
    ; MaybeOptions = error(ErrMsg),
        Result = error("Error processing command line options: " ++
            option_error_to_string(ErrMsg))
    ).

:- pred usage(io::di, io::uo) is det.

usage(!IO) :-
    io.write_string("Plasma foreign initialisation generator\n\n", !IO),

    io.write_string(
        "    The Plasma foreign initialisation generator is used to\n" ++
        "    generate foreign code used to register foreign\n" ++
        "    implementations of Plasma functions.\n\n", !IO),

    io.write_string("Usage:\n\n", !IO),
    io.progname_base("plzgeninit", ProgName, !IO),
    io.format("    %s [-v] [-o <output> | --output <output>]\n",
        [s(ProgName)], !IO),
    io.format("    %s -h\n\n", [s(ProgName)], !IO).

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
