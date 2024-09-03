%-----------------------------------------------------------------------%
% Plasma foreign stub generation
% vim: ts=4 sw=4 et
%
% Copyright (C) Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
% This module includes code for generating the code that registers foreign
% code with the runtime system.
%
%-----------------------------------------------------------------------%
:- module foreign.
%-----------------------------------------------------------------------%
:- interface.

:- import_module io.

:- import_module ast.
:- import_module compile_error.
:- import_module options.
:- import_module util.
:- import_module util.result.

%-----------------------------------------------------------------------%

:- type foreign_info.

:- func make_foreign(ast) = result(foreign_info, compile_error).

:- pred write_foreign(general_options::in, string::in, foreign_info::in,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
:- implementation.

:- import_module cord.
:- import_module list.
:- import_module maybe.
:- import_module string.

:- import_module compile.
:- import_module q_name.
:- import_module util.mercury.
:- import_module util.my_exception.
:- import_module util.my_io.

%-----------------------------------------------------------------------%

:- type foreign_info
    --->    foreign_info(
                fi_module_name      :: q_name,
                fi_includes         :: list(foreign_include),
                fi_funcs            :: list(foreign_func)
            ).

%-----------------------------------------------------------------------%

make_foreign(PlasmaAst) = MaybeForeignInfo :-
    MaybeIncludes = find_foreign_includes(PlasmaAst),
    Funcs = find_foreign_funcs(PlasmaAst),
    ( MaybeIncludes = ok(Includes),
        MaybeForeignInfo = ok(foreign_info(PlasmaAst ^ a_module_name,
            Includes, Funcs))
    ; MaybeIncludes = errors(Errors),
        MaybeForeignInfo = errors(Errors)
    ).

%-----------------------------------------------------------------------%

write_foreign(GeneralOpts, OutputHeader, ForeignInfo, !IO) :-
    WriteOutput = GeneralOpts ^ go_write_output,
    ( WriteOutput = write_output,
        OutputFile = GeneralOpts ^ go_output_file,
        write_foreign_hooks(OutputFile, OutputHeader,
            ForeignInfo ^ fi_module_name,
            ForeignInfo ^ fi_includes,
            ForeignInfo ^ fi_funcs, Result, !IO),
        ( Result = ok
        ; Result = error(ErrMsg),
            exit_error(ErrMsg, !IO)
        )
    ; WriteOutput = dont_write_output
    ).

%-----------------------------------------------------------------------%

:- type foreign_include
    --->    foreign_include(string).

:- func find_foreign_includes(ast) =
    result(list(foreign_include), compile_error).

find_foreign_includes(Ast) = MaybeForeignIncludes :-
    Ast = ast(_, _, Entries),
    filter_entries(Entries, _, _, _, _, Pragmas),
    foldl_result(find_foreign_include_pragma, Pragmas,
        [], MaybeForeignIncludes0),
    MaybeForeignIncludes = result_map(reverse, MaybeForeignIncludes0).

:- pred find_foreign_include_pragma(ast_pragma::in, list(foreign_include)::in,
    result(list(foreign_include), compile_error)::out) is det.

find_foreign_include_pragma(ast_pragma(Name, Args, Context),
        Includes0, MaybeIncludes) :-
    ( if Name = "foreign_include" then
        ( if Args = [ast_pragma_arg(String)] then
            Include = foreign_include(String),
            MaybeIncludes = ok([Include | Includes0])
        else
            MaybeIncludes = return_error(Context,
                ce_pragma_bad_argument)
        )
    else
        MaybeIncludes = ok(Includes0)
    ).

%-----------------------------------------------------------------------%

:- type foreign_func
    --->    foreign_func(
                ff_plasma_name      :: nq_name,
                ff_foreign_name     :: string
            ).

:- func find_foreign_funcs(ast) = list(foreign_func).

find_foreign_funcs(Ast) = ForeignFuncs :-
    Ast = ast(_, _, Entries),
    filter_entries(Entries, _, _, _, Funcs, _),

    filter_map(
        (pred(nq_named(Name, Func)::in, ForeignFunc::out) is semidet :-
            Body = Func ^ af_body,
            Body = ast_body_foreign(ForeignSym),
            ForeignFunc = foreign_func(Name, ForeignSym)
        ),
        Funcs, ForeignFuncs).

%-----------------------------------------------------------------------%

:- pred write_foreign_hooks(string::in, string::in, q_name::in,
    list(foreign_include)::in, list(foreign_func)::in, maybe_error::out,
    io::di, io::uo) is det.

write_foreign_hooks(FilenameCode, FilenameHeader, ModuleName,
        ForeignIncludes, ForeignFuncs, Result, !IO) :-
    write_temp(open_output, close_output,
        write_foreign_hooks_code(ModuleName, ForeignIncludes, ForeignFuncs),
        FilenameCode, ResultCode, !IO),
    write_temp(open_output, close_output,
        write_foreign_hooks_header(ModuleName),
        FilenameHeader, ResultHeader, !IO),
    move_temps_if_successful([ResultCode, ResultHeader], Result, !IO).

:- pred write_foreign_hooks_code(q_name::in, list(foreign_include)::in,
    list(foreign_func)::in, output_stream::in, maybe_error::out,
    io::di, io::uo) is det.

write_foreign_hooks_code(ModuleName, ForeignIncludes, ForeignFuncs, File,
        ok, !IO) :-
    format(File, "// Foreign hooks for %s\n\n",
        [s(q_name_to_string(ModuleName))], !IO),

    % XXX Fix include path.
    write_string(File, "#include \"../../../runtime/pz_common.h\"\n", !IO),
    write_string(File, "#include \"../../../runtime/pz_foreign.h\"\n", !IO),
    write_string(File, "#include \"../../../runtime/pz_generic_run.h\"\n\n", !IO),
    foldl(write_include(File), ForeignIncludes, !IO),
    nl(File, !IO),

    write_string(File, "using namespace pz;\n\n", !IO),

    format(File, "bool pz_init_foreign_code_%s(void *f_, void *gc_) {\n",
        [s(q_name_clobber(ModuleName))], !IO),
    write_string(File,
        "  GCTracer &gc = *reinterpret_cast<GCTracer*>(gc_);\n", !IO),
    write_string(File,
        "  Foreign *f = reinterpret_cast<Foreign*>(f_);\n", !IO),

    foldl(write_register_foreign_func(File, ModuleName), ForeignFuncs, !IO),
    write_string(File, "  return true;\n", !IO),
    write_string(File, "}\n", !IO).

:- pred write_foreign_hooks_header(q_name::in, output_stream::in,
    maybe_error::out, io::di, io::uo) is det.

write_foreign_hooks_header(ModuleName, File, ok, !IO) :-
    format(File, "// Foreign hooks for %s\n\n",
        [s(q_name_to_string(ModuleName))], !IO),

    format(File, "bool pz_init_foreign_code_%s(void *f, void *gc);\n",
        [s(q_name_clobber(ModuleName))], !IO),
    nl(File, !IO).

:- pred write_include(output_stream::in, foreign_include::in,
    io::di, io::uo) is det.

write_include(File, foreign_include(Path), !IO) :-
    io.format(File, "#include \"../%s\"\n", [s(Path)], !IO).

:- pred write_register_foreign_func(output_stream::in, q_name::in,
    foreign_func::in, io::di, io::uo) is det.

write_register_foreign_func(File, ModuleName,
        foreign_func(FuncName, ForeignSym), !IO) :-
    format(File, "  if (!f->register_foreign_code(String(\"%s\"), String(\"%s\"), %s, gc)) {\n",
        [s(q_name_to_string(ModuleName)), s(nq_name_to_string(FuncName)),
            s(ForeignSym)],
        !IO),
    write_string(File, "    return false;\n", !IO),
    write_string(File, "  }\n", !IO).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
