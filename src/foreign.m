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
:- import_module options.

%-----------------------------------------------------------------------%

:- pred do_make_foreign(general_options::in, ast::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
:- implementation.

:- import_module cord.
:- import_module list.
:- import_module maybe.
:- import_module string.

:- import_module compile.
:- import_module compile_error.
:- import_module q_name.
:- import_module util.
:- import_module util.mercury.
:- import_module util.result.

%-----------------------------------------------------------------------%

:- type foreign_func
    --->    foreign_func(
                ff_plasma_name      :: nq_name,
                ff_foreign_name     :: string
            ).

%-----------------------------------------------------------------------%

do_make_foreign(GeneralOpts, PlasmaAst, !IO) :-
    ForeignFuncs = find_foreign_funcs(PlasmaAst),
    WriteOutput = GeneralOpts ^ go_write_output,
    ( WriteOutput = write_output,
        OutputFile = GeneralOpts ^ go_output_file,
        write_foreign_hooks(OutputFile, PlasmaAst ^ a_module_name,
            ForeignFuncs, Result, !IO),
        ( Result = ok
        ; Result = error(ErrMsg),
            exit_error(ErrMsg, !IO)
        )
    ; WriteOutput = dont_write_output
    ).

%-----------------------------------------------------------------------%

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

:- pred write_foreign_hooks(string::in, q_name::in, list(foreign_func)::in,
    maybe_error::out, io::di, io::uo) is det.

write_foreign_hooks(Filename, ModuleName, ForeignFuncs, Result, !IO) :-
    open_output(Filename, OpenRes, !IO),
    ( OpenRes = ok(File),
        format(File, "// Foreign hooks for %s\n\n",
            [s(q_name_to_string(ModuleName))], !IO),
        % XXX Fix include path.
        write_string(File, "#include \"../../../runtime/pz_common.h\"\n", !IO),
        write_string(File, "#include \"../../../runtime/pz_foreign.h\"\n", !IO),
        write_string(File, "#include \"../../../runtime/pz_generic_run.h\"\n\n", !IO),
        write_string(File, "using namespace pz;\n\n", !IO),

        format(File, "bool register_funcs_%s(void *f_, void *gc_) {\n",
            [s(q_name_clobber(ModuleName))], !IO),
        write_string(File, "  GCTracer &gc = *reinterpret_cast<GCTracer*>(gc_);", !IO),
        write_string(File, "  Foreign *f = reinterpret_cast<Foreign*>(f_);", !IO),

        foldl(write_register_foreign_func(File, ModuleName), ForeignFuncs, !IO),
        write_string(File, "  return true;\n", !IO),
        write_string(File, "}\n", !IO),

        close_output(File, !IO),
        Result = ok
    ; OpenRes = error(Error),
        Result = error(format("%s: %s\n",
            [s(Filename), s(error_message(Error))]))
    ).

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
