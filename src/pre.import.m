%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module pre.import.
%
% Copyright (C) 2020 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
% Process imports by reading interface files.
%
%-----------------------------------------------------------------------%

:- interface.

:- import_module io.

:- import_module ast.
:- import_module compile_error.
:- import_module core.
:- import_module pre.env.
:- import_module util.
:- import_module util.result.

%-----------------------------------------------------------------------%

:- pred process_import(ast_import::in, env::in, env::out,
    core::in, core::out,
    errors(compile_error)::in, errors(compile_error)::out,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
:- implementation.

:- import_module cord.
:- import_module list.
:- import_module maybe.
:- import_module require.
:- import_module string.

:- import_module common_types.
:- import_module constant.
:- import_module core.function.
:- import_module parse.
:- import_module parse_util.
:- import_module pre.ast_to_core.
:- import_module q_name.
:- import_module util.exception.
:- import_module util.io.
:- import_module util.path.

%-----------------------------------------------------------------------%

process_import(ast_import(ImportName, _AsName), !Env, !Core, !Errors, !IO) :-
    ModuleName = import_name_to_module_name(ImportName),
    find_interface(ModuleName, Filename, !IO),
    parse_interface(Filename, MaybeAST, !IO),
    ( MaybeAST = ok(AST),
        ModuleNameStr = q_name_to_string(ModuleName),
        ( if AST ^ a_module_name = ModuleNameStr then
            foldl3(process_import_2(ModuleName), AST ^ a_entries, !Env, !Core,
                !Errors)
        else
            add_error(AST ^ a_context,
                ce_interface_contains_wrong_module(Filename, ModuleNameStr,
                    AST ^ a_module_name),
                !Errors)
        )
    ; MaybeAST = errors(Errors),
        add_errors(map(func(error(C, E)) = error(C, ce_read_source_error(E)),
            Errors), !Errors)
    ).

:- func import_name_to_module_name(import_name) = q_name.

import_name_to_module_name(dot(First, Rest)) = Name :-
    ( ( Rest = nil
      ; Rest = star
      ),
      Name = q_name_from_dotted_string(First)
    ; Rest = dot(_, _),
        util.exception.sorry($file, $pred, "Submodules")
    ).

    % Find the interface on the disk. For now we look in the current
    % directory only, later we'll implement include paths.
    %
:- pred find_interface(q_name::in, string::out, io::di, io::uo) is det.

find_interface(ModuleName, FileName, !IO) :-
    get_dir_list(MaybeDirList, !IO),
    ( MaybeDirList = ok(DirList),
        filter(matching_interface_file(ModuleName), DirList, Matches),
        ( Matches = [],
            compile_error($file, $pred, "No matching interfaces found")
        ; Matches = [FileName]
        ; Matches = [_, _ | _],
            compile_error($file, $pred, "Ambigious interfaces found")
        )
    ; MaybeDirList = error(Error),
        unexpected($file, $pred, "IO error while searching for modules: " ++
            Error)
    ).

:- pred matching_interface_file(q_name::in, string::in) is semidet.

matching_interface_file(ModuleName, FileName) :-
    filename_extension(interface_extension, FileName, FileNameBase),
    strip_file_name_punctuation(q_name_to_string(ModuleName)) =
        strip_file_name_punctuation(FileNameBase).

:- pred process_import_2(q_name::in, ast_interface_entry::in,
    env::in, env::out, core::in, core::out,
    errors(compile_error)::in, errors(compile_error)::out) is det.

process_import_2(ModuleName, asti_function(Decl), !Env, !Core,
        !Errors) :-
    core_allocate_function(FuncId, !Core),

    Name = q_name_append_str(ModuleName, Decl ^ afd_name),

    ( if env_add_func(Name, FuncId, !Env) then
        % Imported functions arn't re-exported, so we annotate it with
        % s_private.
        ast_to_func_decl(!.Core, !.Env, Name, Decl, s_private, Result),
        ( Result = ok(Function0),
            func_set_imported(Function0, Function),
            core_set_function(FuncId, Function, !Core)
        ; Result = errors(Errors),
            add_errors(Errors, !Errors)
        )
    else
        % XXX Needs to be context of import directive, we'll do a proper
        % error later.
        compile_error($file, $pred, "Name collision caused by import")
    ).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
