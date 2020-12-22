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

:- import_module bool.
:- import_module io.
:- import_module list.

:- import_module ast.
:- import_module compile_error.
:- import_module core.
:- import_module pre.env.
:- import_module q_name.
:- import_module util.
:- import_module util.log.
:- import_module util.result.

%-----------------------------------------------------------------------%

:- type import_info
    --->    import_info(
                ii_module   :: q_name,
                ii_files    :: import_files
            ).

:- type import_files
    --->    if_not_found
    ;       if_found(
                iff_interface_file      :: string,
                iff_interface_prsent    :: bool,
                iff_source_file         :: string
            ).

    % Find the list of modules and their files we need to import.
    %
:- pred ast_to_import_list(string::in, list(ast_import)::in,
    list(import_info)::out, io::di, io::uo) is det.

    % ast_to_core_imports(Verbose, ImportEnv, Imports, !Env, !Core, !Errors,
    %   !IO).
    %
    % The ImportEnv is the Env that should be used to read interface files,
    % while !Env is a different environment to be updated with the results.
    %
:- pred ast_to_core_imports(log_config::in, env::in, list(ast_import)::in,
    env::in, env::out, core::in, core::out,
    errors(compile_error)::in, errors(compile_error)::out, io::di, io::uo)
    is det.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
:- implementation.

:- import_module assoc_list.
:- import_module cord.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module string.

:- import_module common_types.
:- import_module constant.
:- import_module context.
:- import_module core.function.
:- import_module core.resource.
:- import_module file_utils.
:- import_module parse.
:- import_module parse_util.
:- import_module pre.ast_to_core.
:- import_module util.exception.
:- import_module util.io.
:- import_module util.mercury.
:- import_module util.path.

%-----------------------------------------------------------------------%

ast_to_import_list(Dir, Imports, Result, !IO) :-
    get_dir_list(Dir, MaybeDirList, !IO),
    ( MaybeDirList = ok(DirList),
        ModuleNames = sort_and_remove_dups(map(imported_module, Imports)),
        Result = map(make_import_info(DirList), ModuleNames)
    ; MaybeDirList = error(Error),
        compile_error($file, $pred,
            "IO error while searching for modules: " ++ Error)
    ).

:- func make_import_info(list(string), q_name) = import_info.

make_import_info(DirList, Module) = Result :-
    ResultSource = find_module_file(DirList, source_extension, Module),
    ResultInterface = find_module_file(DirList, interface_extension, Module),
    (
        ResultSource = yes(SourceFile),
        ResultInterface = yes(InterfaceFile),

        ( if
            file_change_extension(source_extension,
                interface_extension, SourceFile, InterfaceFile)
        then
            InterfaceExists = yes,
            Result = import_info(Module,
                if_found(InterfaceFile, InterfaceExists, SourceFile))
        else
            unexpected($file, $pred,
                "Source and interface file names don't match")
        )
    ;
        ResultSource = yes(SourceFile),
        ResultInterface = no,

        file_change_extension(source_extension,
            interface_extension, SourceFile, InterfaceFile),
        InterfaceExists = no,
        Result = import_info(Module,
            if_found(InterfaceFile, InterfaceExists, SourceFile))
    ;
        ResultSource = no,
        ResultInterface = yes(InterfaceFile),

        file_change_extension(interface_extension, source_extension,
            InterfaceFile, SourceFile),
        InterfaceExists = yes,
        Result = import_info(Module,
            if_found(InterfaceFile, InterfaceExists, SourceFile))
    ;
        ResultSource = no,
        ResultInterface = no,

        Result = import_info(Module, if_not_found)
    ).

%-----------------------------------------------------------------------%

ast_to_core_imports(Verbose, ReadEnv, Imports, !Env, !Core, !Errors, !IO) :-
    get_dir_list(".", MaybeDirList, !IO),
    ( MaybeDirList = ok(DirList),
        % Read the imports and convert it to core representation.
        ModuleNames = sort_and_remove_dups(map(imported_module, Imports)),
        foldl3(read_import(Verbose, DirList, ReadEnv), ModuleNames, init,
            ImportMap, !Core, !IO),

        % Enrol the imports in the environment.
        foldl4(process_import(Verbose, ImportMap), Imports, init, _, !Env,
            !Errors, !IO)
    ; MaybeDirList = error(Error),
        compile_error($file, $pred,
            "IO error while searching for modules: " ++ Error)
    ).

:- func imported_module(ast_import) = q_name.

imported_module(Import) = import_name_to_module_name(Import ^ ai_names).

%-----------------------------------------------------------------------%

:- type import_map == map(q_name, import_result).

:- type import_result
    --->    ok(assoc_list(q_name, import_entry))
    ;       read_error(compile_error)
    ;       compile_errors(errors(compile_error)).

:- type import_entry
    --->    ie_resource(resource_id)
    ;       ie_type(arity, type_id)
    ;       ie_ctor(ctor_id)
    ;       ie_func(func_id).

    % Read an import and convert it to core representation, store references
    % to it in the import map.
    %
:- pred read_import(log_config::in, list(string)::in, env::in, q_name::in,
    import_map::in, import_map::out, core::in, core::out,
    io::di, io::uo) is det.

read_import(Verbose, DirList, Env, ModuleName, !ImportMap, !Core, !IO) :-
    MaybeFilename = find_module_file(DirList, interface_extension, ModuleName),
    ( MaybeFilename = yes(Filename),
        verbose_output(Verbose,
            format("Reading %s from %s\n",
                [s(q_name_to_string(ModuleName)), s(Filename)]),
            !IO),
        parse_interface(Filename, MaybeAST, !IO),
        ( MaybeAST = ok(AST),
            ( if AST ^ a_module_name = ModuleName then
                read_import_2(ModuleName, Env, AST ^ a_entries, NamePairs,
                    Errors, !Core),
                ( if is_empty(Errors) then
                    Result = ok(NamePairs)
                else
                    Result = compile_errors(Errors)
                )
            else
                Result = compile_errors(error(AST ^ a_context,
                    ce_interface_contains_wrong_module(
                        Filename, ModuleName, AST ^ a_module_name)))
            )
        ; MaybeAST = errors(Errors),
            Result = compile_errors(
                map(func(error(C, E)) = error(C, ce_read_source_error(E)),
                    Errors))
        )
    ; MaybeFilename = no,
        Result = read_error(ce_module_not_found(ModuleName))
    ),
    det_insert(ModuleName, Result, !ImportMap).

:- pred read_import_2(q_name::in, env::in, list(ast_interface_entry)::in,
    assoc_list(q_name, import_entry)::out, errors(compile_error)::out,
    core::in, core::out) is det.

read_import_2(ModuleName, !.Env, Entries, NamePairs, Errors, !Core) :-
    foldl3(filter_entries, Entries, [], Resources, [], Types, [], Funcs),

    % We update this environment with resources and types so that we can
    % process types and functions correctly.  Then throw away that
    % environment as different bindings will be made depending on the import
    % statement used.

    foldl2(gather_resource, Resources, !Env, !Core),
    map2_foldl(do_import_resource(ModuleName, !.Env), Resources,
        ResourcePairs, ResourceErrors, !Core),

    foldl2(gather_types, Types, !Env, !Core),
    map2_foldl(do_import_type(ModuleName, !.Env), Types, TypePairs,
        TypeErrors, !Core),

    map2_foldl(do_import_function(ModuleName, !.Env), Funcs, FuncPairs,
        FunctionErrors, !Core),

    NamePairs = ResourcePairs ++ condense(TypePairs) ++ FuncPairs,
    Errors = cord_list_to_cord(ResourceErrors ++ TypeErrors ++ FunctionErrors).

:- pred filter_entries(ast_interface_entry::in,
    list(q_named(ast_resource))::in,
    list(q_named(ast_resource))::out,
    list(q_named(ast_type(q_name)))::in,
    list(q_named(ast_type(q_name)))::out,
    list(q_named(ast_function_decl))::in,
    list(q_named(ast_function_decl))::out) is det.

filter_entries(asti_resource(N, R), !Resources, !Types, !Funcs) :-
    !:Resources = [q_named(N, R) | !.Resources].
filter_entries(asti_type(N, T), !Resources, !Types, !Funcs) :-
    !:Types = [q_named(N, T) | !.Types].
filter_entries(asti_function(N, F), !Resources, !Types, !Funcs) :-
    !:Funcs = [q_named(N, F) | !.Funcs].

%-----------------------------------------------------------------------%

:- pred gather_resource(q_named(ast_resource)::in, env::in, env::out,
    core::in, core::out) is det.

gather_resource(q_named(Name, _), !Env, !Core) :-
    core_allocate_resource_id(Res, !Core),
    ( if env_add_resource(Name, Res, !Env) then
        true
    else
        compile_error($file, $pred, "Resource already defined")
    ).

:- pred do_import_resource(q_name::in, env::in, q_named(ast_resource)::in,
    pair(q_name, import_entry)::out, errors(compile_error)::out,
    core::in, core::out) is det.

do_import_resource(ModuleName, Env, q_named(Name, Res0), NamePair,
        !:Errors, !Core) :-
    !:Errors = init,
    ( if q_name_append(ModuleName, _, Name) then
        true
    else
        unexpected($file, $pred,
            "Imported module exports symbols of other module")
    ),

    Res0 = ast_resource(FromName, _, Context),

    env_lookup_resource(Env, Name, Res),
    NamePair = Name - ie_resource(Res),

    ( if env_search_resource(Env, FromName, FromRes) then
        core_set_resource(Res, r_other(Name, FromRes, s_private, Context),
            !Core)
    else
        add_error(Context, ce_resource_unknown(FromName), !Errors)
    ).

%-----------------------------------------------------------------------%

:- pred gather_types(q_named(ast_type(q_name))::in, env::in, env::out,
    core::in, core::out) is det.

gather_types(q_named(Name, Type), !Env, !Core) :-
    core_allocate_type_id(TypeId, !Core),
    Arity = type_arity(Type),
    env_add_type_det(Name, Arity, TypeId, !Env).

:- pred do_import_type(q_name::in, env::in, q_named(ast_type(q_name))::in,
    assoc_list(q_name, import_entry)::out, errors(compile_error)::out,
    core::in, core::out) is det.

do_import_type(ModuleName, Env, q_named(Name, ASTType), NamePairs, Errors,
        !Core) :-
    ( if q_name_append(ModuleName, _, Name) then
        true
    else
        unexpected($file, $pred,
            "Imported module exports symbols of other module")
    ),
    env_lookup_type(Env, Name, TypeEntry),
    ( TypeEntry = te_id(TypeId, Arity)
    ; TypeEntry = te_builtin(_),
        unexpected($file, $pred, "Builtin type")
    ),
    NamePair = Name - ie_type(Arity, TypeId),

    ast_to_core_type_i(func(N) = N, Env, Name, TypeId, ASTType, Result, !Core),
    ( Result = ok({Type, Ctors}),
        core_set_type(TypeId, Type, !Core),
        CtorNamePairs = map(func(C) = C ^ cb_name - ie_ctor(C ^ cb_id),
            Ctors),
        NamePairs = [NamePair | CtorNamePairs],
        Errors = init
    ; Result = errors(Errors),
        NamePairs = []
    ).

%-----------------------------------------------------------------------%

:- pred do_import_function(q_name::in, env::in, q_named(ast_function_decl)::in,
    pair(q_name, import_entry)::out, errors(compile_error)::out,
    core::in, core::out) is det.

do_import_function(ModuleName, Env, q_named(Name, Decl), NamePair,
        Errors, !Core) :-
    core_allocate_function(FuncId, !Core),

    ( if q_name_append(ModuleName, _, Name) then
        true
    else
        unexpected($file, $pred,
            "Imported module exports symbols of other module")
    ),
    NamePair = Name - ie_func(FuncId),

    % Imported functions aren't re-exported, so we annotate it with
    % s_private.
    ast_to_func_decl(!.Core, Env, Name, Decl, s_private, Result),
    ( Result = ok(Function0),
        func_set_imported(Function0, Function),
        core_set_function(FuncId, Function, !Core),
        Errors = init
    ; Result = errors(Errors)
    ).

%-----------------------------------------------------------------------%

    % Enrol an import in the import_map into the environment.
    %
    % IO is used only for logging.
    %
:- pred process_import(log_config::in, import_map::in, ast_import::in,
    set(q_name)::in, set(q_name)::out, env::in, env::out,
    errors(compile_error)::in, errors(compile_error)::out,
    io::di, io::uo) is det.

process_import(Verbose, ImportMap, ast_import(ImportName, _AsName, Context),
        !ReadSet, !Env, !Errors, !IO) :-
    ModuleName = import_name_to_module_name(ImportName),

    verbose_output(Verbose,
        format("Importing %s for %s\n",
            [s(q_name_to_string(ModuleName)), s(string(ImportName))]), !IO),

    ( if insert_new(ModuleName, !ReadSet) then
        true
    else
        add_error(Context, ce_import_would_clobber(ModuleName), !Errors)
    ),

    map.lookup(ImportMap, ModuleName, ReadResult),
    ( ReadResult = ok(NamePairs),
        foldl(import_add_to_env, NamePairs, !Env)
    ; ReadResult = read_error(Error),
        add_error(Context, Error, !Errors)
    ; ReadResult = compile_errors(Errors),
        add_errors(Errors, !Errors)
    ).

:- pred import_add_to_env(pair(q_name, import_entry)::in,
    env::in, env::out) is det.

import_add_to_env(Name - Entry, !Env) :-
    ( if
        require_complete_switch [Entry]
        ( Entry = ie_resource(ResId),
            env_add_resource(Name, ResId, !Env)
        ; Entry = ie_type(Arity, TypeId),
            env_add_type(Name, Arity, TypeId, !Env)
        ; Entry = ie_ctor(CtorId),
            env_add_constructor(Name, CtorId, !Env)
        ; Entry = ie_func(FuncId),
            env_add_func(Name, FuncId, !Env)
        )
    then
        true
    else
        % XXX Needs to be context of import directive, we'll do a proper
        % error later.
        compile_error($file, $pred, "Name collision caused by import")
    ).

%-----------------------------------------------------------------------%

:- func import_name_to_module_name(import_name) = q_name.

import_name_to_module_name(dot(First, Rest)) = Name :-
    ( ( Rest = nil
      ; Rest = star
      ),
      % The parser has checked this and we can use the _det version.
      Name = q_name_from_dotted_string_det(First)
    ; Rest = dot(_, _),
        util.exception.sorry($file, $pred, "Submodules")
    ).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
