%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module pre.import.
%
% Copyright (C) 2020-2022 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
% Process imports by reading interface files.
%
%-----------------------------------------------------------------------%

:- interface.

:- import_module io.
:- import_module list.
:- import_module maybe.

:- import_module ast.
:- import_module compile_error.
:- import_module core.
:- import_module pre.env.
:- import_module q_name.
:- import_module util.
:- import_module util.log.
:- import_module util.result.

%-----------------------------------------------------------------------%

:- type import_type
    --->    interface_import
    ;       typeres_import.

%-----------------------------------------------------------------------%

:- type import_info
    --->    import_info(
                ii_module               :: q_name,
                ii_whitelisted          :: whitelisted,
                ii_source_file          :: maybe(string),
                ii_interface_file       :: string,
                ii_interface_exists     :: file_exists,
                ii_typeres_file         :: string,
                ii_typeres_exists       :: file_exists
            ).

:- type whitelisted
    --->    w_is_whitelisted
    ;       w_not_whitelisted
    ;       w_no_whitelist.

:- type file_exists
    --->    file_exists
    ;       file_does_not_exist.

%-----------------------------------------------------------------------%

    % ast_to_import_list(ThisModule, Directory, WhitelistFile,
    %   Imports, ImportInfo, !IO)
    %
    % Find the list of modules and their files we need to import.
    %
:- pred ast_to_import_list(q_name::in, string::in, maybe(string)::in,
    list(ast_import)::in, list(import_info)::out, io::di, io::uo) is det.

    % ast_to_core_imports(Verbose, ModuleName, ImportType, ImportEnv,
    %   MaybeWhitelistFile, Imports, !Env, !Core, !Errors, !IO).
    %
    % The ImportEnv is the Env that should be used to read interface files,
    % while !Env is a different environment to be updated with the results.
    %
:- pred ast_to_core_imports(log_config::in, q_name::in, import_type::in,
    env::in, maybe(string)::in, list(ast_import)::in,
    env::in, env::out, core::in, core::out,
    errors(compile_error)::in, errors(compile_error)::out, io::di, io::uo)
    is det.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
:- implementation.

:- import_module assoc_list.
:- import_module cord.
:- import_module map.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module unit.

:- import_module common_types.
:- import_module constant.
:- import_module context.
:- import_module core.function.
:- import_module core.resource.
:- import_module core.types.
:- import_module file_utils.
:- import_module parse.
:- import_module parse_util.
:- import_module pre.ast_to_core.
:- import_module util.my_exception.
:- import_module util.my_io.
:- import_module util.mercury.
:- import_module util.path.

%-----------------------------------------------------------------------%

ast_to_import_list(ThisModule, Dir, MaybeWhitelistFile, Imports, Result, !IO) :-
    ( MaybeWhitelistFile = yes(WhitelistFile),
        read_whitelist(ThisModule, WhitelistFile, MaybeWhitelist, !IO)
    ; MaybeWhitelistFile = no,
        MaybeWhitelist = no
    ),

    ModuleNames = sort_and_remove_dups(map(func(I) = I ^ ai_name, Imports)),
    map_foldl2(make_import_info(Dir, MaybeWhitelist), ModuleNames, Result,
        init, _, !IO).

:- pred make_import_info(string::in, maybe(import_whitelist)::in, q_name::in,
    import_info::out, dir_info::in, dir_info::out, io::di, io::uo) is det.

make_import_info(Path, MaybeWhitelist, Module, Result, !DirInfo, !IO) :-
    ( MaybeWhitelist = no,
        Whitelisted = w_no_whitelist
    ; MaybeWhitelist = yes(Whitelist),
        ( if member(Module, Whitelist) then
            Whitelisted = w_is_whitelisted
        else
            Whitelisted = w_not_whitelisted
        )
    ),

    find_module_file(Path, source_extension, Module, ResultSource,
        !DirInfo, !IO),

    ( ResultSource = yes(SourceFile),
        MbSourceFile = yes(SourceFile)
    ; ResultSource = no,
        MbSourceFile = no
    ; ResultSource = error(ErrPath, Error),
        compile_error($file, $pred,
            "IO error while searching for modules: " ++
            ErrPath ++ ": " ++ Error)
    ),

    find_module_file(Path, interface_extension, Module, ResultInterface,
        !DirInfo, !IO),
    CanonBaseName = canonical_base_name(Module),
    ( ResultInterface = yes(InterfaceFile),
        InterfaceExists = file_exists
    ; ResultInterface = no,
        InterfaceFile = CanonBaseName ++ interface_extension,
        InterfaceExists = file_does_not_exist
    ; ResultInterface = error(ErrPath, Error),
        compile_error($file, $pred,
            "IO error while searching for modules: " ++
            ErrPath ++ ": " ++ Error)
    ),

    find_module_file(Path, typeres_extension, Module, ResultTypeRes,
        !DirInfo, !IO),
    ( ResultTypeRes = yes(TyperesFile),
        TyperesExists = file_exists
    ; ResultTypeRes = no,
        TyperesFile = CanonBaseName ++ typeres_extension,
        TyperesExists = file_does_not_exist
    ; ResultTypeRes = error(ErrPath, Error),
        compile_error($file, $pred,
            "IO error while searching for modules: " ++
            ErrPath ++ ": " ++ Error)
    ),

    Result = import_info(Module, Whitelisted, MbSourceFile,
        InterfaceFile, InterfaceExists, TyperesFile, TyperesExists).

%-----------------------------------------------------------------------%

:- type import_whitelist == set(q_name).

:- pred read_whitelist(q_name::in, string::in, maybe(import_whitelist)::out,
    io::di, io::uo) is det.

read_whitelist(ThisModule, Filename, MaybeWhitelist, !IO) :-
    io.open_input(Filename, OpenRes, !IO),
    ( OpenRes = ok(File),
        read(File, WhitelistRes, !IO),
        ( WhitelistRes = ok(WhitelistList `with_type` list(list(q_name))),
            % The whitelist is stored as the list of lists of modules groups
            % from the build file, we need to find the relevant sets and
            % compute their intersection.
            ModulesSets = filter(
                pred(M::in) is semidet :- member(ThisModule, M),
                map(set.from_list, WhitelistList)),
            ( ModulesSets = [],
                % We can't compute the intersection of zero sets, so ignore
                % the whitelist.  This can happen if the module name in the
                % build file doesn't match the actual name.
                % TODO: we need to catch this error elsewhere.
                MaybeWhitelist = no
            ; ModulesSets = [_ | _],
                MaybeWhitelist = yes(delete(power_intersect_list(ModulesSets),
                    ThisModule))
            )
        ; WhitelistRes = eof,
            compile_error($file, $pred, format("%s: premature end of file",
                [s(Filename)]))
        ; WhitelistRes = error(Error, Line),
            compile_error($file, $pred, format("%s:%d: %s",
                [s(Filename), i(Line), s(Error)]))
        ),
        close_input(File, !IO)
    ; OpenRes = error(Error),
        compile_error($file, $pred, format("%s: %s",
            [s(Filename), s(error_message(Error))]))
    ).

%-----------------------------------------------------------------------%

ast_to_core_imports(Verbose, ThisModule, ImportType, !.ReadEnv,
        MbImportWhitelist, Imports, !Env, !Core, !Errors, !IO) :-
    ast_to_import_list(ThisModule, ".", MbImportWhitelist, Imports,
        ImportInfos, !IO),

    % Read the imports and convert it to AST.
    map_foldl(read_import(Verbose, !.Core, ImportType), ImportInfos,
        ImportAsts0, !IO),

    % Process the imports to add them to the core representation.
    ( ImportType = interface_import,
        % We update this environment with resources and types so that we can
        % process types and functions correctly.  Then throw away that
        % environment as different bindings will be made depending on the import
        % statement used.
        import_map_foldl2(gather_declarations, ImportAsts0, ImportAsts,
            !ReadEnv, !Core),

        % Process transitively imported things.  These things are declared
        % by .typeres files we didn't read, but must exist or we wouldn't
        % have been able to generate the .pi files we're now reading.  This
        % has to be done after regular declarations so those can be checked
        % more rigidly and it's simplier to do them before processing
        % definitions below.
        import_foldl2(gather_implicit_declarations, ImportAsts,
            !ReadEnv, !Core),

        import_map_foldl(process_interface_import(!.ReadEnv),
            ImportAsts, ImportItems, !Core)
    ; ImportType = typeres_import,
        import_map_foldl(process_typeres_import, ImportAsts0, ImportItems,
            !Core)
    ),

    ImportMap = map.from_assoc_list(ImportItems),

    % Enrol the imports in the environment.
    foldl5(enroll_import(Verbose, ImportMap), Imports, set.init, _,
        set.init, _, !Env, !Errors, !IO).

%-----------------------------------------------------------------------%

:- type import_map(T) == map(q_name, import_result(T)).
:- type import_list(T) == assoc_list(q_name, import_result(T)).

:- type import_result(T)
    --->    ok(T)
    ;       read_error(compile_error)
    ;       compile_errors(errors(compile_error)).

:- pred import_map_foldl(pred(q_name, X, import_result(Y), A, A),
    import_list(X), import_list(Y), A, A).
:- mode import_map_foldl(pred(in, in, out, in, out) is det,
    in, out, in, out) is det.

import_map_foldl(_, [], [], !A).
import_map_foldl(Pred, [N - XRes | Xs], [N - YRes | Ys], !A) :-
    ( XRes = ok(X),
        Pred(N, X, YRes, !A)
    ; XRes = read_error(E),
        YRes = read_error(E)
    ; XRes = compile_errors(Es),
        YRes = compile_errors(Es)
    ),
    import_map_foldl(Pred, Xs, Ys, !A).

:- pred import_map_foldl2(pred(q_name, X, import_result(Y), A, A, B, B),
    import_list(X), import_list(Y), A, A, B, B).
:- mode import_map_foldl2(pred(in, in, out, in, out, in, out) is det,
    in, out, in, out, in, out) is det.

import_map_foldl2(_, [], [], !A, !B).
import_map_foldl2(Pred, [N - XRes | Xs], [N - YRes | Ys], !A, !B) :-
    ( XRes = ok(X),
        Pred(N, X, YRes, !A, !B)
    ; XRes = read_error(E),
        YRes = read_error(E)
    ; XRes = compile_errors(Es),
        YRes = compile_errors(Es)
    ),
    import_map_foldl2(Pred, Xs, Ys, !A, !B).

    % Only processes ok(_) entries.
    %
:- pred import_foldl2(pred(q_name, X, A, A, B, B),
    import_list(X), A, A, B, B).
:- mode import_foldl2(pred(in, in, in, out, in, out) is det,
    in, in, out, in, out) is det.

import_foldl2(_, [], !A, !B).
import_foldl2(Pred, [N - XRes | Xs], !A, !B) :-
    ( XRes = ok(X),
        Pred(N, X, !A, !B)
    ; XRes = read_error(_)
    ; XRes = compile_errors(_)
    ),
    import_foldl2(Pred, Xs, !A, !B).

%-----------------------------------------------------------------------%

    % The AST in the ast.m file stores entries in the order they occur in
    % the file.  This AST stores them by type.  We should consider
    % re-writing ast.m to be like this then drop this type definition.  In
    % the future we may want something that reconstructs things in file
    % order but that's solveable, and not what we need today anyway.
    %
:- type import_ast(R, T)
    --->    import_ast(
                ia_module_name      :: q_name,
                ia_context          :: context,
                ia_entries          :: entry_types(R, T)
            ).

:- type entry_types(R, T)
    --->    et_typeres(
                ett_resources       :: list(q_name),
                ett_types           :: list({q_name, arity})
            )
    ;       et_interface(
                eti_resources       :: list({q_name, ast_resource, R}),
                eti_types           :: list({q_name, ast_type(q_name), T}),
                eti_functions       :: list(q_named(ast_function_decl))
            ).

    % Read an import and convert it to core representation, store references
    % to it in the import map.
    %
:- pred read_import(log_config::in, core::in, import_type::in, import_info::in,
    pair(q_name, import_result(import_ast(unit, unit)))::out,
    io::di, io::uo) is det.

read_import(Verbose, Core, ImportType, ImportInfo, ModuleName - Result,
        !IO) :-
    ModuleName = ImportInfo ^ ii_module,
    Whitelisted = ImportInfo ^ ii_whitelisted,
    ( Whitelisted = w_not_whitelisted,
        Result = read_error(ce_module_unavailable(ModuleName,
            module_name(Core)))
    ;
        ( Whitelisted = w_is_whitelisted
        ; Whitelisted = w_no_whitelist
        ),

        ( ImportType = interface_import,
            FileExists = ImportInfo ^ ii_interface_exists,
            Filename = ImportInfo ^ ii_interface_file
        ; ImportType = typeres_import,
            FileExists = ImportInfo ^ ii_typeres_exists,
            Filename = ImportInfo ^ ii_typeres_file
        ),
        ( FileExists = file_exists,
            verbose_output(Verbose,
                format("Reading %s from %s\n",
                    [s(q_name_to_string(ModuleName)), s(Filename)]),
                !IO),

            ( ImportType = interface_import,
                parse_interface(Filename, MaybeAST, !IO),
                ( MaybeAST = ok(AST),
                    foldl3(filter_entries, AST ^ a_entries, [], Resources0,
                        [], Types0, [], Funcs),
                    Resources = map(
                        func(q_named(Name, Res)) = {Name, Res, unit},
                        Resources0),
                    Types = map(
                        func(q_named(Name, Type)) = {Name, Type, unit},
                        Types0),
                    Result = ok(import_ast(AST ^ a_module_name,
                        AST ^ a_context,
                        et_interface(Resources, Types, Funcs)))
                ; MaybeAST = errors(Errors),
                    Result = compile_errors(
                        map(func(error(C, E)) =
                                error(C, ce_read_source_error(E)),
                            Errors))
                )
            ; ImportType = typeres_import,
                parse_typeres(Filename, MaybeAST, !IO),
                ( MaybeAST = ok(AST),
                    filter_map(
                        pred(asti_resource_abs(N)::in, N::out) is semidet,
                        AST ^ a_entries, Resources),
                    filter_map(
                        pred(asti_type_abs(N, A)::in, {N, A}::out) is semidet,
                        AST ^ a_entries, Types),
                    Result = ok(import_ast(AST ^ a_module_name,
                        AST ^ a_context, et_typeres(Resources, Types)))
                ; MaybeAST = errors(Errors),
                    Result = compile_errors(
                        map(func(error(C, E)) =
                                error(C, ce_read_source_error(E)),
                            Errors))
                )
            )
        ; FileExists = file_does_not_exist,
            Result = read_error(ce_module_not_found(ModuleName))
        )
    ).

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

:- pred process_typeres_import(q_name::in,
    import_ast(_, _)::in, import_result(import_entries)::out,
    core::in, core::out) is det.

process_typeres_import(ModuleName, ImportAST, Result, !Core) :-
    ImportAST = import_ast(ModuleNameAST, Context, Entries),
    ( if ModuleNameAST = ModuleName then
        ( Entries = et_interface(_, _, _),
            unexpected($file, $pred, "Interface")
        ; Entries = et_typeres(Resources, Types),
            map_foldl((pred(Name::in, NQName - ie_resource(Res)::out,
                        C0::in, C::out) is det :-
                    ( if q_name_append(ModuleName, NQName0, Name) then
                        NQName = NQName0
                    else
                        unexpected($file, $pred,
                            "Imported module exports symbols of other module")
                    ),
                    core_allocate_resource_id(Res, C0, C1),
                    core_set_resource(Res, r_abstract(Name), C1, C)
                ), Resources, NamePairsA, !Core),
            map_foldl((pred({Name, Arity}::in,
                        NQName - ie_type(Arity, Type)::out,
                        C0::in, C::out) is det :-
                    ( if q_name_append(ModuleName, NQName0, Name) then
                        NQName = NQName0
                    else
                        unexpected($file, $pred,
                            "Imported module exports symbols of other module")
                    ),
                    core_allocate_type_id(Type, C0, C1),
                    core_set_type(Type,
                        type_init_abstract(Name, Arity, nil_context), C1, C)
                ), Types, NamePairsB, !Core),
            Result = ok(NamePairsA ++ NamePairsB)
        )
    else
        Result = compile_errors(error(Context,
            ce_interface_contains_wrong_module(
                Context ^ c_file, ModuleName, ModuleNameAST)))
    ).

%-----------------------------------------------------------------------%

:- pred gather_declarations(q_name::in, import_ast(_, _)::in,
    import_result(import_ast(resource_id, type_id))::out, env::in, env::out,
    core::in, core::out) is det.

gather_declarations(_, ImportAST0, ok(ImportAST), !Env, !Core) :-
    Entries0 = ImportAST0 ^ ia_entries,
    ( Entries0 = et_typeres(_, _),
        unexpected($file, $pred, "Typeres")
    ; Entries0 = et_interface(Resources0, Types0, Funcs),
        map_foldl2(gather_resource, Resources0, Resources, !Env, !Core),
        map_foldl2(gather_types, Types0, Types, !Env, !Core),
        Entries = et_interface(Resources, Types, Funcs)
    ),
    ImportAST = ImportAST0 ^ ia_entries := Entries.

:- pred gather_implicit_declarations(q_name::in, import_ast(_, _)::in,
    env::in, env::out, core::in, core::out) is det.

gather_implicit_declarations(ImportModule, ImportAST, !Env, !Core) :-
    ThisModule = module_name(!.Core),
    Entries = ImportAST ^ ia_entries,
    ( Entries = et_typeres(_, _),
        unexpected($file, $pred, "Typeres")
    ; Entries = et_interface(Resources, Types, Funcs),
        % Gather resources and types that this module uses that my be
        % declared by transitively-imported modules.

        ResNames0 = union_list(map(resource_get_resources, Resources))
            `union` union_list(map(func_get_resources, Funcs)),
        ResNames = filter(module_name_filter(ThisModule, ImportModule),
            ResNames0),
        foldl2(maybe_add_implicit_resource, ResNames, !Env, !Core),

        TypeNames0 = union_list(map(type_get_types, Types))
            `union` union_list(map(func_get_types, Funcs)),
        TypeNames = filter((pred({N, _}::in) is semidet :-
                module_name_filter(ThisModule, ImportModule, N)
            ), TypeNames0),
        foldl2(maybe_add_implicit_type, TypeNames, !Env, !Core)
    ).


:- pred module_name_filter(q_name::in, q_name::in, q_name::in) is semidet.

module_name_filter(ThisModule, ImportModule, Name) :-
    q_name_parts(Name, MbModule, _),
    ( MbModule = no,
        unexpected($file, $pred, "No module part in name")
    ; MbModule = yes(Module)
    ),

    % Exclude resources in the module we're compiling
    \+ ThisModule = Module,

    % Exclude resources in the module being imported
    \+ ImportModule = Module.

:- pred maybe_add_implicit_resource(q_name::in, env::in, env::out,
    core::in, core::out) is det.

maybe_add_implicit_resource(Name, !Env, !Core) :-
    ( if env_search_resource(!.Env, Name, _) then
        true
    else
        core_allocate_resource_id(ResId, !Core),
        core_set_resource(ResId, r_abstract(Name), !Core),
        env_add_resource_det(Name, ResId, !Env)
    ).

:- pred maybe_add_implicit_type({q_name, arity}::in, env::in, env::out,
    core::in, core::out) is det.

maybe_add_implicit_type({Name, Arity}, !Env, !Core) :-
    ( if env_search_type(!.Env, Name, _) then
        true
    else
        core_allocate_type_id(TypeId, !Core),
        env_add_type_det(Name, Arity, TypeId, !Env),
        core_set_type(TypeId, type_init_abstract(Name, Arity, nil_context),
            !Core)
    ).

%-----------------------------------------------------------------------%

:- type import_entries == assoc_list(nq_name, import_entry).

:- type import_entry
    --->    ie_resource(resource_id)
    ;       ie_type(arity, type_id)
    ;       ie_ctor(ctor_id)
    ;       ie_func(func_id).

:- pred process_interface_import(env::in, q_name::in,
    import_ast(resource_id, type_id)::in, import_result(import_entries)::out,
    core::in, core::out) is det.

process_interface_import(Env, ModuleName, ImportAST, Result, !Core) :-
    ImportAST = import_ast(ModuleNameAST, Context, Entries),
    ( if ModuleNameAST = ModuleName then
        ( Entries = et_interface(Resources, Types, Funcs),
            read_import_import(ModuleName, Env, Resources, Types, Funcs,
                NamePairs, Errors, !Core),
            ( if is_empty(Errors) then
                Result = ok(NamePairs)
            else
                Result = compile_errors(Errors)
            )
        ; Entries = et_typeres(_, _),
            unexpected($file, $pred, "Typeres")
        )
    else
        Result = compile_errors(error(Context,
            ce_interface_contains_wrong_module(
                Context ^ c_file, ModuleName, ModuleNameAST)))
    ).

:- pred read_import_import(q_name::in, env::in,
    list({q_name, ast_resource, resource_id})::in,
    list({q_name, ast_type(q_name), type_id})::in,
    list(q_named(ast_function_decl))::in,
    assoc_list(nq_name, import_entry)::out, errors(compile_error)::out,
    core::in, core::out) is det.

read_import_import(ModuleName, Env, Resources, Types, Funcs, NamePairs,
        Errors, !Core) :-
    map2_foldl(do_import_resource(ModuleName, Env), Resources,
        ResourcePairs, ResourceErrors, !Core),

    map2_foldl(do_import_type(ModuleName, Env), Types, TypePairs,
        TypeErrors, !Core),

    map2_foldl(do_import_function(ModuleName, Env), Funcs, FuncPairs,
        FunctionErrors, !Core),

    NamePairs = ResourcePairs ++ condense(TypePairs) ++ FuncPairs,
    Errors = cord_list_to_cord(ResourceErrors ++ TypeErrors ++
        FunctionErrors).

%-----------------------------------------------------------------------%

:- pred gather_resource({q_name, ast_resource, _}::in,
    {q_name, ast_resource, resource_id}::out,
    env::in, env::out, core::in, core::out) is det.

gather_resource({Name, Res, _}, {Name, Res, ResId}, !Env, !Core) :-
    core_allocate_resource_id(ResId, !Core),
    ( if env_add_resource(Name, ResId, !Env) then
        true
    else
        compile_error($file, $pred, "Resource already defined")
    ).

:- func resource_get_resources({_, ast_resource, _}) = set(q_name).

resource_get_resources({_, ast_resource(Name, _, _), _}) =
    make_singleton_set(Name).

:- pred do_import_resource(q_name::in, env::in,
    {q_name, ast_resource, resource_id}::in,
    pair(nq_name, import_entry)::out, errors(compile_error)::out,
    core::in, core::out) is det.

do_import_resource(ModuleName, Env, {Name, Res0, ResId}, NamePair,
        !:Errors, !Core) :-
    !:Errors = init,
    ( if q_name_append(ModuleName, NQName0, Name) then
        NQName = NQName0
    else
        unexpected($file, $pred,
            "Imported module exports symbols of other module")
    ),

    Res0 = ast_resource(FromName, _, Context),

    NamePair = NQName - ie_resource(ResId),

    ( if env_search_resource(Env, FromName, FromRes) then
        core_set_resource(ResId,
            r_other(Name, FromRes, s_private, i_imported, Context), !Core)
    else
        add_error(Context, ce_resource_unknown(FromName), !Errors)
    ).

%-----------------------------------------------------------------------%

:- pred gather_types({q_name, ast_type(q_name), _}::in,
    {q_name, ast_type(q_name), type_id}::out, env::in, env::out,
    core::in, core::out) is det.

gather_types({Name, Type, _}, {Name, Type, TypeId}, !Env, !Core) :-
    core_allocate_type_id(TypeId, !Core),
    Arity = type_arity(Type),
    env_add_type_det(Name, Arity, TypeId, !Env).

:- func type_get_types({_, ast_type(_), _}) = set({q_name, arity}).

type_get_types({_, Type, _}) = Types :-
    ( Type = ast_type(_, Ctors, _, _),
        Types = union_list(map(ctor_get_types, Ctors))
    ; Type = ast_type_abstract(_, _),
        Types = init
    ).

:- func ctor_get_types(at_constructor(_)) = set({q_name, arity}).

ctor_get_types(Ctor) = union_list(map(field_get_types, Ctor ^ atc_args)).

:- func field_get_types(at_field) = set({q_name, arity}).

field_get_types(at_field(_, TypeExpr, _)) = type_expr_get_types(TypeExpr).

:- func type_expr_get_types(ast_type_expr) = set({q_name, arity}).

type_expr_get_types(ast_type(Name, Args, _)) =
    make_singleton_set({Name, arity(length(Args))}) `union`
    union_list(map(type_expr_get_types, Args)).
type_expr_get_types(ast_type_func(Args, Returns, _, _)) =
    union_list(map(type_expr_get_types, Args)) `union`
        union_list(map(type_expr_get_types, Returns)).
type_expr_get_types(ast_type_var(_, _)) = init.

:- pred do_import_type(q_name::in, env::in,
    {q_name, ast_type(q_name), type_id}::in,
    assoc_list(nq_name, import_entry)::out, errors(compile_error)::out,
    core::in, core::out) is det.

do_import_type(ModuleName, Env, {Name, ASTType, TypeId}, NamePairs, Errors,
        !Core) :-
    ( if q_name_append(ModuleName, NQName0, Name) then
        NQName = NQName0
    else
        unexpected($file, $pred,
            "Imported module exports symbols of other module")
    ),
    NamePair = NQName - ie_type(type_arity(ASTType), TypeId),

    ast_to_core_type_i(func(N) = N, i_imported, Env, Name, TypeId, ASTType, 
        Result, !Core),
    ( Result = ok({Type, Ctors}),
        core_set_type(TypeId, Type, !Core),
        CtorNamePairs = map(
            func(C) = q_name_unqual(C ^ cb_name) - ie_ctor(C ^ cb_id),
            Ctors),
        NamePairs = [NamePair | CtorNamePairs],
        Errors = init
    ; Result = errors(Errors),
        NamePairs = []
    ).

%-----------------------------------------------------------------------%

:- func func_get_resources(q_named(ast_function_decl)) = set(q_name).

func_get_resources(q_named(_, Func)) =
    list_to_set(map(func(U) = U ^ au_name, Func ^ afd_uses)).

:- func func_get_types(q_named(ast_function_decl)) = set({q_name, arity}).

func_get_types(q_named(_, Func)) =
    union_list(map(func(ast_param(_, T)) = type_expr_get_types(T),
        Func ^ afd_params)) `union`
    union_list(map(type_expr_get_types, Func ^ afd_return)).

%-----------------------------------------------------------------------%

:- pred do_import_function(q_name::in, env::in, q_named(ast_function_decl)::in,
    pair(nq_name, import_entry)::out, errors(compile_error)::out,
    core::in, core::out) is det.

do_import_function(ModuleName, Env, q_named(Name, Decl), NamePair,
        Errors, !Core) :-
    core_allocate_function(FuncId, !Core),

    ( if q_name_append(ModuleName, NQName0, Name) then
        NQName = NQName0
    else
        unexpected($file, $pred,
            "Imported module exports symbols of other module")
    ),
    NamePair = NQName - ie_func(FuncId),

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
:- pred enroll_import(log_config::in, import_map(import_entries)::in,
    ast_import::in, set(q_name)::in, set(q_name)::out,
    set(q_name)::in, set(q_name)::out, env::in, env::out,
    errors(compile_error)::in, errors(compile_error)::out,
    io::di, io::uo) is det.

enroll_import(Verbose, ImportMap, ast_import(ModuleName, MaybeAsName, Context),
        !AsSet, !DupImportsSet, !Env, !Errors, !IO) :-
    ( MaybeAsName = no,
        AsName = ModuleName
    ; MaybeAsName = yes(AsNameStr),
        AsName = q_name_from_dotted_string_det(AsNameStr)
    ),
    verbose_output(Verbose,
        format("Importing %s as %s\n",
            [s(q_name_to_string(ModuleName)), s(q_name_to_string(AsName))]),
            !IO),

    ( if insert_new(AsName, !AsSet) then
        true
    else
        add_error(Context, ce_import_would_clobber(ModuleName,
                map_maybe(q_name_from_dotted_string_det, MaybeAsName)),
            !Errors)
    ),
    ( if insert_new(ModuleName, !DupImportsSet) then
        true
    else
        add_error(Context, ce_import_duplicate(ModuleName), !Errors)
    ),

    map.lookup(ImportMap, ModuleName, ReadResult),
    ( ReadResult = ok(NamePairs),
        foldl(import_add_to_env(AsName), NamePairs, !Env)
    ; ReadResult = read_error(Error),
        add_error(Context, Error, !Errors)
    ; ReadResult = compile_errors(Errors),
        add_errors(Errors, !Errors)
    ).

:- pred import_add_to_env(q_name::in, pair(nq_name, import_entry)::in,
    env::in, env::out) is det.

import_add_to_env(IntoName, Name0 - Entry, !Env) :-
    Name = q_name_append(IntoName, Name0),
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
%-----------------------------------------------------------------------%
