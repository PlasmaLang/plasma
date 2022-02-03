%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module core.
%
% Copyright (C) 2015-2022 Plasma Team
% Distributed under the terms of the MIT see ../LICENSE.code
%
% Plasma core representation
%
%-----------------------------------------------------------------------%

:- interface.

:- include_module core.code.
:- include_module core.function.
:- include_module core.pretty.
:- include_module core.resource.
:- include_module core.types.

:- include_module core.arity_chk.
:- include_module core.branch_chk.
:- include_module core.res_chk.
:- include_module core.simplify.
:- include_module core.type_chk.

:- include_module core.util.

%-----------------------------------------------------------------------%

:- import_module assoc_list.
:- import_module list.
:- import_module set.

:- import_module common_types.
:- import_module core.function.
:- import_module core.resource.
:- import_module core.types.
:- import_module q_name.

%-----------------------------------------------------------------------%

:- type core.

:- func init(q_name) = core.

:- func module_name(core) = q_name.

:- pred core_allocate_function(func_id::out, core::in, core::out) is det.

:- func core_all_functions(core) = assoc_list(func_id, function).

:- func core_all_functions_set(core) = set(func_id).

    % All functions with bodies.
    %
:- func core_all_defined_functions(core) = assoc_list(func_id, function).

:- func core_all_defined_functions_set(core) = set(func_id).

:- func core_all_exported_functions(core) = assoc_list(func_id, function).

:- pred core_set_function(func_id::in, function::in, core::in, core::out)
    is det.

:- pred core_get_function_det(core::in, func_id::in, function::out) is det.

:- type core_entrypoint
            % The entrypoint is func() -> Int
    --->    entry_plain(func_id)

            % The entrypoint is func(argv : List(String)) -> Int
    ;       entry_argv(func_id).

:- func core_entry_candidates(core) = set(core_entrypoint).

:- pred core_add_entry_function(core_entrypoint::in, core::in, core::out)
    is det.

:- func core_lookup_function_name(core, func_id) = q_name.

%-----------------------------------------------------------------------%

    % Return all the defined functions, topologically sorted into their
    % SCCs.
    %
:- func core_all_defined_functions_sccs(core) = list(set(func_id)).

%-----------------------------------------------------------------------%

:- pred core_allocate_type_id(type_id::out, core::in, core::out) is det.

:- func core_all_types(core) = assoc_list(type_id, user_type).

:- func core_all_exported_types(core) = assoc_list(type_id, user_type).

:- func core_get_type(core, type_id) = user_type.

:- pred core_set_type(type_id::in, user_type::in, core::in, core::out)
    is det.

:- func core_lookup_type_name(core, type_id) = q_name.

:- pred core_allocate_ctor_id(ctor_id::out, core::in, core::out) is det.

:- func core_lookup_constructor_name(core, ctor_id) = q_name.

:- pred core_get_constructor_type(core::in, ctor_id::in, type_id::out) is det.

:- pred core_get_constructor_det(core::in, ctor_id::in,
    constructor::out) is det.

:- pred core_set_constructor(ctor_id::in, q_name::in, type_id::in,
    constructor::in, core::in, core::out) is det.

%-----------------------------------------------------------------------%

:- pred core_allocate_resource_id(resource_id::out, core::in, core::out)
    is det.

:- pred core_set_resource(resource_id::in, resource::in,
    core::in, core::out) is det.

:- func core_get_resource(core, resource_id) = resource.

:- func core_all_exported_resources(core) = assoc_list(resource_id, resource).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

:- import_module digraph.
:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module varmap.

:- import_module core.code.
:- import_module util.
:- import_module util.my_exception.

%-----------------------------------------------------------------------%

:- type core
    --->    core(
                c_module_name       :: q_name,

                c_funcs             :: map(func_id, function),
                c_next_func_id      :: func_id,
                c_entry_candidates  :: set(core_entrypoint),

                c_next_type_id      :: type_id,
                c_types             :: map(type_id, user_type),

                c_next_ctor_id      :: ctor_id,
                c_constructors      :: map(ctor_id, ctor_info),

                c_next_res_id       :: resource_id,
                c_resources         :: map(resource_id, resource)
            ).

:- type ctor_info
    --->    ctor_info(
                ci_name         :: q_name,
                ci_arity        :: int,
                ci_type_id      :: type_id,
                ci_constructor  :: constructor
            ).

%-----------------------------------------------------------------------%

init(ModuleName) =
    core(ModuleName,
        % Functions
        init, func_id(0), init,
        % Types
        type_id(0), init,
        % Constructors
        ctor_id(0), init,
        % Resources
        resource_id(0), init
    ).

module_name(Core) = Core ^ c_module_name.

%-----------------------------------------------------------------------%

core_allocate_function(FuncId, !Core) :-
    FuncId = !.Core ^ c_next_func_id,
    FuncId = func_id(N),
    !Core ^ c_next_func_id := func_id(N+1).

core_all_functions(Core) = to_assoc_list(Core ^ c_funcs).

core_all_functions_set(Core) = keys_as_set(Core ^ c_funcs).

core_all_defined_functions(Core) =
    filter(is_defined, core_all_functions(Core)).

core_all_defined_functions_set(Core) =
    list_to_set(map(fst, core_all_defined_functions(Core))).

:- pred is_defined(pair(_, function)::in) is semidet.

is_defined(_ - Func) :-
    func_get_body(Func, _, _, _, _).

core_all_exported_functions(Core) =
    filter(is_exported, core_all_functions(Core)).

:- pred is_exported(pair(_, function)::in) is semidet.

is_exported(_ - Func) :-
    func_get_sharing(Func) = s_public.

core_set_function(FuncId, Func, !Core) :-
    map.set(FuncId, Func, !.Core ^ c_funcs, Funcs),
    !Core ^ c_funcs := Funcs.

core_get_function_det(Core, FuncId, Func) :-
    map.lookup(Core ^ c_funcs, FuncId, Func).

core_entry_candidates(Core) = Core ^ c_entry_candidates.

core_add_entry_function(Entrypoint, !Core) :-
    !Core ^ c_entry_candidates :=
        insert(!.Core ^ c_entry_candidates, Entrypoint).

core_lookup_function_name(Core, FuncId) = func_get_name(Func) :-
    core_get_function_det(Core, FuncId, Func).

%-----------------------------------------------------------------------%

core_all_defined_functions_sccs(Core) = SCCs :-
    AllFuncs = map(fst, core_all_defined_functions(Core)),
    AllFuncsSet = list_to_set(AllFuncs),
    some [!Graph] (
        !:Graph = digraph.init,
        map_foldl(add_vertex, AllFuncs, _, !Graph),
        foldl(core_build_graph(Core, AllFuncsSet), AllFuncs, !Graph),
        SCCs = atsort(!.Graph)
    ).

:- pred core_build_graph(core::in, set(func_id)::in, func_id::in,
    digraph(func_id)::in, digraph(func_id)::out) is det.

core_build_graph(Core, AllFuncs, FuncId, !Graph) :-
    core_get_function_det(Core, FuncId, Func),
    Callees = func_get_callees(Func),
    FuncIdKey = lookup_key(!.Graph, FuncId),
    foldl(core_add_edge(AllFuncs, FuncIdKey), Callees, !Graph).

:- pred core_add_edge(set(func_id)::in, digraph_key(func_id)::in, func_id::in,
    digraph(func_id)::in, digraph(func_id)::out) is det.

core_add_edge(AllFuncs, CallerKey, Callee, !Graph) :-
    ( if set.member(Callee, AllFuncs) then
        CalleeKey = lookup_key(!.Graph, Callee),
        add_edge(CallerKey, CalleeKey, !Graph)
    else
        true
    ).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

core_allocate_type_id(TypeId, !Core) :-
    TypeId = !.Core ^ c_next_type_id,
    TypeId = type_id(N),
    !Core ^ c_next_type_id := type_id(N+1).

core_all_types(Core) = to_assoc_list(Core ^ c_types).

core_all_exported_types(Core) =
    filter(type_is_exported, core_all_types(Core)).

:- pred type_is_exported(pair(_, user_type)::in) is semidet.

type_is_exported(_ - Type) :-
    Sharing = utype_get_sharing(Type),
    ( Sharing = st_public
    ; Sharing = st_public_abstract
    ).

core_get_type(Core, TypeId) = Type :-
    lookup(Core ^ c_types, TypeId, Type).

core_set_type(TypeId, Type, !Core) :-
    set(TypeId, Type, !.Core ^ c_types, Map),
    !Core ^ c_types := Map.

core_lookup_type_name(Core, TypeId) =
    utype_get_name(core_get_type(Core, TypeId)).

%-----------------------------------------------------------------------%

core_allocate_ctor_id(CtorId, !Core) :-
    CtorId = !.Core ^ c_next_ctor_id,
    CtorId = ctor_id(N),
    !Core ^ c_next_ctor_id := ctor_id(N+1).

core_lookup_constructor_name(Core, CtorId) = Info ^ ci_name :-
    lookup(Core ^ c_constructors, CtorId, Info).

core_get_constructor_type(Core, CtorId, Info ^ ci_type_id) :-
    lookup(Core ^ c_constructors, CtorId, Info).

core_get_constructor_det(Core, CtorId, Info ^ ci_constructor) :-
    lookup(Core ^ c_constructors, CtorId, Info).

core_set_constructor(CtorId, Name, TypeId, Cons, !Core) :-
    Info = ctor_info(Name, length(Cons ^ c_fields), TypeId, Cons),
    det_insert(CtorId, Info, !.Core ^ c_constructors, ConsMap),
    !Core ^ c_constructors := ConsMap.

%-----------------------------------------------------------------------
%-----------------------------------------------------------------------

core_allocate_resource_id(ResId, !Core) :-
    ResId = !.Core ^ c_next_res_id,
    resource_id(N) = ResId,
    !Core ^ c_next_res_id := resource_id(N+1).

core_set_resource(ResId, Res, !Core) :-
    map.set(ResId, Res, !.Core ^ c_resources, NewMap),
    !Core ^ c_resources := NewMap.

core_get_resource(Core, ResId) = map.lookup(Core ^ c_resources, ResId).

core_all_exported_resources(Core) =
    filter(resource_is_exported, to_assoc_list(Core ^ c_resources)).

:- pred resource_is_exported(pair(resource_id, resource)::in) is semidet.

resource_is_exported(_ - r_other(_, _, s_public, _, _)).

%-----------------------------------------------------------------------%
