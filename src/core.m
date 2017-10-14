%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module core.
%
% Copyright (C) 2015-2017 Plasma Team
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

:- include_module core.branch.
:- include_module core.typecheck.

:- include_module core.util.

%-----------------------------------------------------------------------%

:- import_module list.
:- import_module map.
:- import_module set.

:- import_module context.
:- import_module common_types.
:- import_module core.code.
:- import_module core.function.
:- import_module core.resource.
:- import_module core.types.
:- import_module q_name.
:- import_module varmap.

%-----------------------------------------------------------------------%

:- type core.

:- func init(q_name) = core.

:- func module_name(core) = q_name.

:- pred core_allocate_function(func_id::out, core::in, core::out) is det.

:- func core_all_functions(core) = list(func_id).

:- func core_all_nonimported_functions(core) = list(func_id).

:- pred core_set_function(func_id::in, function::in, core::in, core::out)
    is det.

:- pred core_get_function_det(core::in, func_id::in, function::out) is det.

:- pred core_entry_function(core::in, func_id::out) is semidet.

:- pred core_set_entry_function(func_id::in, core::in, core::out) is det.

:- pred core_lookup_function_name(core::in, func_id::in, q_name::out)
    is det.

%-----------------------------------------------------------------------%

    % Return all the non-imported functions, topologically sorted into their
    % SCCs.
    %
:- func core_all_nonimported_functions_sccs(core) = list(set(func_id)).

%-----------------------------------------------------------------------%

:- pred core_allocate_type_id(type_id::out, core::in, core::out) is det.

:- func core_all_types(core) = list(type_id).

:- func core_get_type(core, type_id) = user_type.

:- pred core_set_type(type_id::in, user_type::in, core::in, core::out)
    is det.

:- pred core_lookup_type_name(core::in, type_id::in, q_name::out) is det.

:- pred core_allocate_ctor_id(ctor_id::out, q_name::in, core::in, core::out)
    is det.

:- pred core_lookup_constructor_name(core::in, ctor_id::in, q_name::out)
    is det.

:- pred core_get_constructor_types(core::in, ctor_id::in, int::in,
    set(type_id)::out) is det.

:- pred core_get_constructor_det(core::in, type_id::in, ctor_id::in,
    constructor::out) is det.

:- pred core_set_constructor(type_id::in, ctor_id::in, constructor::in,
    core::in, core::out) is det.

%-----------------------------------------------------------------------%

:- pred core_allocate_resource_id(resource_id::out, core::in, core::out)
    is det.

:- pred core_set_resource(resource_id::in, resource::in,
    core::in, core::out) is det.

:- func core_get_resource(core, resource_id) = resource.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

:- import_module bimap.
:- import_module digraph.
:- import_module int.
:- import_module maybe.
:- import_module require.
:- import_module set.

:- import_module util.

%-----------------------------------------------------------------------%

:- type core
    --->    core(
                c_module_name       :: q_name,

                c_funcs             :: map(func_id, function),
                c_next_func_id      :: func_id,
                c_entry_func_id     :: maybe(func_id),

                c_next_type_id      :: type_id,
                c_types             :: map(type_id, user_type),

                c_next_ctor_id      :: ctor_id,
                c_constructor_infos :: map(ctor_id, ctor_info),
                c_constructors      :: map({type_id, ctor_id}, constructor),

                c_next_res_id       :: resource_id,
                c_resources         :: map(resource_id, resource)
            ).

:- type ctor_info
    --->    ctor_info(
                ci_name             :: q_name,
                ci_arity_type_map   :: map(int, set(type_id))
            ).

%-----------------------------------------------------------------------%

init(ModuleName) =
    core(ModuleName,
        % Functions
        init, func_id(0), no,
        % Types
        type_id(0), init,
        % Constructors
        ctor_id(0), init, init,
        % Resources
        resource_id(0), init
    ).

module_name(Core) = Core ^ c_module_name.

%-----------------------------------------------------------------------%

core_allocate_function(FuncId, !Core) :-
    FuncId = !.Core ^ c_next_func_id,
    FuncId = func_id(N),
    !Core ^ c_next_func_id := func_id(N+1).

core_all_functions(Core) = keys(Core ^ c_funcs).

core_all_nonimported_functions(Core) =
    filter(is_nonimported(Core), core_all_functions(Core)).

:- pred is_nonimported(core::in, func_id::in) is semidet.

is_nonimported(Core, FuncId) :-
    core_get_function_det(Core, FuncId, Func),
    func_get_body(Func, _, _, _).

core_set_function(FuncId, Func, !Core) :-
    map.set(FuncId, Func, !.Core ^ c_funcs, Funcs),
    !Core ^ c_funcs := Funcs.

core_get_function_det(Core, FuncId, Func) :-
    map.lookup(Core ^ c_funcs, FuncId, Func).

core_entry_function(Core, FuncId) :-
    yes(FuncId) = Core ^ c_entry_func_id.

core_set_entry_function(FuncId, !Core) :-
    !Core ^ c_entry_func_id := yes(FuncId).

core_lookup_function_name(Core, FuncId, Name) :-
    core_get_function_det(Core, FuncId, Func),
    Name = func_get_name(Func).

%-----------------------------------------------------------------------%

core_all_nonimported_functions_sccs(Core) = SCCs :-
    AllFuncs = core_all_nonimported_functions(Core),
    AllFuncsSet = set(AllFuncs),
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

core_all_types(Core) = keys(Core ^ c_types).

core_get_type(Core, TypeId) = Type :-
    lookup(Core ^ c_types, TypeId, Type).

core_set_type(TypeId, Type, !Core) :-
    set(TypeId, Type, !.Core ^ c_types, Map),
    !Core ^ c_types := Map.

core_lookup_type_name(Core, TypeId, Name) :-
    Name = type_get_name(core_get_type(Core, TypeId)).

%-----------------------------------------------------------------------%

core_allocate_ctor_id(CtorId, Symbol, !Core) :-
    CtorId = !.Core ^ c_next_ctor_id,
    CtorId = ctor_id(N),
    !Core ^ c_next_ctor_id := ctor_id(N+1),
    Info = ctor_info(Symbol, map.init),
    det_insert(CtorId, Info, !.Core ^ c_constructor_infos, Infos),
    !Core ^ c_constructor_infos := Infos.

core_lookup_constructor_name(Core, CtorId, Name) :-
    lookup(Core ^ c_constructor_infos, CtorId, Info),
    Name = Info ^ ci_name.

core_get_constructor_types(Core, CtorId, Arity, Types) :-
    lookup(Core ^ c_constructor_infos, CtorId, Info),
    ( if search(Info ^ ci_arity_type_map, Arity, TypesPrime) then
        Types = TypesPrime
    else
        Types = set.init
    ).

core_get_constructor_det(Core, TypeId, CtorId, Cons) :-
    lookup(Core ^ c_constructors, {TypeId, CtorId}, Cons).

core_set_constructor(TypeId, CtorId, Cons, !Core) :-
    core_constructor_add_type_arity_det(CtorId, TypeId,
        length(Cons ^ c_fields), !Core),
    set({TypeId, CtorId}, Cons, !.Core ^ c_constructors, ConsMap),
    !Core ^ c_constructors := ConsMap.

:- pred core_constructor_add_type_arity(ctor_id::in, type_id::in, int::in,
    core::in, core::out) is semidet.

core_constructor_add_type_arity(CtorId, TypeId, Arity, !Core) :-
    lookup(!.Core ^ c_constructor_infos, CtorId, Info0),
    TypeMap0 = Info0 ^ ci_arity_type_map,
    % TODO: Should this check that the same type does not appear twice?
    core_get_constructor_types(!.Core, CtorId, Arity, Types0),
    set.insert_new(TypeId, Types0, Types),
    map.set(Arity, Types, TypeMap0, TypeMap),
    Info = Info0 ^ ci_arity_type_map := TypeMap,
    set(CtorId, Info, !.Core ^ c_constructor_infos, Infos),
    !Core ^ c_constructor_infos := Infos.

:- pred core_constructor_add_type_arity_det(ctor_id::in, type_id::in, int::in,
    core::in, core::out) is det.

core_constructor_add_type_arity_det(CtorId, TypeId, Arity, !Core) :-
    ( if core_constructor_add_type_arity(CtorId, TypeId, Arity, !Core) then
        true
    else
        util.compile_error($file, $pred,
            "This type already has a constructor with this name")
    ).

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

%-----------------------------------------------------------------------%
