%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module core.
%
% Copyright (C) 2015-2016 Plasma Team
% Distributed under the terms of the MIT see ../LICENSE.code
%
% Plasma core representation
%
%-----------------------------------------------------------------------%

:- interface.

:- include_module core.code.
:- include_module core.function.
:- include_module core.pretty.
:- include_module core.types.
:- include_module core.typecheck.

%-----------------------------------------------------------------------%

:- import_module list.
:- import_module map.
:- import_module set.

:- import_module context.
:- import_module common_types.
:- import_module core.code.
:- import_module core.function.
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

:- pred core_allocate_cons_id(cons_id::out, core::in, core::out) is det.

:- pred core_set_constructor(cons_id::in, constructor::in,
    core::in, core::out) is det.

:- pred core_lookup_constructor_name(core::in, cons_id::in, q_name::out)
    is det.

%-----------------------------------------------------------------------%

    % In later verious resources can be named and may have types, with rules
    % about which resources contain which other resources.  For now there is
    % only an IO resource.
    %
:- type resource
    --->    r_io.

:- func resource_to_string(resource) = string.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

:- import_module bimap.
:- import_module digraph.
:- import_module int.
:- import_module maybe.
:- import_module require.
:- import_module set.

%-----------------------------------------------------------------------%

:- type core
    --->    core(
                c_module_name       :: q_name,

                c_funcs             :: map(func_id, function),
                c_next_func_id      :: func_id,
                c_entry_func_id     :: maybe(func_id),

                c_next_type_id      :: type_id,

                c_next_cons_id      :: cons_id,
                c_constructors      :: map(cons_id, constructor)
            ).

%-----------------------------------------------------------------------%

init(ModuleName) =
    core(ModuleName,
        % Functions
        init, func_id(0), no,
        % Types
        type_id(0), cons_id(0), init
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

core_allocate_cons_id(ConsId, !Core) :-
    ConsId = !.Core ^ c_next_cons_id,
    ConsId = cons_id(N),
    !Core ^ c_next_cons_id := cons_id(N+1).

:- pred core_get_constructor_det(core::in, cons_id::in, constructor::out)
    is det.

core_get_constructor_det(Core, ConsId, Cons) :-
    lookup(Core ^ c_constructors, ConsId, Cons).

core_set_constructor(ConsId, Cons, !Core) :-
    set(ConsId, Cons, !.Core ^ c_constructors, ConsMap),
    !Core ^ c_constructors := ConsMap.

core_lookup_constructor_name(Core, ConsId, Name) :-
    core_get_constructor_det(Core, ConsId, Cons),
    Name = Cons ^ c_name.

%-----------------------------------------------------------------------
%-----------------------------------------------------------------------

resource_to_string(r_io) = "IO".

%-----------------------------------------------------------------------%
