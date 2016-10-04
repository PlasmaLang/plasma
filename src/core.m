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

:- pred core_register_function(q_name::in, func_id::out,
    core::in, core::out) is semidet.

:- func core_all_functions(core) = list(func_id).

:- func core_all_nonimported_functions(core) = list(func_id).

:- pred core_search_function(core::in, q_name::in, func_id::out) is semidet.

:- pred core_lookup_function(core::in, q_name::in, func_id::out) is det.

:- pred core_lookup_function_name(core::in, func_id::in, q_name::out) is det.

    % Return the exact match.
    %
:- pred det_core_lookup_function(core::in, q_name::in, func_id::out) is det.

:- pred core_set_function(func_id::in, function::in, core::in, core::out)
    is det.

:- pred core_get_function_det(core::in, func_id::in, function::out) is det.

%-----------------------------------------------------------------------%

    % Return all the non-imported functions, topologically sorted into their
    % SCCs.
    %
:- func core_all_nonimported_functions_sccs(core) = list(set(func_id)).

%-----------------------------------------------------------------------%

:- pred core_register_type(q_name::in, type_id::out, core::in, core::out)
    is semidet.

:- pred core_register_constructor(q_name::in, cons_id::out,
    core::in, core::out) is semidet.

:- pred core_lookup_constructor(core::in, q_name::in, cons_id::out) is det.

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

% TODO: I don't like having symbol -> ID mappings here.  I think that all of
% these should be handled by pre.env.m

:- type core
    --->    core(
                c_module_name       :: q_name,
                c_funcs             :: map(func_id, function),
                c_func_syms         :: bimap(q_name, func_id),
                c_next_func_id      :: func_id,

                c_type_syms         :: bimap(q_name, type_id),
                c_next_type_id      :: type_id,

                c_cons_syms         :: bimap(q_name, cons_id),
                c_next_cons_id      :: cons_id
            ).

%-----------------------------------------------------------------------%

init(ModuleName) =
    core(ModuleName,
        % Functions
        init, init, func_id(0),
        % Types
        init, type_id(0), init, cons_id(0)
    ).

module_name(Core) = Core ^ c_module_name.

%-----------------------------------------------------------------------%

core_register_function(Symbol, FuncId, !Core) :-
    FuncId = !.Core ^ c_next_func_id,
    insert(Symbol, FuncId, !.Core ^ c_func_syms, FuncSyms),
    !Core ^ c_func_syms := FuncSyms,
    FuncId = func_id(N),
    !Core ^ c_next_func_id := func_id(N+1).

core_all_functions(Core) = keys(Core ^ c_funcs).

core_all_nonimported_functions(Core) =
    filter(is_nonimported(Core), core_all_functions(Core)).

:- pred is_nonimported(core::in, func_id::in) is semidet.

is_nonimported(Core, FuncId) :-
    core_get_function_det(Core, FuncId, Func),
    func_get_body(Func, _, _, _).

core_search_function(Core, Symbol, FuncId) :-
    search(Core ^ c_func_syms, Symbol, FuncId).

core_lookup_function(Core, Symbol, FuncId) :-
    ( if search(Core ^ c_func_syms, Symbol, FuncIdP) then
        FuncId = FuncIdP
    else if
        % Temporary work around.  Until environments work properly and we
        % setup a prelude this will ensure that builtin function names don't
        % need to be qualified.
        q_name_parts(Symbol, [], Name),
        search(Core ^ c_func_syms, q_name(["builtin"], Name),
            FuncIdP)
    then
        FuncId = FuncIdP
    else
        unexpected($file, $pred, "Symbol not found")
    ).

core_lookup_function_name(Core, FuncId, Symbol) :-
    reverse_lookup(Core ^ c_func_syms, Symbol, FuncId).

det_core_lookup_function(Core, Symbol, FuncId) :-
    core_lookup_function(Core, Symbol, FuncId).

core_set_function(FuncId, Func, !Core) :-
    map.set(FuncId, Func, !.Core ^ c_funcs, Funcs),
    !Core ^ c_funcs := Funcs.

core_get_function_det(Core, FuncId, Func) :-
    map.lookup(Core ^ c_funcs, FuncId, Func).

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

core_register_type(Symbol, TypeId, !Core) :-
    TypeId = !.Core ^ c_next_type_id,
    insert(Symbol, TypeId, !.Core ^ c_type_syms, TypeSyms),
    !Core ^ c_type_syms := TypeSyms,
    TypeId = type_id(N),
    !Core ^ c_next_type_id := type_id(N+1).

core_register_constructor(Symbol, ConsId, !Core) :-
    ConsId = !.Core ^ c_next_cons_id,
    insert(Symbol, ConsId, !.Core ^ c_cons_syms, ConsSyms),
    !Core ^ c_cons_syms := ConsSyms,
    ConsId = cons_id(N),
    !Core ^ c_next_cons_id := cons_id(N+1).

core_lookup_constructor(Core, Symbol, ConsId) :-
    lookup(Core ^ c_cons_syms, Symbol, ConsId).

%-----------------------------------------------------------------------
%-----------------------------------------------------------------------

resource_to_string(r_io) = "IO".

%-----------------------------------------------------------------------%
