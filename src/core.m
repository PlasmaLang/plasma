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
:- include_module core.pretty.
:- include_module core.types.
:- include_module core.typecheck.

%-----------------------------------------------------------------------%

:- import_module list.
:- import_module set.

:- import_module context.
:- import_module common_types.
:- import_module core.code.
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

:- type func_id.

:- type function.

    % function_init(Context, Sharing, ParamTypes, ReturnTypes, UsingResources,
    %   ObservingResources) = Function
    %
:- func func_init(context, sharing, list(type_), list(type_), set(resource),
    set(resource)) = function.

:- func func_get_context(function) = context.

:- func func_get_imported(function) = imported.

:- pred func_get_signature(function::in, list(type_)::out, list(type_)::out,
    arity::out) is det.

:- pred func_set_body(varmap::in, list(var)::in, expr::in,
    function::in, function::out) is det.

:- pred func_get_body(function::in, varmap::out, list(var)::out,
    expr::out) is semidet.

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
:- import_module map.
:- import_module maybe.
:- import_module require.
:- import_module set.

%-----------------------------------------------------------------------%

:- type core
    --->    core(
                c_module_name       :: q_name,
                c_funcs             :: map(func_id, function),
                c_func_syms         :: bimap(q_name, func_id),
                c_next_func_id      :: func_id
            ).

%-----------------------------------------------------------------------%

init(ModuleName) = core(ModuleName, init, init, func_id(0)).

module_name(Core) = Core ^ c_module_name.

%-----------------------------------------------------------------------%

:- type func_id
    --->    func_id(int).

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

:- type function
    --->    function(
                f_signature         :: signature,
                f_maybe_func_defn   :: maybe(function_defn),
                f_context           :: context,
                f_sharing           :: sharing
            ).
:- type signature
    --->    signature(
                % The parameter and return types are given in the order they
                % appear in function's definition.
                fs_param_types      :: list(type_),
                fs_return_types     :: list(type_),
                % It seems redundant to store the list of return types and
                % the arity.  However in the future return types may be
                % inferred, and therefore won't be available all the time.
                fs_arity            :: arity,
                fs_using            :: set(resource),
                fs_observing        :: set(resource)
            ).

:- type function_defn
    --->    function_defn(
                fd_var_map          :: varmap,
                fd_param_names      :: list(var),
                fd_body             :: expr
            ).

%-----------------------------------------------------------------------%

func_init(Context, Sharing, Params, Return, Using, Observing) = Func :-
    Arity = arity(length(Return)),
    Func = function(signature(Params, Return, Arity, Using, Observing),
        no, Context, Sharing).

func_get_context(Func) = Func ^ f_context.

func_get_imported(Func) = Imported :-
    MaybeDefn = Func ^ f_maybe_func_defn,
    ( MaybeDefn = yes(_),
        Imported = i_local
    ; MaybeDefn = no,
        Imported = i_imported
    ).

func_get_signature(Func, Inputs, Outputs, Arity) :-
    Inputs = Func ^ f_signature ^ fs_param_types,
    Outputs = Func ^ f_signature ^ fs_return_types,
    Arity = Func ^ f_signature ^ fs_arity.

func_set_body(Varmap, ParamNames, Stmts, !Function) :-
    Defn = function_defn(Varmap, ParamNames, Stmts),
    !Function ^ f_maybe_func_defn := yes(Defn).

func_get_body(Func, Varmap, ParamNames, Stmts) :-
    yes(Defn) = Func ^ f_maybe_func_defn,
    function_defn(Varmap, ParamNames, Stmts) = Defn.

%-----------------------------------------------------------------------%

:- func func_get_callees(function) = set(func_id).

func_get_callees(Func) = Callees :-
    MaybeDefn = Func ^ f_maybe_func_defn,
    ( MaybeDefn = yes(Defn),
        Callees = expr_get_callees(Defn ^ fd_body)
    ; MaybeDefn = no,
        Callees = set.init
    ).

%-----------------------------------------------------------------------

resource_to_string(r_io) = "IO".

%-----------------------------------------------------------------------%
