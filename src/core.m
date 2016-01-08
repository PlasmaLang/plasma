%-----------------------------------------------------------------------%
% Plasma core representation
% vim: ts=4 sw=4 et
%
% Copyright (C) 2015 Paul Bone
% Distributed under the terms of the GPLv2 see ../LICENSE.tools
%
% This program compiles plasma modules.
%
%-----------------------------------------------------------------------%
:- module core.
%-----------------------------------------------------------------------%

:- interface.

:- include_module core.code.
:- include_module core.types.

%-----------------------------------------------------------------------%

:- import_module list.
:- import_module set.

:- import_module core.code.
:- import_module core.types.
:- import_module symtab.
:- import_module varmap.

%-----------------------------------------------------------------------%

:- type core.

:- func init(symbol) = core.

:- func module_name(core) = symbol.

:- pred core_register_function(symbol::in, func_id::out,
    core::in, core::out) is semidet.

:- pred core_set_function(func_id::in, function::in, core::in, core::out)
    is det.

%-----------------------------------------------------------------------%

    % Is a declration visible outside of its defining module.
    %
:- type sharing
    --->    s_public
    ;       s_private.

%-----------------------------------------------------------------------%

:- type func_id.

:- type function.

    % function_init(SHAring, ParamTypes, ReturnType, UsingResources,
    %   ObservingResources) = Function
    %
:- func function_init(sharing, list(type_), type_, set(resource),
    set(resource)) = function.

:- pred function_set_body(varmap::in, list(var)::in, expr::in,
    function::in, function::out) is det.

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

:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module set.

%-----------------------------------------------------------------------%

:- type core
    --->    core(
                c_module_name       :: symbol,
                c_funcs             :: map(func_id, function),
                c_func_syms         :: symtab(func_id),
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
    symtab.insert(Symbol, FuncId, !.Core ^ c_func_syms, FuncSyms),
    !Core ^ c_func_syms := FuncSyms,
    FuncId = func_id(N),
    !Core ^ c_next_func_id := func_id(N+1).

core_set_function(FuncId, Func, !Core) :-
    map.set(FuncId, Func, !.Core ^ c_funcs, Funcs),
    !Core ^ c_funcs := Funcs.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- type function
    --->    function(
                f_sharing           :: sharing,
                f_signature         :: signature,
                f_funC_defn         :: maybe(function_defn)
            ).

:- type signature
    --->    signature(
                fs_param_types  :: list(type_),
                fs_return_types :: list(type_),
                fs_using        :: set(resource),
                fs_observing    :: set(resource)
            ).

:- type function_defn
    --->    function_defn(
                fd_var_map          :: varmap,
                fd_param_names      :: list(var),
                fd_body             :: expr
            ).

%-----------------------------------------------------------------------%

function_init(Sharing, Params, Return, Using, Observing) =
    function(Sharing, signature(Params, [Return], Using, Observing), no).

function_set_body(Varmap, ParamNames, Stmts, !Function) :-
    !Function ^ f_funC_defn := yes(function_defn(Varmap, ParamNames, Stmts)).

%-----------------------------------------------------------------------%

resource_to_string(r_io) = "IO".

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
