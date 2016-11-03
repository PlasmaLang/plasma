%-----------------------------------------------------------------------%
% Plasma function representation
% vim: ts=4 sw=4 et
%
% Copyright (C) 2015-2016 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
%-----------------------------------------------------------------------%
:- module core.function.
%-----------------------------------------------------------------------%
:- interface.

%-----------------------------------------------------------------------%

:- type function.

    % function_init(Name, Context, Sharing, ParamTypes, ReturnTypes,
    %   UsingResources, ObservingResources) = Function
    %
:- func func_init(q_name, context, sharing, list(type_), list(type_),
    set(resource), set(resource)) = function.

:- func func_get_name(function) = q_name.

:- func func_get_context(function) = context.

:- func func_get_imported(function) = imported.

:- pred func_get_signature(function::in, list(type_)::out, list(type_)::out,
    arity::out) is det.

    % func_set_body(Varmap, Params, Body, !func).
    %
:- pred func_set_body(varmap::in, list(var)::in, expr::in,
    function::in, function::out) is det.

:- pred func_set_vartypes(map(var, type_)::in, function::in, function::out)
    is det.

:- pred func_get_body(function::in, varmap::out, list(var)::out, expr::out)
    is semidet.

:- pred func_get_vartypes(function::in, map(var, type_)::out) is semidet.

%-----------------------------------------------------------------------%

:- func func_get_callees(function) = set(func_id).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
:- implementation.

%-----------------------------------------------------------------------%

:- type function
    --->    function(
                f_name              :: q_name,
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
                fd_maybe_var_types  :: maybe(map(var, type_)),
                fd_body             :: expr
            ).

%-----------------------------------------------------------------------%

func_init(Name, Context, Sharing, Params, Return, Using, Observing) = Func :-
    Arity = arity(length(Return)),
    Func = function(Name, signature(Params, Return, Arity, Using, Observing),
        no, Context, Sharing).

func_get_name(Func) = Func ^ f_name.

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

func_set_body(Varmap, ParamNames, Expr, !Function) :-
    Defn = function_defn(Varmap, ParamNames, no, Expr),
    !Function ^ f_maybe_func_defn := yes(Defn).

func_set_vartypes(VarTypes, !Function) :-
    MaybeDefn0 = !.Function ^ f_maybe_func_defn,
    ( MaybeDefn0 = yes(Defn0)
    ; MaybeDefn0 = no,
        unexpected($file, $pred, "No function body")
    ),
    Defn = Defn0 ^ fd_maybe_var_types := yes(VarTypes),
    !Function ^ f_maybe_func_defn := yes(Defn).

func_get_body(Func, Varmap, ParamNames, Expr) :-
    yes(Defn) = Func ^ f_maybe_func_defn,
    function_defn(Varmap, ParamNames, _VarTypes, Expr) = Defn.

func_get_vartypes(Func, VarTypes) :-
    yes(Defn) = Func ^ f_maybe_func_defn,
    yes(VarTypes) = Defn ^ fd_maybe_var_types.

%-----------------------------------------------------------------------%

func_get_callees(Func) = Callees :-
    MaybeDefn = Func ^ f_maybe_func_defn,
    ( MaybeDefn = yes(Defn),
        Callees = expr_get_callees(Defn ^ fd_body)
    ; MaybeDefn = no,
        Callees = set.init
    ).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
