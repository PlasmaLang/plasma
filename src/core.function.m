%-----------------------------------------------------------------------%
% Plasma function representation
% vim: ts=4 sw=4 et
%
% Copyright (C) Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
%-----------------------------------------------------------------------%
:- module core.function.
%-----------------------------------------------------------------------%
:- interface.

:- import_module context.
:- import_module pz.
:- import_module pz.code.

%-----------------------------------------------------------------------%

:- type function.

    % func_init_user(Name, Context, Sharing, ParamTypes, ReturnTypes,
    %   Uses, Observes) = Function
    %
    % Create a user-provided function.
    %
:- func func_init_user(q_name, context, sharing, list(type_), list(type_),
    set(resource_id), set(resource_id)) = function.

    % func_init_builtin_inline_pz(Name, Inputs, Outputs, Uses, Observes,
    %   Defn) = Function
    %
    % Creates a builitn function defined by a list of PZ instructions.  See
    % comment in builtins.m
    %
:- func func_init_builtin_inline_pz(q_name, list(type_), list(type_),
    set(resource_id), set(resource_id), list(pz_instr)) = function.

    % func_init_builtin_rts(Name, Inputs, Outputs, Uses, Observes) =
    %   Function
    %
    % Creates a builtin function that will be defined by the runtime system.
    %
:- func func_init_builtin_rts(q_name, list(type_), list(type_),
    set(resource_id), set(resource_id)) = function.

    % func_init_builtin_core(Name, Inputs, Outputs, Uses, Observes) =
    %   Function
    %
    % Creates a builtin function that has a "Plasma core" representation
    % compiled in each module and made available to optimisations.
    %
:- func func_init_builtin_core(q_name, list(type_), list(type_),
    set(resource_id), set(resource_id)) = function.

    % func_init_anon(ModuleName, Sharing, Params, Results, Uses, Observes)
    %
:- func func_init_anon(q_name, sharing, list(type_), list(type_),
    set(resource_id), set(resource_id)) = function.

:- func func_get_name(function) = q_name.

:- func func_get_context(function) = context.

:- func func_get_imported(function) = imported.

:- pred func_set_imported(function::in, function::out) is det.

:- func func_get_sharing(function) = sharing.

:- pred func_get_type_signature(function::in, list(type_)::out,
    list(type_)::out, arity::out) is det.

    % func_get_resource_signature(Func, Uses, Observes).
    %
:- pred func_get_resource_signature(function::in,
    set(resource_id)::out, set(resource_id)::out) is det.

:- type func_is_used
    --->    used_probably
    ;       unused.

:- func func_get_used(function) = func_is_used.

:- pred func_set_used(func_is_used::in, function::in, function::out) is det.

%-----------------------------------------------------------------------%

:- pred func_set_captured_vars_types(list(type_)::in,
    function::in, function::out) is det.

    % Throws an exception if typechecking has not provided this.
    %
:- func func_get_captured_vars_types(function) = list(type_).

:- func func_maybe_captured_vars_types(function) = maybe(list(type_)).

%-----------------------------------------------------------------------%

:- pred func_is_builtin(function::in) is semidet.

    % The three main types of builtins.  See the comment at the beginning of
    % builtins.m.  This only makes sense for functions in the builtin
    % module.
    %
:- type builtin_impl_type
    --->    bit_core         % Builtins implemented by the compiler in core
                             % representation.

    ;       bit_inline_pz    % Builtins implemented by the compiler by
                             % replacing their use with PZ instructions (eg
                             % math operators)

    ;       bit_rts.         % Bultins implemented by the RTS.

    % Get how this function's definition is provided if it is a builtin,
    % false otherwise.
    %
:- pred func_builtin_type(function::in, builtin_impl_type::out) is semidet.

:- pred func_set_builtin(builtin_impl_type::in, function::in, function::out)
    is det.

:- pred func_builtin_inline_pz(function::in, list(pz_instr)::out)
    is semidet.

:- pred func_set_foreign(function::in, function::out) is det.

:- pred func_is_foreign(function::in) is semidet.

:- type code_type
    --->    ct_plasma
    ;       ct_foreign
    ;       ct_builtin(
                builtin_impl_type
            ).

:- func func_get_code_type(function) = code_type.

%-----------------------------------------------------------------------%

    % func_set_body(Varmap, Params, Captured, Body, !func).
    %
:- pred func_set_body(varmap::in, list(var)::in, list(var)::in, expr::in,
    function::in, function::out) is det.

:- pred func_set_body(varmap::in, list(var)::in, list(var)::in, expr::in,
    map(var, type_)::in, function::in, function::out) is det.

:- pred func_set_vartypes(map(var, type_)::in, function::in, function::out)
    is det.

    % func_get_body(Func, Varmap, ParamNames, Captured, Expr)
    %
:- pred func_get_body(function::in, varmap::out, list(var)::out,
    list(var)::out, expr::out) is semidet.

    % func_get_body_det(Func, Varmap, ParamNames, Captured, Expr)
    %
:- pred func_get_body_det(function::in, varmap::out, list(var)::out,
    list(var)::out, expr::out) is det.

:- pred func_get_varmap(function::in, varmap::out) is semidet.

:- pred func_get_vartypes(function::in, map(var, type_)::out) is semidet.

:- func func_get_vartypes_det(function) = map(var, type_).

:- pred func_raise_error(function::in, function::out) is det.

:- pred func_has_error(function::in) is semidet.

%-----------------------------------------------------------------------%

:- func func_get_callees(function) = set(func_id).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
:- implementation.

:- import_module string.
:- import_module require.

%-----------------------------------------------------------------------%

:- type function
    --->    function(
                f_name              :: q_name,
                f_signature         :: signature,
                f_context           :: context,
                f_sharing           :: sharing,
                f_maybe_func_defn   :: maybe(function_defn),

                    % Some builtins may be defined by a list of PZ
                    % instructions.
                f_maybe_ipz_defn    :: maybe(list(pz_instr)),
                f_code_type         :: code_type,
                f_imported          :: imported,
                f_used              :: func_is_used,
                f_has_errors        :: has_errors
            ).

:- type signature
    --->    signature(
                % The parameter and return types are given in the order they
                % appear in function's definition.
                fs_param_types      :: list(type_),
                fs_return_types     :: list(type_),
                fs_captured_types   :: maybe(list(type_)),
                % It seems redundant to store the list of return types and
                % the arity.  However in the future return types may be
                % inferred, and therefore won't be available all the time.
                fs_arity            :: arity,
                fs_uses             :: set(resource_id),
                fs_observes         :: set(resource_id)
            ).

:- type function_defn
    --->    function_defn(
                fd_var_map          :: varmap,
                fd_param_names      :: list(var),
                fd_maybe_var_types  :: maybe(map(var, type_)),
                fd_captured         :: list(var),
                fd_body             :: expr
            ).

:- type has_errors
    --->    does_not_have_errors
    ;       has_errors.

%-----------------------------------------------------------------------%

func_init_user(Name, Context, Sharing, Params, Return, Uses, Observes) =
    func_init(Name, Context, Sharing, Params, Return, Uses, Observes).

func_init_builtin_inline_pz(Name, Params, Return, Uses, Observes,
        PzInstrs) =
    func_init_builtin(Name, Params, Return, [], Uses, Observes,
        bit_inline_pz, no, yes(PzInstrs)).

func_init_builtin_rts(Name, Params, Return, Uses, Observes) =
    func_init_builtin(Name, Params, Return, [], Uses, Observes, bit_rts,
        no, no).

func_init_builtin_core(Name, Params, Return, Uses, Observes) =
    func_init_builtin(Name, Params, Return, [], Uses, Observes, bit_core,
        no, no).

:- func func_init_builtin(q_name, list(type_), list(type_), list(type_),
    set(resource_id), set(resource_id), builtin_impl_type,
    maybe(function_defn), maybe(list(pz_instr))) = function.

func_init_builtin(Name, Params, Return, Captured, Uses, Observes,
        BuiltinImplType, MbDefn, MbIPzDefn) = Func :-
    Context = builtin_context,
    Sharing = s_private,
    Arity = arity(length(Return)),
    CodeType = ct_builtin(BuiltinImplType),
    Func = function(Name, signature(Params, Return, yes(Captured), Arity,
        Uses, Observes), Context, Sharing, MbDefn, MbIPzDefn, CodeType,
        i_imported, used_probably, does_not_have_errors).

func_init_anon(ModuleName, Sharing, Params, Return, Uses, Observes) =
    func_init(q_name_append_str(ModuleName, "Anon"), nil_context,
        Sharing, Params, Return, Uses, Observes).

:- func func_init(q_name, context, sharing, list(type_), list(type_),
    set(resource_id), set(resource_id)) = function.

func_init(Name, Context, Sharing, Params, Return, Uses, Observes)
        = Func :-
    Arity = arity(length(Return)),
    Func = function(Name, signature(Params, Return, no, Arity, Uses, Observes),
        Context, Sharing, no, no, ct_plasma, i_local, used_probably,
        does_not_have_errors).

func_get_name(Func) = Func ^ f_name.

func_get_context(Func) = Func ^ f_context.

func_get_imported(Func) = Func ^ f_imported.

func_set_imported(!Func) :-
    !Func ^ f_signature ^ fs_captured_types := yes([]),
    !Func ^ f_imported := i_imported.

func_get_sharing(Func) = Func ^ f_sharing.

func_get_type_signature(Func, Inputs, Outputs, Arity) :-
    Inputs = Func ^ f_signature ^ fs_param_types,
    Outputs = Func ^ f_signature ^ fs_return_types,
    Arity = Func ^ f_signature ^ fs_arity.

func_get_resource_signature(Func, Uses, Observes) :-
    Uses = Func ^ f_signature ^ fs_uses,
    Observes = Func ^ f_signature ^ fs_observes.

func_get_used(Func) = Func ^ f_used.

func_set_used(Used, !Func) :-
    !Func ^ f_used := Used.

%-----------------------------------------------------------------------%

func_set_captured_vars_types(Types, !Func) :-
    !Func ^ f_signature ^ fs_captured_types := yes(Types).

func_get_captured_vars_types(Func) = Types :-
    MaybeTypes = func_maybe_captured_vars_types(Func),
    ( MaybeTypes = yes(Types)
    ; MaybeTypes = no,
        unexpected($file, $pred,
            format("Captured vars' types unknown for %s",
                [s(q_name_to_string(Func ^ f_name))]))
    ).

func_maybe_captured_vars_types(Func) =
    Func ^ f_signature ^ fs_captured_types.

%-----------------------------------------------------------------------%

func_is_builtin(Func) :-
    func_builtin_type(Func, _).

func_builtin_type(Func, BuiltinType) :-
    ct_builtin(BuiltinType) = Func ^ f_code_type.

func_set_builtin(BuiltinType, !Func) :-
    ( if
        not func_is_builtin(!.Func),
        not func_is_foreign(!.Func),
        no = !.Func ^ f_maybe_func_defn
    then
        !Func ^ f_code_type := ct_builtin(BuiltinType),
        func_set_captured_vars_types([], !Func)
    else
        unexpected($file, $pred,
            "Function is already builtin or already has a body")
    ).

func_builtin_inline_pz(Func, PZInstrs) :-
    yes(PZInstrs) = Func ^ f_maybe_ipz_defn.

func_set_foreign(!Func) :-
    ( if
        not func_is_builtin(!.Func),
        not func_is_foreign(!.Func),
        no = !.Func ^ f_maybe_func_defn
    then
        !Func ^ f_code_type := ct_foreign,
        func_set_captured_vars_types([], !Func)
    else
        unexpected($file, $pred,
            "Function is already builtin or already has a body")
    ).

func_is_foreign(Func) :-
    Func ^ f_code_type = ct_foreign.

func_get_code_type(Func) = Func ^ f_code_type.

%-----------------------------------------------------------------------%

func_set_body(Varmap, ParamNames, Captured, Expr, !Function) :-
    ( if func_get_vartypes(!.Function, Vartypes) then
        MaybeVartypes = yes(Vartypes)
    else
        MaybeVartypes = no
    ),
    Defn = function_defn(Varmap, ParamNames, MaybeVartypes, Captured, Expr),
    !Function ^ f_maybe_func_defn := yes(Defn).

func_set_body(Varmap, ParamNames, Captured, Expr, VarTypes, !Function) :-
    Defn = function_defn(Varmap, ParamNames, yes(VarTypes), Captured, Expr),
    !Function ^ f_maybe_func_defn := yes(Defn).

func_set_vartypes(VarTypes, !Function) :-
    MaybeDefn0 = !.Function ^ f_maybe_func_defn,
    ( MaybeDefn0 = no,
        unexpected($file, $pred, "No function body")
    ; MaybeDefn0 = yes(function_defn(Varmap, ParamNames, _, Captured, Expr)),
        Defn = function_defn(Varmap, ParamNames, yes(VarTypes),
            Captured, Expr),
        !Function ^ f_maybe_func_defn := yes(Defn)
    ).

func_get_body(Func, Varmap, ParamNames, Captured, Expr) :-
    yes(Defn) = Func ^ f_maybe_func_defn,
    function_defn(Varmap, ParamNames, _VarTypes, Captured, Expr) = Defn.

func_get_body_det(Func, Varmap, ParamNames, Captured, Expr) :-
    ( if func_get_body(Func, VarmapP, ParamNamesP, CapturedP, ExprP) then
        Varmap = VarmapP,
        ParamNames = ParamNamesP,
        Captured = CapturedP,
        Expr = ExprP
    else
        unexpected($file, $pred, "coudln't get predicate baody")
    ).

func_get_varmap(Func, Varmap) :-
    func_get_body(Func, Varmap, _, _, _).

func_get_vartypes(Func, VarTypes) :-
    yes(Defn) = Func ^ f_maybe_func_defn,
    yes(VarTypes) = Defn ^ fd_maybe_var_types.

func_get_vartypes_det(Func) = VarTypes :-
    ( if func_get_vartypes(Func, VarTypesPrime) then
        VarTypes = VarTypesPrime
    else
        unexpected($file, $pred, "No VarTypes")
    ).

func_raise_error(!Func) :-
    !Func ^ f_has_errors := has_errors.

func_has_error(Func) :-
    Func ^ f_has_errors = has_errors.

%-----------------------------------------------------------------------%

func_get_callees(Func) = Callees :-
    MaybeDefn = Func ^ f_maybe_func_defn,
    ( MaybeDefn = yes(function_defn(_, _, _, _, Body)),
        Callees = expr_get_callees(Body)
    ; MaybeDefn = no,
        Callees = set.init
    ).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
