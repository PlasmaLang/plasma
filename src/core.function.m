%-----------------------------------------------------------------------%
% Plasma function representation
% vim: ts=4 sw=4 et
%
% Copyright (C) 2015-2019 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
%-----------------------------------------------------------------------%
:- module core.function.
%-----------------------------------------------------------------------%
:- interface.

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
:- func func_init_builtin_rts(q_name, list(type_), list(type_), list(type_),
    set(resource_id), set(resource_id)) = function.

    % func_init_builtin_core(Name, Inputs, Outputs, Uses, Observes) =
    %   Function
    %
    % Creates a builtin function that has a "Plasma core" representation
    % compiled in each module and made available to optimisations.
    %
:- func func_init_builtin_core(q_name, list(type_), list(type_),
    list(type_), set(resource_id), set(resource_id)) = function.

    % func_init_anon(ModuleName, Sharing, Params, Results, Uses, Observes)
    %
:- func func_init_anon(q_name, sharing, list(type_), list(type_),
    set(resource_id), set(resource_id)) = function.

:- func func_get_name(function) = q_name.

:- func func_get_context(function) = context.

:- func func_get_imported(function) = imported.

:- pred func_get_type_signature(function::in, list(type_)::out,
    list(type_)::out, arity::out) is det.

    % func_get_resource_signature(Func, Uses, Observes).
    %
:- pred func_get_resource_signature(function::in,
    set(resource_id)::out, set(resource_id)::out) is det.

%-----------------------------------------------------------------------%

:- pred func_set_captured_vars_types(list(type_)::in,
    function::in, function::out) is det.

:- func func_get_captured_vars_types(function) = list(type_).

%-----------------------------------------------------------------------%

:- pred func_is_builtin(function::in) is semidet.

    % The three main types of builtins.  See the comment at the beginning of
    % builtins.m.  This only makes sense for functions in the builtin
    % module.
    %
:- type builtin_impl_type
    --->    bit_core
    ;       bit_inline_pz
            % Foreign and non-foreign bultins implemented by the RTS.
    ;       bit_rts.

    % Get how this function's definition is provided if it is a builtin,
    % false otherwise.
    %
:- pred func_builtin_type(function::in, builtin_impl_type::out) is semidet.

:- pred func_builtin_inline_pz(function::in, list(pz_instr)::out)
    is semidet.

%-----------------------------------------------------------------------%

    % func_set_body(Varmap, Params, Captured, Body, !func).
    %
:- pred func_set_body(varmap::in, list(var)::in, list(var)::in, expr::in,
    function::in, function::out) is det.

:- pred func_set_body(varmap::in, list(var)::in, list(var)::in, expr::in,
    map(var, type_)::in, function::in, function::out) is det.

:- pred func_set_vartypes(map(var, type_)::in, function::in, function::out)
    is det.

:- pred func_get_body(function::in, varmap::out, list(var)::out,
    list(var)::out, expr::out) is semidet.

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

%-----------------------------------------------------------------------%

:- type function
    --->    function(
                f_name              :: q_name,
                f_signature         :: signature,
                f_context           :: context,
                f_sharing           :: sharing,
                f_maybe_func_defn   :: function_defn,
                f_builtin           :: maybe(builtin_impl_type),
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
    --->    no_definition
    ;       function_defn(
                fd_var_map          :: varmap,
                fd_param_names      :: list(var),
                fd_maybe_var_types  :: maybe(map(var, type_)),
                fd_captured         :: list(var),
                fd_body             :: expr
            )
    ;       pz_inline_builtin(
                pib_instrs          :: list(pz_instr)
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
        bit_inline_pz, pz_inline_builtin(PzInstrs)).

func_init_builtin_rts(Name, Params, Return, Captured, Uses, Observes) =
    func_init_builtin(Name, Params, Return, Captured, Uses, Observes, bit_rts,
        no_definition).

func_init_builtin_core(Name, Params, Return, Captured, Uses, Observes) =
    func_init_builtin(Name, Params, Return, Captured, Uses, Observes, bit_core,
        no_definition).

:- func func_init_builtin(q_name, list(type_), list(type_), list(type_),
    set(resource_id), set(resource_id), builtin_impl_type, function_defn) =
    function.

func_init_builtin(Name, Params, Return, Captured, Uses, Observes,
        BuiltinImplType, Defn) = Func :-
    Context = nil_context,
    Sharing = s_private,
    Arity = arity(length(Return)),
    Builtin = yes(BuiltinImplType),
    Func = function(Name, signature(Params, Return, yes(Captured), Arity,
        Uses, Observes), Context, Sharing, Defn, Builtin, does_not_have_errors).

func_init_anon(ModuleName, Sharing, Params, Return, Uses, Observes) =
    func_init(q_name_snoc(ModuleName, "Anon"), nil_context,
        Sharing, Params, Return, Uses, Observes).

:- func func_init(q_name, context, sharing, list(type_), list(type_),
    set(resource_id), set(resource_id)) = function.

func_init(Name, Context, Sharing, Params, Return, Uses, Observes)
        = Func :-
    Arity = arity(length(Return)),
    Func = function(Name, signature(Params, Return, no, Arity, Uses, Observes),
        Context, Sharing, no_definition, no, does_not_have_errors).

func_get_name(Func) = Func ^ f_name.

func_get_context(Func) = Func ^ f_context.

func_get_imported(Func) = Imported :-
    % XXX: The import status should not be tied to the definition.
    MaybeDefn = Func ^ f_maybe_func_defn,
    (
        ( MaybeDefn = no_definition
        ; MaybeDefn = pz_inline_builtin(_)
        ),
        Imported = i_imported
    ; MaybeDefn = function_defn(_, _, _, _, _),
        Imported = i_local
    ).

func_get_type_signature(Func, Inputs, Outputs, Arity) :-
    Inputs = Func ^ f_signature ^ fs_param_types,
    Outputs = Func ^ f_signature ^ fs_return_types,
    Arity = Func ^ f_signature ^ fs_arity.

func_get_resource_signature(Func, Uses, Observes) :-
    Uses = Func ^ f_signature ^ fs_uses,
    Observes = Func ^ f_signature ^ fs_observes.

%-----------------------------------------------------------------------%

func_set_captured_vars_types(Types, !Func) :-
    !Func ^ f_signature ^ fs_captured_types := yes(Types).

func_get_captured_vars_types(Func) = Types :-
    MaybeTypes = Func ^ f_signature ^ fs_captured_types,
    ( MaybeTypes = yes(Types)
    ; MaybeTypes = no,
        unexpected($file, $pred, "Captured vars' types unknown")
    ).

%-----------------------------------------------------------------------%

func_is_builtin(Func) :-
    func_builtin_type(Func, _).

func_builtin_type(Func, BuiltinType) :-
    yes(BuiltinType) = Func ^ f_builtin.

func_builtin_inline_pz(Func, PZInstrs) :-
    pz_inline_builtin(PZInstrs) = Func ^ f_maybe_func_defn.

%-----------------------------------------------------------------------%

func_set_body(Varmap, ParamNames, Captured, Expr, VarTypes, !Function) :-
    Defn = function_defn(Varmap, ParamNames, yes(VarTypes), Captured, Expr),
    !Function ^ f_maybe_func_defn := Defn.

func_set_body(Varmap, ParamNames, Captured, Expr, !Function) :-
    ( if func_get_vartypes(!.Function, _) then
        unexpected($file, $pred,
            "This call will throw away old VarTypes information, " ++
            "use the 6 argument version instead")
    else
        true
    ),
    Defn = function_defn(Varmap, ParamNames, no, Captured, Expr),
    !Function ^ f_maybe_func_defn := Defn.

func_set_vartypes(VarTypes, !Function) :-
    MaybeDefn0 = !.Function ^ f_maybe_func_defn,
    ( MaybeDefn0 = no_definition,
        unexpected($file, $pred, "No function body")
    ; MaybeDefn0 = pz_inline_builtin(_),
        unexpected($file, $pred, "PZ inline builtin")
    ; MaybeDefn0 = function_defn(Varmap, ParamNames, _, Captured, Expr),
        MaybeDefn = function_defn(Varmap, ParamNames, yes(VarTypes),
            Captured, Expr),
        !Function ^ f_maybe_func_defn := MaybeDefn
    ).

func_get_body(Func, Varmap, ParamNames, Captured, Expr) :-
    Defn = Func ^ f_maybe_func_defn,
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


func_get_vartypes(Func, VarTypes) :-
    Defn = Func ^ f_maybe_func_defn,
    yes(VarTypes) = Defn ^ fd_maybe_var_types.

func_get_vartypes_det(Func) = VarTypes :-
    ( if func_get_vartypes(Func, VarTypesPrime) then
        VarTypes = VarTypesPrime
    else
        unexpected($file, $pred, "No VarTypes")
    ).

func_get_varmap(Func, Varmap) :-
    func_get_body(Func, Varmap, _, _, _).

func_raise_error(!Func) :-
    !Func ^ f_has_errors := has_errors.

func_has_error(Func) :-
    Func ^ f_has_errors = has_errors.

%-----------------------------------------------------------------------%

func_get_callees(Func) = Callees :-
    Defn = Func ^ f_maybe_func_defn,
    ( Defn = function_defn(_, _, _, _, Body),
        Callees = expr_get_callees(Body)
    ;
        ( Defn = no_definition
        ; Defn = pz_inline_builtin(_)
        ),
        Callees = set.init
    ).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
