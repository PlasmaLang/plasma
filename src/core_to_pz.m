%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module core_to_pz.
%
% Copyright (C) 2015-2018 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
% Plasma core to pz conversion
%
%-----------------------------------------------------------------------%

:- interface.

:- import_module map.

:- import_module builtins.
:- import_module core.
:- import_module pz.
:- import_module q_name.

%-----------------------------------------------------------------------%

:- pred core_to_pz(core::in, pz::out) is det.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module string.

:- import_module context.
:- import_module common_types.
:- import_module core.code.
:- import_module core.function.
:- import_module core.types.
:- import_module pz.code.
:- import_module varmap.

:- include_module core_to_pz.code.
:- include_module core_to_pz.data.
:- import_module core_to_pz.code.
:- import_module core_to_pz.data.

%-----------------------------------------------------------------------%

core_to_pz(!.Core, !:PZ) :-
    !:PZ = init_pz,

    % Get ProcIds for builtin procedures.
    setup_pz_builtin_procs(BuiltinProcs, !PZ),

    % Remove higher order usage of builtin (C language) functions.
    setup_ho_builtins(!Core),

    % Make decisions about how data should be stored in memory.
    % This covers what tag values to use for each constructor and the IDs of
    % each structure.
    gen_constructor_data(!.Core, BuiltinProcs, TypeTagMap, TypeCtorTagMap, !PZ),

    % Generate constants.
    FuncIds = core_all_functions(!.Core),
    foldl2(gen_const_data(!.Core), FuncIds, init, DataMap, !PZ),

    % Generate functions.
    foldl3(make_proc_id_map(!.Core), FuncIds,
        init, ProcIdMap, init, OpIdMap, !PZ),
    map(gen_proc(!.Core, OpIdMap, ProcIdMap, BuiltinProcs, TypeTagMap,
            TypeCtorTagMap, DataMap),
        keys(ProcIdMap), Procs),
    foldl((pred((PID - P)::in, PZ0::in, PZ::out) is det :-
            pz_add_proc(PID, P, PZ0, PZ)
        ), Procs, !PZ),
    set_entry_function(!.Core, ProcIdMap, !PZ).

:- pred set_entry_function(core::in, map(func_id, pzp_id)::in,
    pz::in, pz::out) is det.

set_entry_function(Core, ProcIdMap, !PZ) :-
    ( if core_entry_function(Core, FuncId) then
        lookup(ProcIdMap, FuncId, PID),
        pz_set_entry_proc(PID, !PZ)
    else
        true
    ).

%-----------------------------------------------------------------------%

    % Find uses of builtin functions in higher order contexts and wrap them
    % in a Plasma function so they can be called with the plasma calling
    % convention.
    %
:- pred setup_ho_builtins(core::in, core::out) is det.

setup_ho_builtins(!Core) :-
    FuncIds = core_all_nonimported_functions(!.Core),
    foldl2(setup_ho_builtins_func, FuncIds, !Core, init, _).

:- pred setup_ho_builtins_func(func_id::in, core::in, core::out,
    map(func_id, func_id)::in, map(func_id, func_id)::out) is det.

setup_ho_builtins_func(FuncId, !Core, !Map) :-
    core_get_function_det(!.Core, FuncId, Func0),
    func_get_body_det(Func0, Varmap, Args, Body0),
    VarTypes = func_get_vartypes_det(Func0),
    setup_ho_builtins_expr(Body0, Body, !Core, !Map),
    func_set_body(Varmap, Args, Body, VarTypes, Func0, Func),
    core_set_function(FuncId, Func, !Core).

:- pred setup_ho_builtins_expr(expr::in, expr::out, core::in, core::out,
    map(func_id, func_id)::in, map(func_id, func_id)::out) is det.

setup_ho_builtins_expr(!Expr, !Core, !Map) :-
    EType0 = !.Expr ^ e_type,
    ( EType0 = e_tuple(Exprs0),
        map_foldl2(setup_ho_builtins_expr, Exprs0, Exprs, !Core, !Map),
        EType = e_tuple(Exprs)
    ; EType0 = e_let(Vars, LetExpr0, InExpr0),
        setup_ho_builtins_expr(LetExpr0, LetExpr, !Core, !Map),
        setup_ho_builtins_expr(InExpr0, InExpr, !Core, !Map),
        EType = e_let(Vars, LetExpr, InExpr)
    ;
        ( EType0 = e_call(_, _, _)
        ; EType0 = e_var(_)
        ; EType0 = e_construction(_, _)
        ),
        EType = EType0
    ; EType0 = e_match(Var, Cases0),
        map_foldl2(setup_ho_builtins_case, Cases0, Cases, !Core, !Map),
        EType = e_match(Var, Cases)
    ; EType0 = e_constant(ConstType0),
        (
            ( ConstType0 = c_string(_)
            ; ConstType0 = c_number(_)
            ; ConstType0 = c_ctor(_)
            ),
            ConstType = ConstType0
        ; ConstType0 = c_func(Func0),
            maybe_setup_ho_builtin(Func0, Func, !Core, !Map),
            ConstType = c_func(Func)
        ),
        EType = e_constant(ConstType)
    ),
    !Expr ^ e_type := EType.

:- pred setup_ho_builtins_case(expr_case::in, expr_case::out,
    core::in, core::out,
    map(func_id, func_id)::in, map(func_id, func_id)::out) is det.

setup_ho_builtins_case(e_case(Pat, Expr0), e_case(Pat, Expr), !Core, !Map) :-
    setup_ho_builtins_expr(Expr0, Expr, !Core, !Map).

:- pred maybe_setup_ho_builtin(func_id::in, func_id::out,
    core::in, core::out,
    map(func_id, func_id)::in, map(func_id, func_id)::out) is det.

maybe_setup_ho_builtin(FuncId0, FuncId, !Core, !Map) :-
    ( if search(!.Map, FuncId0, FuncIdPrime) then
        FuncId = FuncIdPrime
    else
        core_get_function_det(!.Core, FuncId0, Func),
        ( if func_builtin_type(Func, BT) then
            (
                BT = bit_core,
                % There's no need to wrap a core builtin, the regular
                % higher-order call code will work like normal.
                FuncId = FuncId0
            ;
                ( BT = bit_inline_pz
                ; BT = bit_rts
                ),
                % Wrap the builtin.
                setup_ho_builtin(FuncId0, Func, FuncId, !Core),
                det_insert(FuncId0, FuncId, !Map)
            )
        else
            FuncId = FuncId0
        )
    ).

:- pred setup_ho_builtin(func_id::in, function::in, func_id::out,
    core::in, core::out) is det.

setup_ho_builtin(CalleeId, Callee, WrapperId, !Core) :-
    func_get_type_signature(Callee, Args, Returns, _),
    func_get_resource_signature(Callee, Uses, Observes),
    some [!Wrapper, !Varmap, !Vartypes, !CallInfo] (
        % It might be nice to use a single PZ call instruction here, or copy
        % the PZ instructions from the function we're referring to.  However
        % in the first case we don't know the ProcId yet, we could re-order
        % things but we'd need to break a cycle somewhere, or generate some
        % ProcIds now and some later.  This naive solution is simple:
        % TODO we should optimise this, and avoid the code duplication
        % also.
        core_allocate_function(WrapperId, !Core),
        !:Wrapper = func_init_anon(module_name(!.Core), s_private,
            Args, Returns, Uses, Observes),
        !:Varmap = init,
        !:Vartypes = init,
        map_foldl2(create_anon_var_with_type, Args, ArgVars, !Varmap,
            !Vartypes),
        EType = e_call(c_plain(CalleeId), ArgVars, resources(Uses, Observes)),
        !:CallInfo = code_info_init(nil_context),
        code_info_set_arity(arity(length(Returns)), !CallInfo),
        code_info_set_types(Returns, !CallInfo),
        Expr = expr(EType, !.CallInfo),
        func_set_body(!.Varmap, ArgVars, Expr, !Wrapper),
        func_set_vartypes(!.Vartypes, !Wrapper),
        core_set_function(WrapperId, !.Wrapper, !Core)
    ).

:- pred create_anon_var_with_type(type_::in, var::out,
    varmap::in, varmap::out, map(var, type_)::in, map(var, type_)::out) is det.

create_anon_var_with_type(Type, Var, !Varmap, !Vartypes) :-
    add_anon_var(Var, !Varmap),
    det_insert(Var, Type, !Vartypes).

%-----------------------------------------------------------------------%

:- pred make_proc_id_map(core::in, func_id::in,
    map(func_id, pzp_id)::in, map(func_id, pzp_id)::out,
    map(func_id, list(pz_instr))::in, map(func_id, list(pz_instr))::out,
    pz::in, pz::out) is det.

make_proc_id_map(Core, FuncId, !ProcMap, !OpMap, !PZ) :-
    core_get_function_det(Core, FuncId, Function),
    Name = q_name_to_string(func_get_name(Function)),
    ( if func_builtin_type(Function, BuiltinType) then
        ( BuiltinType = bit_core,
            make_proc_id_core_or_rts(FuncId, Function, !ProcMap, !PZ),
            ( if func_get_body(Function, _, _, _) then
                true
            else
                unexpected($file, $pred,
                    format("Builtin core function ('%s') has no body",
                        [s(Name)]))
            )
        ; BuiltinType = bit_inline_pz,
            ( if func_builtin_inline_pz(Function, PZInstrs) then
                det_insert(FuncId, PZInstrs, !OpMap)
            else
                unexpected($file, $pred, format(
                    "Inline PZ builtin ('%s') without list of instructions",
                    [s(Name)]))
            )
        ; BuiltinType = bit_rts,
            make_proc_id_core_or_rts(FuncId, Function, !ProcMap, !PZ),
            ( if
                not func_builtin_inline_pz(Function, _),
                not func_get_body(Function, _, _, _)
            then
                true
            else
                unexpected($file, $pred,
                    format("RTS builtin ('%s') with a body",
                        [s(Name)]))
            )
        )
    else
        make_proc_id_core_or_rts(FuncId, Function, !ProcMap, !PZ),
        ( if func_get_body(Function, _, _, _) then
            true
        else
            unexpected($file, $pred,
                format("Non builtin function ('%s') has no body", [s(Name)]))
        )
    ).

:- pred make_proc_id_core_or_rts(func_id::in, function::in,
    map(func_id, pzp_id)::in, map(func_id, pzp_id)::out,
    pz::in, pz::out) is det.

make_proc_id_core_or_rts(FuncId, Function, !Map, !PZ) :-
    Imported = func_get_imported(Function),
    pz_new_proc_id(Imported, ProcId, !PZ),
    det_insert(FuncId, ProcId, !Map).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
