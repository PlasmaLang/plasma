%-----------------------------------------------------------------------%
% Plasma core to pz conversion
% vim: ts=4 sw=4 et
%
% Copyright (C) 2015 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
% This program compiles plasma modules.
%
%-----------------------------------------------------------------------%
:- module core_to_pz.
%-----------------------------------------------------------------------%

:- interface.

:- import_module core.
:- import_module pz.

%-----------------------------------------------------------------------%

:- pred core_to_pz(core::in, pz::out) is det.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module cord.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.

:- import_module common_types.
:- import_module core.code.
:- import_module core.types.
:- import_module pz.code.
:- import_module string.
:- import_module symtab.
:- import_module varmap.


%-----------------------------------------------------------------------%

core_to_pz(Core, !:PZ) :-
    !:PZ = init_pz,
    FuncIds = core_all_functions(Core),
    foldl2(gen_const_data(Core), FuncIds, init, DataMap, !PZ),
    foldl2(make_proc_id_map(Core), FuncIds, init, ProcIdMap, !PZ),
    map(gen_proc(Core, ProcIdMap, DataMap), FuncIds, Procs),
    foldl((pred((PID - P)::in, PZ0::in, PZ::out) is det :-
            pz_add_proc(PID, P, PZ0, PZ)
        ), Procs, !PZ),
    set_entry_function(Core, ProcIdMap, !PZ).

:- pred set_entry_function(core::in, map(func_id, pzp_id)::in,
    pz::in, pz::out) is det.

set_entry_function(Core, ProcIdMap, !PZ) :-
    MainName = symbol_append(module_name(Core), "main"),
    ( if core_lookup_function(Core, MainName, FuncId) then
        lookup(ProcIdMap, FuncId, PID),
        pz_set_entry_proc(PID, !PZ)
    else
        true
    ).

%-----------------------------------------------------------------------%

:- pred make_proc_id_map(core::in, func_id::in,
    map(func_id, pzp_id)::in, map(func_id, pzp_id)::out,
    pz::in, pz::out) is det.

make_proc_id_map(Core, FuncId, !Map, !PZ) :-
    core_get_function_det(Core, FuncId, Function),
    Imported = func_get_imported(Function),
    pz_new_proc_id(Imported, ProcId, !PZ),
    det_insert(FuncId, ProcId, !Map).

:- type const_data
    --->    cd_string(string).

:- pred gen_const_data(core::in, func_id::in,
    map(const_data, pzd_id)::in, map(const_data, pzd_id)::out,
    pz::in, pz::out) is det.

gen_const_data(Core, FuncId, !DataMap, !PZ) :-
    core_get_function_det(Core, FuncId, Func),
    ( if func_get_body(Func, _, _, Expr) then
        gen_const_data_expr(Expr, !DataMap, !PZ)
    else
        true
    ).

:- pred gen_const_data_expr(expr::in,
    map(const_data, pzd_id)::in, map(const_data, pzd_id)::out,
    pz::in, pz::out) is det.

gen_const_data_expr(expr(ExprType, _), !DataMap, !PZ) :-
    ( ExprType = e_sequence(Exprs),
        foldl2(gen_const_data_expr, Exprs, !DataMap, !PZ)
    ; ExprType = e_call(_, Exprs),
        foldl2(gen_const_data_expr, Exprs, !DataMap, !PZ)
    ; ExprType = e_var(_)
    ; ExprType = e_const(Const),
        ( Const = c_string(String),
            gen_const_data_string(String, !DataMap, !PZ)
        ; Const = c_number(_)
        )
    ; ExprType = e_func(_)
    ).

:- pred gen_const_data_string(string::in,
    map(const_data, pzd_id)::in, map(const_data, pzd_id)::out,
    pz::in, pz::out) is det.

gen_const_data_string(String, !DataMap, !PZ) :-
    ConstData = cd_string(String),
    ( if search(!.DataMap, ConstData, _) then
        true
    else
        pz_new_data_id(DID, !PZ),
        % XXX: currently ASCII.
        Bytes = map(to_int, to_char_list(String)) ++ [0],
        Data = pz_data(type_array(w8), pzv_sequence(Bytes)),
        pz_add_data(DID, Data, !PZ),
        det_insert(ConstData, DID, !DataMap)
    ).

%-----------------------------------------------------------------------%

:- pred gen_proc(core::in, map(func_id, pzp_id)::in,
    map(const_data, pzd_id)::in, func_id::in,
    pair(pzp_id, pz_proc)::out) is det.

gen_proc(Core, ProcIdMap, DataMap, FuncId, PID - Proc) :-
    lookup(ProcIdMap, FuncId, PID),
    core_get_function_det(Core, FuncId, Func),
    core_lookup_function_name(Core, FuncId, Symbol),

    func_get_signature(Func, Input0, Output0, _),
    Input = map(type_to_pz_width, Input0),
    Output = map(type_to_pz_width, Output0),
    Signature = pz_signature(Input, Output),

    Imported = func_get_imported(Func),

    ( Imported = i_local,
        ( if func_get_body(Func, Varmap, Inputs, BodyExpr) then
            CGInfo = code_gen_info(ProcIdMap, DataMap, Varmap),
            gen_blocks(CGInfo, Inputs, BodyExpr, Blocks)
        else
            unexpected($file, $pred, format("No function body for %s",
                [s(symbol_to_string(Symbol))]))
        ),
        MaybeBlocks = yes(Blocks)
    ; Imported = i_imported,
        MaybeBlocks = no
    ),

    Proc = pz_proc(Symbol, Signature, MaybeBlocks).

:- func type_to_pz_width(type_) = pz_data_width.

% XXX: fix for GD.
type_to_pz_width(Type) = Width :-
    ( Type = builtin_type(BuiltinType),
        ( BuiltinType = int,
            Width = w_fast
        ; BuiltinType = string,
            Width = ptr
        )
    ; Type = type_variable(_),
        unexpected($file, $pred, "unimplemeted polymorphism")
    ; Type = type_(_, Args),
        ( Args = [],
            Width = w_ptr
        ; Args = [_ | _],
            Width = ptr
        )
    ).

:- pred gen_blocks(code_gen_info::in, list(var)::in, expr::in,
    list(pz_block)::out) is det.

gen_blocks(CGInfo, Params, Expr, Blocks) :-
    initial_bind_map(Params, 0, map.init, BindMap),
    Depth = length(Params),
    % XXX: Check end depth.
    gen_blocks_2(CGInfo, Expr, Depth, EndDepth, BindMap, _,
        cord.init, Instrs0, [], Blocks0),
    % Pop Depth parameters off the stack, each parameter may be behind
    % EndDepth - Depth return values.
    Instrs = Instrs0 ++
        cord.from_list(condense(duplicate(Depth,
                [pzi_roll(EndDepth - Depth), pzi_drop]))) ++
        singleton(pzi_ret),
    Blocks = Blocks0 ++ [pz_block(list(Instrs))].

:- type code_gen_info
    --->    code_gen_info(
                cgi_proc_id_map     :: map(func_id, pzp_id),
                cgi_data_map        :: map(const_data, pzd_id),
                cgi_varmap          :: varmap
            ).

:- pred gen_blocks_2(code_gen_info::in, expr::in,
    int::in, int::out,
    map(var, int)::in, map(var, int)::out,
    cord(pz_instr)::in, cord(pz_instr)::out,
    list(pz_block)::in, list(pz_block)::out) is det.

gen_blocks_2(CGInfo, Expr, !Depth, !BindMap, !Instrs, !Blocks) :-
    Expr = expr(ExprType, _CodeInfo),
    ( ExprType = e_sequence(Exprs),
        foldl4(gen_blocks_2(CGInfo), Exprs, !Depth, !BindMap, !Instrs,
            !Blocks)
    ; ExprType = e_call(Callee, Args),
        foldl2(gen_instrs(CGInfo, !.BindMap), Args, !Depth, !Instrs),
        ProcIdMap = CGInfo ^ cgi_proc_id_map,
        lookup(ProcIdMap, Callee, PID),
        !:Instrs = !.Instrs ++ singleton(pzi_call(PID)),
        !:Depth = !.Depth - length(Args) + 2
    ; ExprType = e_var(Var),
        lookup(!.BindMap, Var, VarDepth),
        RelDepth = !.Depth - VarDepth + 1,
        !:Instrs = !.Instrs ++ singleton(pzi_pick(RelDepth)),
        !:Depth = !.Depth + 1
    ; ExprType = e_const(Const),
        ( Const = c_number(Num),
            !:Instrs = !.Instrs ++
                singleton(pzi_load_immediate(pzow_fast, immediate32(Num))),
            !:Depth = !.Depth + 1
        ; Const = c_string(String),
            lookup(CGInfo ^ cgi_data_map, cd_string(String), DID),
            !:Instrs = !.Instrs ++
                singleton(pzi_load_immediate(pzow_ptr,
                    immediate_data(DID)))
        )
    ; ExprType = e_func(_),
        sorry($pred, "function")
    ).

:- pred gen_instrs(code_gen_info::in, map(var, int)::in, expr::in,
    int::in, int::out, cord(pz_instr)::in, cord(pz_instr)::out) is det.

gen_instrs(CGInfo, BindMap, Expr, !Depth, !Instrs) :-
    gen_blocks_2(CGInfo, Expr, !Depth, BindMap, _, !Instrs, [], Blocks),
    ( Blocks = [],
        true
    ; Blocks = [_ | _],
        unexpected($file, $pred, "gen_instrs generated blocks")
    ).

%-----------------------------------------------------------------------%

:- pred initial_bind_map(list(var)::in, int::in,
    map(var, int)::in, map(var, int)::out) is det.

initial_bind_map([], _, !Map).
initial_bind_map([Var | Vars], Depth0, !Map) :-
    Depth = Depth0 + 1,
    det_insert(Var, Depth, !Map),
    initial_bind_map(Vars, Depth, !Map).

