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
    ; ExprType = e_tuple(Exprs),
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
    gen_instrs(CGInfo, Expr, Depth, BindMap, Instrs0, [], Blocks0),
    Arity = code_info_get_arity(Expr ^ e_info),
    % Pop Depth parameters off the stack, each parameter may be behind
    % values that we need to return.
    Instrs = Instrs0 ++ fixup_stack(Depth, Arity ^ a_num) ++
        singleton(pzi_ret),
    Blocks = Blocks0 ++ [pz_block(list(Instrs))].

:- func fixup_stack(int, int) = cord(pz_instr).

fixup_stack(BottomItems, Items) =
    ( if BottomItems = 0 then
        % There are no items underneath the items we want to return.
        init
    else if Items = 0 then
        % There are no items on the top, so we can just drop BottomItems.
        cord.from_list(condense(duplicate(BottomItems, [pzi_drop])))
    else
        cord.from_list([pzi_roll(BottomItems + Items), pzi_drop]) ++
            fixup_stack(BottomItems - 1, Items)
    ).

:- type code_gen_info
    --->    code_gen_info(
                cgi_proc_id_map     :: map(func_id, pzp_id),
                cgi_data_map        :: map(const_data, pzd_id),
                cgi_varmap          :: varmap
            ).

:- pred gen_instrs(code_gen_info::in, expr::in, int::in,
    map(var, int)::in, cord(pz_instr)::out,
    list(pz_block)::in, list(pz_block)::out) is det.

gen_instrs(CGInfo, Expr, Depth, BindMap, Instrs, !Blocks) :-
    Expr = expr(ExprType, CodeInfo),
    ( ExprType = e_sequence(Exprs),
        Arity = code_info_get_arity(CodeInfo),
        gen_instrs_sequence(CGInfo, Exprs, Depth, BindMap, Arity, Instrs,
            !Blocks)
    ; ExprType = e_tuple(Exprs),
        gen_instrs_tuple(CGInfo, Exprs, Depth, BindMap, Instrs, !Blocks)
    ; ExprType = e_call(Callee, Args),
        gen_instrs_tuple(CGInfo, Args, Depth, BindMap, InstrsArgs, !Blocks),
        ProcIdMap = CGInfo ^ cgi_proc_id_map,
        lookup(ProcIdMap, Callee, PID),
        Instrs = InstrsArgs ++ singleton(pzi_call(PID))
    ; ExprType = e_var(Var),
        lookup(BindMap, Var, VarDepth),
        RelDepth = Depth - VarDepth + 1,
        Instrs = singleton(pzi_pick(RelDepth))
    ; ExprType = e_const(Const),
        ( Const = c_number(Num),
            Instrs =
                singleton(pzi_load_immediate(pzow_fast, immediate32(Num)))
        ; Const = c_string(String),
            lookup(CGInfo ^ cgi_data_map, cd_string(String), DID),
            Instrs = singleton(pzi_load_immediate(pzow_ptr,
                    immediate_data(DID)))
        )
    ; ExprType = e_func(_),
        sorry($pred, "function")
    ).

:- pred gen_instrs_sequence(code_gen_info::in, list(expr)::in,
    int::in, map(var, int)::in, arity::in, cord(pz_instr)::out,
    list(pz_block)::in, list(pz_block)::out) is det.

gen_instrs_sequence(_, [], _, _, _, cord.init, !Blocks).
gen_instrs_sequence(CGInfo, [Expr | Exprs], Depth, BindMap, Arity, Instrs,
        !Blocks) :-
    % XXX: An assignment may update bind map.
    gen_instrs(CGInfo, Expr, Depth, BindMap, InstrsExpr, !Blocks),
    ExprArity = code_info_get_arity(Expr ^ e_info),
    gen_instrs_sequence(CGInfo, Exprs, Depth + ExprArity ^ a_num, BindMap,
        Arity, InstrsExprs, !Blocks),
    ( Exprs = [_ | _],
        % Remove Expr's items from the stack as the result of a sequence is
        % the last item in the sequence, and this is not the last.
        % We remove it after the computing Exprs as Exprs may have picked
        % Expr's value (if Expr was an assignment).
        PopInstrs = fixup_stack(ExprArity ^ a_num, Arity ^ a_num)
    ; Exprs = [],
        PopInstrs = init
    ),
    Instrs = InstrsExpr ++ InstrsExprs ++ PopInstrs.

:- pred gen_instrs_tuple(code_gen_info::in, list(expr)::in,
    int::in, map(var, int)::in, cord(pz_instr)::out,
    list(pz_block)::in, list(pz_block)::out) is det.

gen_instrs_tuple(_, [], _, _, cord.init, !Blocks).
gen_instrs_tuple(CGInfo, [Arg | Args], Depth, BindMap, Instrs, !Blocks) :-
    % BindMap does not change in a list of arguments because arguments
    % do not affect one-another's environment.
    gen_instrs(CGInfo, Arg, Depth, BindMap, InstrsArg, !Blocks),
    Arity = code_info_get_arity(Arg ^ e_info),
    ( if Arity ^ a_num \= 1 then
        % Type checking should have already rejected this.
        unexpected($file, $pred, "Bad expression arity used in argument")
    else
        true
    ),
    gen_instrs_tuple(CGInfo, Args, Depth + Arity ^ a_num, BindMap,
        InstrsArgs, !Blocks),
    Instrs = InstrsArg ++ InstrsArgs.

%-----------------------------------------------------------------------%

:- pred initial_bind_map(list(var)::in, int::in,
    map(var, int)::in, map(var, int)::out) is det.

initial_bind_map([], _, !Map).
initial_bind_map([Var | Vars], Depth0, !Map) :-
    Depth = Depth0 + 1,
    det_insert(Var, Depth, !Map),
    initial_bind_map(Vars, Depth, !Map).

