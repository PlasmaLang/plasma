%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module core_to_pz.
%
% Copyright (C) 2015-2016 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
% Plasma core to pz conversion
%
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
:- import_module set.

:- import_module builtins.
:- import_module common_types.
:- import_module core.code.
:- import_module core.types.
:- import_module pz.code.
:- import_module q_name.
:- import_module string.
:- import_module util.
:- import_module varmap.

%-----------------------------------------------------------------------%

core_to_pz(Core, !:PZ) :-
    !:PZ = init_pz,
    FuncIds = core_all_functions(Core),
    foldl2(gen_const_data(Core), FuncIds, init, DataMap, !PZ),
    OpIdMap = builtin_operator_map(Core),
    RealFuncIds = to_sorted_list(set(FuncIds) `difference`
        set(keys(OpIdMap))),
    foldl2(make_proc_id_map(Core), RealFuncIds, init, ProcIdMap, !PZ),
    map(gen_proc(Core, OpIdMap, ProcIdMap, DataMap), RealFuncIds, Procs),
    foldl((pred((PID - P)::in, PZ0::in, PZ::out) is det :-
            pz_add_proc(PID, P, PZ0, PZ)
        ), Procs, !PZ),
    set_entry_function(Core, ProcIdMap, !PZ).

:- pred set_entry_function(core::in, map(func_id, pzp_id)::in,
    pz::in, pz::out) is det.

set_entry_function(Core, ProcIdMap, !PZ) :-
    MainName = q_name_snoc(module_name(Core), "main"),
    ( if core_search_function(Core, MainName, FuncId) then
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
    ( ExprType = e_let(_Vars, ExprA, ExprB),
        gen_const_data_expr(ExprA, !DataMap, !PZ),
        gen_const_data_expr(ExprB, !DataMap, !PZ)
    ; ExprType = e_tuple(Exprs),
        foldl2(gen_const_data_expr, Exprs, !DataMap, !PZ)
    ; ExprType = e_call(_, _)
    ; ExprType = e_var(_)
    ; ExprType = e_const(Const),
        ( Const = c_string(String),
            gen_const_data_string(String, !DataMap, !PZ)
        ; Const = c_number(_)
        ; Const = c_func(_)
        )
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

:- func builtin_operator_map(core) = map(func_id, list(pz_instr)).

builtin_operator_map(Core) = Map :-
    Operators = [builtin_add_int    - [pzi_add(pzow_fast)],
                 builtin_sub_int    - [pzi_sub(pzow_fast)],
                 builtin_mul_int    - [pzi_mul(pzow_fast)],
                 % Mod and div can maybe be combined into one operator, and
                 % optimised at PZ load time.
                 builtin_div_int    - [pzi_div(pzow_fast)],
                 builtin_mod_int    - [pzi_mod(pzow_fast)],
                 builtin_lshift_int - [pzi_trunc(pzow_fast, pzow_8),
                                       pzi_lshift(pzow_fast)],
                 builtin_rshift_int - [pzi_trunc(pzow_fast, pzow_8),
                                       pzi_rshift(pzow_fast)],
                 builtin_and_int    - [pzi_and(pzow_fast)],
                 builtin_or_int     - [pzi_or(pzow_fast)],
                 builtin_xor_int    - [pzi_xor(pzow_fast)],

                 % These are candidates for optimisation
                 builtin_minus_int  - [pzi_load_immediate(pzow_fast,
                                         immediate32(0)),
                                       pzi_roll(2),
                                       pzi_sub(pzow_fast)],
                                      % Until the runtime supports loading
                                      % data of any width (and sign
                                      % extending it, if necessary) we must
                                      % do that here.
                 builtin_comp_int   - [pzi_load_immediate(pzow_32,
                                        immediate32(0xFFFFFFFF)),
                                       pzi_se(pzow_32, pzow_fast),
                                       pzi_xor(pzow_fast)]
                ],
    foldl(make_builtin_operator_map(Core), Operators, init, Map).

:- pred make_builtin_operator_map(core::in, pair(q_name, list(pz_instr))::in,
    map(func_id, list(pz_instr))::in, map(func_id, list(pz_instr))::out)
    is det.

make_builtin_operator_map(Core, Name - Instr, !Map) :-
    q_name_append(builtin_module_name, Name, QName),
    core_lookup_function(Core, QName, FuncId),
    det_insert(FuncId, Instr, !Map).

%-----------------------------------------------------------------------%

:- pred gen_proc(core::in, map(func_id, list(pz_instr))::in,
    map(func_id, pzp_id)::in, map(const_data, pzd_id)::in, func_id::in,
    pair(pzp_id, pz_proc)::out) is det.

gen_proc(Core, OpIdMap, ProcIdMap, DataMap, FuncId, PID - Proc) :-
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
            CGInfo = code_gen_info(OpIdMap, ProcIdMap, DataMap, Varmap),
            gen_blocks(CGInfo, Inputs, BodyExpr, Blocks)
        else
            unexpected($file, $pred, format("No function body for %s",
                [s(q_name_to_string(Symbol))]))
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
    Varmap = CGInfo ^ cgi_varmap,
    initial_bind_map(Params, 0, Varmap, ParamDepthComments, map.init, BindMap),

    Depth = length(Params),
    gen_instrs(CGInfo, Expr, Depth, BindMap, Instrs0, [], Blocks0),

    % Pop Depth parameters off the stack, each parameter may be behind
    % values that we need to return.
    Arity = code_info_get_arity(Expr ^ e_info),
    FixupInstrs = fixup_stack(Depth, Arity ^ a_num),

    Instrs = ParamDepthComments ++ Instrs0 ++ FixupInstrs ++
        singleton(pzio_instr(pzi_ret)),
    Blocks = Blocks0 ++ [pz_block(list(Instrs))].

:- func fixup_stack(int, int) = cord(pz_instr_obj).

fixup_stack(BottomItems, Items) = Fixup :-
    Fixup0 = fixup_stack_2(BottomItems, Items),
    ( if is_empty(Fixup0) then
        Fixup = init
    else
        Fixup = cons(pzio_comment(
                format("fixup_stack(%d, %d)", [i(BottomItems), i(Items)])),
            map((func(I) = pzio_instr(I)), Fixup0))
    ).

:- func fixup_stack_2(int, int) = cord(pz_instr).

fixup_stack_2(BottomItems, Items) =
    ( if BottomItems = 0 then
        % There are no items underneath the items we want to return.
        init
    else if Items = 0 then
        % There are no items on the top, so we can just drop BottomItems.
        cord.from_list(condense(duplicate(BottomItems, [pzi_drop])))
    else
        cord.from_list([pzi_roll(BottomItems + Items), pzi_drop]) ++
            fixup_stack_2(BottomItems - 1, Items)
    ).

:- type code_gen_info
    --->    code_gen_info(
                cgi_op_id_map       :: map(func_id, list(pz_instr)),
                cgi_proc_id_map     :: map(func_id, pzp_id),
                cgi_data_map        :: map(const_data, pzd_id),
                cgi_varmap          :: varmap
            ).

:- pred gen_instrs(code_gen_info::in, expr::in, int::in,
    map(var, int)::in, cord(pz_instr_obj)::out,
    list(pz_block)::in, list(pz_block)::out) is det.

gen_instrs(CGInfo, Expr, Depth, BindMap, cons(DepthComment, Instrs),
        !Blocks) :-
    DepthComment = pzio_comment(format("Depth: %d", [i(Depth)])),

    Varmap = CGInfo ^ cgi_varmap,
    Expr = expr(ExprType, CodeInfo),
    ( ExprType = e_tuple(Exprs),
        gen_instrs_tuple(CGInfo, Exprs, Depth, BindMap, Instrs, !Blocks)
    ; ExprType = e_let(Vars, LetExpr, InExpr),
        % Run the instructions that will place these variables onto the
        % stack.
        CommentBegin = singleton(pzio_comment(
            format("Begin let at depth %d { ", [i(Depth)]))),
        gen_instrs(CGInfo, LetExpr, Depth, BindMap, InstrsLet, !Blocks),

        % Update the BindMap for the "In" part of the expression.  This
        % records the stack slot of each variable.
        insert_vars_depth(Vars, Depth, Varmap, CommentBinds,
            BindMap, InBindMap),

        % Run the "In" expression.
        LetArity = code_info_get_arity(LetExpr ^ e_info),
        InDepth = Depth + LetArity ^ a_num,
        gen_instrs(CGInfo, InExpr, InDepth, InBindMap, InstrsIn, !Blocks),

        % Fixup the stack.
        ExprArity = code_info_get_arity(CodeInfo),
        InstrsPop = fixup_stack(length(Vars), ExprArity ^ a_num),
        CommentEnd = singleton(pzio_comment(
            format("} End let at depth %d", [i(Depth)]))),

        Instrs = CommentBegin ++ InstrsLet ++ CommentBinds ++ InstrsIn ++
            InstrsPop ++ CommentEnd
    ; ExprType = e_call(Callee, Args),
        map_foldl((pred(V::in, I::out, D0::in, D::out) is det :-
                gen_var_access(BindMap, Varmap, V, D0, I),
                D = D0 + 1
            ), Args, InstrsArgs0, Depth, _),
        InstrsArgs = cord_list_to_cord(InstrsArgs0),

        ( if search(CGInfo ^ cgi_op_id_map, Callee, Instrs0P) then
            % The function is implemented with a short sequence of
            % instructions.
            Instrs0 = map((func(I) = pzio_instr(I)), Instrs0P)
        else
            lookup(CGInfo ^ cgi_proc_id_map, Callee, PID),
            Instrs0 = [pzio_instr(pzi_call(PID))]
        ),
        Instrs = InstrsArgs ++ cord.from_list(Instrs0)
    ; ExprType = e_var(Var),
        gen_var_access(BindMap, Varmap, Var, Depth, Instrs)
    ; ExprType = e_const(Const),
        ( Const = c_number(Num),
            Instrs = singleton(pzio_instr(
                pzi_load_immediate(pzow_fast, immediate32(Num))))
        ; Const = c_string(String),
            lookup(CGInfo ^ cgi_data_map, cd_string(String), DID),
            Instrs = singleton(pzio_instr(
                pzi_load_immediate(pzow_ptr, immediate_data(DID))))
        ; Const = c_func(_),
            sorry($pred, "function")
        )
    ).

:- pred gen_var_access(map(var, int)::in, varmap::in, var::in, int::in,
    cord(pz_instr_obj)::out) is det.

gen_var_access(BindMap, Varmap, Var, Depth, Instrs) :-
    lookup(BindMap, Var, VarDepth),
    RelDepth = Depth - VarDepth + 1,
    VarName = get_var_name(Varmap, Var),
    Instrs = from_list([pzio_comment(format("get var %s", [s(VarName)])),
        pzio_instr(pzi_pick(RelDepth))]).

:- pred gen_instrs_tuple(code_gen_info::in, list(expr)::in,
    int::in, map(var, int)::in, cord(pz_instr_obj)::out,
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

:- pred initial_bind_map(list(var)::in, int::in, varmap::in,
    cord(pz_instr_obj)::out, map(var, int)::in, map(var, int)::out) is det.

initial_bind_map(Vars, Depth, Varmap, Comments, !Map) :-
    insert_vars_depth(Vars, Depth, Varmap, Comments, !Map).

:- pred insert_vars_depth(list(var)::in, int::in, varmap::in,
    cord(pz_instr_obj)::out, map(var, int)::in, map(var, int)::out) is det.

insert_vars_depth([], _, _, init, !Map).
insert_vars_depth([Var | Vars], Depth0, Varmap, Comments, !Map) :-
    Depth = Depth0 + 1,
    det_insert(Var, Depth, !Map),
    Comment = pzio_comment(format("let: %s is at depth %d",
        [s(get_var_name(Varmap, Var)), i(Depth)])),
    insert_vars_depth(Vars, Depth, Varmap, Comments0, !Map),
    Comments = cons(Comment, Comments0).

