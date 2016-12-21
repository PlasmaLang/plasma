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

:- import_module map.

:- import_module builtins.
:- import_module core.
:- import_module pz.
:- import_module q_name.

%-----------------------------------------------------------------------%

:- pred core_to_pz(map(q_name, builtin_item)::in, core::in, pz::out) is det.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

:- import_module assoc_list.
:- import_module char.
:- import_module cord.
:- import_module int.
:- import_module list.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module set.

:- import_module common_types.
:- import_module core.code.
:- import_module core.function.
:- import_module core.types.
:- import_module pz.code.
:- import_module string.
:- import_module util.
:- import_module varmap.

%-----------------------------------------------------------------------%

core_to_pz(BuiltinMap, Core0, !:PZ) :-
    !:PZ = init_pz,
    FuncIds = core_all_functions(Core0),

    % Generate constants.
    foldl2(gen_const_data(Core0), FuncIds, init, DataMap, !PZ),

    % Make decisions about tagged pointers
    TypeIds = core_all_types(Core0),
    foldl(gen_type_tags, TypeIds, Core0, Core),

    % Generate functions.
    OpIdMap = builtin_operator_map(BuiltinMap),
    RealFuncIds = to_sorted_list(set(FuncIds) `difference`
        set(keys(OpIdMap))),
    setup_builtin_procs(BuiltinProcs, !PZ),
    foldl2(make_proc_id_map(Core), RealFuncIds, init, ProcIdMap, !PZ),
    map(gen_proc(Core, OpIdMap, ProcIdMap, BuiltinProcs, DataMap),
        RealFuncIds, Procs),
    foldl((pred((PID - P)::in, PZ0::in, PZ::out) is det :-
            pz_add_proc(PID, P, PZ0, PZ)
        ), Procs, !PZ),
    set_entry_function(Core, ProcIdMap, !PZ).

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
    ; ExprType = e_constant(Const),
        ( Const = c_string(String),
            gen_const_data_string(String, !DataMap, !PZ)
        ; Const = c_number(_)
        ; Const = c_func(_)
        ; Const = c_ctor(_)
        )
    ; ExprType = e_construction(_, _)
    ; ExprType = e_match(_, Cases),
        foldl2(gen_const_data_case, Cases, !DataMap, !PZ)
    ).

:- pred gen_const_data_case(expr_case::in,
    map(const_data, pzd_id)::in, map(const_data, pzd_id)::out,
    pz::in, pz::out) is det.

gen_const_data_case(e_case(_, Expr), !DataMap, !PZ) :-
    gen_const_data_expr(Expr, !DataMap, !PZ).

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
        Data = pz_data(type_array(pzw_8), pzv_sequence(Bytes)),
        pz_add_data(DID, Data, !PZ),
        det_insert(ConstData, DID, !DataMap)
    ).

%-----------------------------------------------------------------------%

:- pred gen_type_tags(type_id::in, core::in, core::out) is det.

gen_type_tags(TypeId, !Core) :-
    Type0 = core_get_type(!.Core, TypeId),
    type_setup_ctor_tags(!.Core, TypeId, Type0, Type),
    core_set_type(TypeId, Type, !Core).

%-----------------------------------------------------------------------%

:- func builtin_operator_map(map(q_name, builtin_item)) =
    map(func_id, list(pz_instr)).

builtin_operator_map(BuiltinMap) = Map :-
    Operators = [builtin_add_int    - [pzi_add(pzw_fast)],
                 builtin_sub_int    - [pzi_sub(pzw_fast)],
                 builtin_mul_int    - [pzi_mul(pzw_fast)],
                 % Mod and div can maybe be combined into one operator, and
                 % optimised at PZ load time.
                 builtin_div_int    - [pzi_div(pzw_fast)],
                 builtin_mod_int    - [pzi_mod(pzw_fast)],
                 builtin_lshift_int - [pzi_trunc(pzw_fast, pzw_8),
                                       pzi_lshift(pzw_fast)],
                 builtin_rshift_int - [pzi_trunc(pzw_fast, pzw_8),
                                       pzi_rshift(pzw_fast)],
                 builtin_and_int    - [pzi_and(pzw_fast)],
                 builtin_or_int     - [pzi_or(pzw_fast)],
                 builtin_xor_int    - [pzi_xor(pzw_fast)],

                 builtin_gt_int     - [pzi_gt_s(pzw_fast)],
                 builtin_lt_int     - [pzi_lt_s(pzw_fast)],
                 builtin_gteq_int   - [pzi_lt_s(pzw_fast),
                                       pzi_not(pzw_fast)],
                 builtin_lteq_int   - [pzi_gt_s(pzw_fast),
                                       pzi_not(pzw_fast)],
                 builtin_eq_int     - [pzi_eq(pzw_fast)],
                 builtin_neq_int    - [pzi_eq(pzw_fast),
                                       pzi_not(pzw_fast)],

                 builtin_and_bool   - [pzi_and(pzw_fast)],
                 builtin_or_bool    - [pzi_or(pzw_fast)],

                 builtin_not_bool   - [pzi_not(pzw_fast)],

                 % These are candidates for optimisation
                 builtin_minus_int  - [pzi_load_immediate(pzw_fast,
                                         immediate32(0)),
                                       pzi_roll(2),
                                       pzi_sub(pzw_fast)],
                                      % Until the runtime supports loading
                                      % data of any width (and sign
                                      % extending it, if necessary) we must
                                      % do that here.
                 builtin_comp_int   - [pzi_load_immediate(pzw_32,
                                        immediate32(0xFFFFFFFF)),
                                       pzi_se(pzw_32, pzw_fast),
                                       pzi_xor(pzw_fast)]
                ],
    foldl(make_builtin_operator_map(BuiltinMap), Operators, init, Map).

:- pred make_builtin_operator_map(map(q_name, builtin_item)::in,
    pair(q_name, list(pz_instr))::in,
    map(func_id, list(pz_instr))::in, map(func_id, list(pz_instr))::out)
    is det.

make_builtin_operator_map(Map, Name - Instr, !Map) :-
    lookup(Map, Name, Item),
    ( if Item = bi_func(FuncId) then
        det_insert(FuncId, Instr, !Map)
    else
        unexpected($file, $pred, "Builtin item is not a function")
    ).

%-----------------------------------------------------------------------%

:- pred gen_proc(core::in, map(func_id, list(pz_instr))::in,
    map(func_id, pzp_id)::in, builtin_procs::in, map(const_data, pzd_id)::in,
    func_id::in, pair(pzp_id, pz_proc)::out) is det.

gen_proc(Core, OpIdMap, ProcIdMap, BuiltinProcs, DataMap, FuncId,
        PID - Proc) :-
    lookup(ProcIdMap, FuncId, PID),
    core_get_function_det(Core, FuncId, Func),
    Symbol = func_get_name(Func),

    func_get_signature(Func, Input0, Output0, _),
    Input = map(type_to_pz_width, Input0),
    Output = map(type_to_pz_width, Output0),
    Signature = pz_signature(Input, Output),

    Imported = func_get_imported(Func),

    ( Imported = i_local,
        ( if
            func_get_body(Func, Varmap, Inputs, BodyExpr),
            func_get_vartypes(Func, Vartypes)
        then
            CGInfo = code_gen_info(Core, OpIdMap, ProcIdMap, BuiltinProcs,
                DataMap, Vartypes, Varmap),
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

:- func type_to_pz_width(type_) = pz_width.

type_to_pz_width(Type) = Width :-
    ( Type = builtin_type(BuiltinType),
        ( BuiltinType = int,
            Width = pzw_fast
        ; BuiltinType = string,
            Width = pzw_ptr
        )
    ;
        ( Type = type_variable(_)
        ; Type = type_ref(_)
        ),
        Width = pzw_ptr
    ).

:- pred gen_blocks(code_gen_info::in, list(var)::in, expr::in,
    list(pz_block)::out) is det.

gen_blocks(CGInfo, Params, Expr, Blocks) :-
    Varmap = CGInfo ^ cgi_varmap,
    some [!Instrs, !Blocks] (
        !:Blocks = pz_blocks(0, no, map.init),
        alloc_block(EntryBlockId, !Blocks),
        start_block(EntryBlockId, !Blocks),
        !:Instrs = cord.init,

        initial_bind_map(Params, 0, Varmap, ParamDepthComments, map.init,
            BindMap),
        add_instrs(ParamDepthComments, !Instrs),

        Depth = length(Params),
        gen_instrs(CGInfo, Expr, Depth, BindMap, !Instrs, !Blocks),

        % Pop Depth parameters off the stack, each parameter may be behind
        % values that we need to return.
        Arity = code_info_get_arity(Expr ^ e_info),
        add_instrs(fixup_stack(Depth, Arity ^ a_num), !Instrs),
        add_instr(pzio_instr(pzi_ret), !Instrs),

        % Finish block.
        finish_block(!.Instrs, !Blocks),
        Blocks = values(to_sorted_assoc_list(!.Blocks ^ pzb_blocks))
    ).

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
                cgi_core            :: core,
                cgi_op_id_map       :: map(func_id, list(pz_instr)),
                cgi_proc_id_map     :: map(func_id, pzp_id),
                cgi_builtin_procs   :: builtin_procs,
                cgi_data_map        :: map(const_data, pzd_id),
                cgi_type_map        :: map(var, type_),
                cgi_varmap          :: varmap
            ).

:- pred gen_instrs(code_gen_info::in, expr::in, int::in,
    map(var, int)::in, cord(pz_instr_obj)::in, cord(pz_instr_obj)::out,
    pz_blocks::in, pz_blocks::out) is det.

gen_instrs(CGInfo, Expr, Depth, BindMap, !Instrs, !Blocks) :-
    Varmap = CGInfo ^ cgi_varmap,
    Expr = expr(ExprType, CodeInfo),
    ( ExprType = e_tuple(Exprs),
        gen_instrs_tuple(CGInfo, Exprs, Depth, BindMap, !Instrs, !Blocks)
    ; ExprType = e_let(Vars, LetExpr, InExpr),
        % Run the instructions that will place these variables onto the
        % stack.
        add_instr(
            pzio_comment(format("Begin let at depth %d { ", [i(Depth)])),
            !Instrs),

        gen_instrs(CGInfo, LetExpr, Depth, BindMap, !Instrs, !Blocks),

        % Update the BindMap for the "In" part of the expression.  This
        % records the stack slot of each variable.
        insert_vars_depth(Vars, Depth, Varmap, CommentBinds,
            BindMap, InBindMap),
        add_instrs(CommentBinds, !Instrs),

        % Run the "In" expression.
        LetArity = code_info_get_arity(LetExpr ^ e_info),
        InDepth = Depth + LetArity ^ a_num,
        gen_instrs(CGInfo, InExpr, InDepth, InBindMap, !Instrs, !Blocks),

        % Fixup the stack.
        ExprArity = code_info_get_arity(CodeInfo),
        add_instrs(fixup_stack(length(Vars), ExprArity ^ a_num), !Instrs),
        add_instr(pzio_comment(format("} End let at depth %d", [i(Depth)])),
            !Instrs)
    ; ExprType = e_call(Callee, Args),
        gen_instrs_args(BindMap, Varmap, Args, InstrsArgs, Depth, _),

        ( if search(CGInfo ^ cgi_op_id_map, Callee, Instrs0P) then
            % The function is implemented with a short sequence of
            % instructions.
            Instrs0 = map((func(I) = pzio_instr(I)), Instrs0P)
        else
            lookup(CGInfo ^ cgi_proc_id_map, Callee, PID),
            Instrs0 = [pzio_instr(pzi_call(PID))]
        ),
        add_instrs(InstrsArgs ++ cord.from_list(Instrs0), !Instrs)
    ; ExprType = e_var(Var),
        add_instrs(gen_var_access(BindMap, Varmap, Var, Depth), !Instrs)
    ; ExprType = e_constant(Const),
        ( Const = c_number(Num),
            Instrs = singleton(pzio_instr(
                pzi_load_immediate(pzw_fast, immediate32(Num))))
        ; Const = c_string(String),
            lookup(CGInfo ^ cgi_data_map, cd_string(String), DID),
            Instrs = singleton(pzio_instr(
                pzi_load_immediate(pzw_ptr, immediate_data(DID))))
        ;
            ( Const = c_func(_)
            ; Const = c_ctor(_)
            ),
            util.sorry($file, $pred, "Higher order value")
        ),
        add_instrs(Instrs, !Instrs)
    ; ExprType = e_construction(CtorId, Args),
        TypeId = one_item(code_info_get_types(CodeInfo)),
        gen_instrs_args(BindMap, Varmap, Args, ArgsInstrs, Depth, _),
        !:Instrs = !.Instrs ++ ArgsInstrs ++
            gen_construction(CGInfo, TypeId, CtorId)
    ; ExprType = e_match(Var, Cases),
        add_instr(pzio_comment(format("Switch at depth %d", [i(Depth)])),
            !Instrs),
        alloc_block(ContinueId, !Blocks),
        lookup(CGInfo ^ cgi_type_map, Var, VarType),
        add_instrs(gen_var_access(BindMap, Varmap, Var, Depth), !Instrs),
        foldl2(gen_instrs_case(CGInfo, Depth+1, BindMap, ContinueId, VarType),
            Cases, !Instrs, !Blocks),
        % We can assume that the cases are exhaustive, there's no need to
        % generate match-all code.  A transformation on the core
        % representation will ensure this.
        % TODO: Continue after each branch for non-returning cases.
        finish_block(!.Instrs, !Blocks),
        start_block(ContinueId, !Blocks),
        !:Instrs = cord.init,
        add_instr(depth_comment_instr(Depth), !Instrs)
    ).

    % The body of each case is placed in a new block.
    %
    % + If the pattern matches then we jump to that block, that block itself
    %   will execute the expression then jump to the contiuation block.
    % + Otherwise fall-through and try the next case.
    %
    % No indexing, jump tables or other optimisations are
    % implemented.
    %
:- pred gen_instrs_case(code_gen_info::in, int::in, map(var, int)::in,
    int::in, type_::in, expr_case::in,
    cord(pz_instr_obj)::in, cord(pz_instr_obj)::out,
    pz_blocks::in, pz_blocks::out) is det.

gen_instrs_case(CGInfo, !.Depth, BindMap0, ContinueId, VarType,
        e_case(Pattern, Expr), !Instrs, !Blocks) :-
    ContinueDepth = !.Depth - 1,
    alloc_block(BlockNum, !Blocks),

    % Create the code that attempts to match the pattern and jump into the
    % new block.
    gen_instrs_case_match(CGInfo, Pattern, BlockNum, VarType, !Depth,
        !Instrs),
    push_block(PrevBlockId, !Blocks),
    PrevInstrs = !.Instrs,

    start_block(BlockNum, !Blocks),
    !:Instrs = cord.init,
    % The variable that we're switching on is on the top of the stack, and
    % we can use it directly.  But we need to put it back when we're done,
    % unless the jump is unconditional.

    add_instr(depth_comment_instr(!.Depth), !Instrs),
    % At the start of the new block we place code that will provide any
    % variables bound by the matching pattern.
    gen_deconstruction(CGInfo, Pattern, VarType, BindMap0, BindMap, !Depth,
        !Instrs),
    % Generate the body of the new block.
    gen_instrs(CGInfo, Expr, !.Depth, BindMap, !Instrs, !Blocks),

    % Fixup the stack.
    ExprArity = code_info_get_arity(Expr ^ e_info),
    add_instrs(fixup_stack(!.Depth - ContinueDepth, ExprArity ^ a_num),
        !Instrs),
    !:Depth = ContinueDepth,

    % After executing the body, jump to the continuation block.
    add_instrs_list([
        depth_comment_instr(!.Depth),
        pzio_instr(pzi_jmp(ContinueId))], !Instrs),
    finish_block(!.Instrs, !Blocks),
    pop_block(PrevBlockId, !Blocks),
    !:Instrs = PrevInstrs.

:- pred gen_instrs_case_match(code_gen_info::in, expr_pattern::in, int::in,
    type_::in, int::in, int::out,
    cord(pz_instr_obj)::in, cord(pz_instr_obj)::out) is det.

gen_instrs_case_match(_, p_num(Num), BlockNum, _, !Depth,
        !Instrs) :-
    add_instrs_list([
        pzio_comment(format("Case for %d", [i(Num)])),
        % Save the switched-on value for the next case.
        pzio_instr(pzi_pick(1)),
        % Compare Num with TOS and jump if equal.
        pzio_instr(pzi_load_immediate(pzw_fast, immediate32(Num))),
        pzio_instr(pzi_eq(pzw_fast)),
        depth_comment_instr(!.Depth + 1),
        pzio_instr(pzi_cjmp(BlockNum, pzw_fast))], !Instrs).
gen_instrs_case_match(_, p_variable(_), BlockNum, _, !Depth,
        !Instrs) :-
    add_instrs_list([
        pzio_comment("Case match all and bind variable"),
        depth_comment_instr(!.Depth),
        pzio_instr(pzi_jmp(BlockNum))], !Instrs).
gen_instrs_case_match(_, p_wildcard, BlockNum, _, !Depth,
        !Instrs) :-
    add_instrs_list([
        pzio_comment("Case match wildcard"),
        depth_comment_instr(!.Depth),
        pzio_instr(pzi_jmp(BlockNum))], !Instrs).
gen_instrs_case_match(CGInfo, p_ctor(CtorId, Args), BlockNum, VarType, !Depth,
        !Instrs) :-
    SetupInstrs = from_list([
        pzio_comment("Case match deconstruction"),
        depth_comment_instr(!.Depth),
        % Save the switched-on value for the next case.
        pzio_instr(pzi_pick(1))]),
    ( VarType = type_ref(TypeId),
        MatchInstrs = gen_match_ctor(CGInfo, TypeId, CtorId)
    ;
        ( VarType = builtin_type(_)
        ; VarType = type_variable(_)
        ),
        unexpected($file, $pred,
            "Deconstructions must be on user types")
    ),
    ( Args = [],
        JmpInstrs = from_list([
            depth_comment_instr(!.Depth + 1),
            pzio_instr(pzi_cjmp(BlockNum, pzw_fast))]),
        add_instrs(SetupInstrs ++ MatchInstrs ++ JmpInstrs, !Instrs)
    ; Args = [_ | _],
        util.sorry($file, $pred, "Construction pattern with arguments")
    ).

    % Generate code that attempts to match a data constructor.  It has the
    % stack usage (ptr - w) the input is a copy of the value to switch on,
    % the output is a boolean suitable for "cjmp".
    %
    % TODO: This could be made a call to a pz procedure.  Making it a call
    % (outline) matches the pz style, letting the interpreter do the
    % inlining.  It may also make seperate compilation simpler.
    %
:- func gen_match_ctor(code_gen_info, type_id, ctor_id) = cord(pz_instr_obj).

gen_match_ctor(CGInfo, TypeId, CtorId) = Instrs :-
    TagInfo = get_type_ctor_info(CGInfo, TypeId, CtorId),
    ( TagInfo = ti_constant(PTag, WordBits),
        ShiftMakeTagId = CGInfo ^ cgi_builtin_procs ^ bp_shift_make_tag,
        Instrs = from_list([
            % Compare tagged value with TOS and jump if equal.
            pzio_instr(pzi_load_immediate(pzw_ptr, immediate32(WordBits))),
            pzio_instr(pzi_load_immediate(pzw_ptr, immediate32(PTag))),
            pzio_instr(pzi_call(ShiftMakeTagId)),
            pzio_instr(pzi_eq(pzw_ptr))])
    ; TagInfo = ti_constant_notag(Word),
        Instrs = from_list([
            % Compare constant value with TOS and jump if equal.
            pzio_instr(pzi_load_immediate(pzw_ptr, immediate32(Word))),
            pzio_instr(pzi_eq(pzw_ptr))])
    ; TagInfo = ti_tagged_pointer(_),
        util.sorry($file, $pred, "Tagged pointer")
    ).

:- pred gen_deconstruction(code_gen_info::in, expr_pattern::in, type_::in,
    map(var, int)::in, map(var, int)::out, int::in, int::out,
    cord(pz_instr_obj)::in, cord(pz_instr_obj)::out) is det.

gen_deconstruction(_, p_num(_), _, !BindMap, !Depth, !Instrs) :-
    % Drop the switched on variable when entering the branch.
    add_instr(pzio_instr(pzi_drop), !Instrs),
    !:Depth = !.Depth - 1.
gen_deconstruction(_, p_wildcard, _, !BindMap, !Depth, !Instrs) :-
    % Drop the switched on variable when entering the branch.
    add_instr(pzio_instr(pzi_drop), !Instrs),
    !:Depth = !.Depth - 1.
gen_deconstruction(CGInfo, p_variable(Var), _, !BindMap, !Depth,
        !Instrs) :-
    Varmap = CGInfo ^ cgi_varmap,
    % Leave the value on the stack and update the bind map so that the
    % expression can find it.
    % NOTE: This call expects the depth where the variable begins.
    insert_vars_depth([Var], !.Depth - 1, Varmap, CommentBinds,
        !BindMap),
    add_instrs(CommentBinds, !Instrs).
gen_deconstruction(CGInfo, p_ctor(CtorId, Args), VarType, !BindMap, !Depth,
        !Instrs) :-
    (
        VarType = type_ref(TypeId),
        ( Args = [],
            % Discard the value, it doesn't bind any variables.
            add_instr(pzio_instr(pzi_drop), !Instrs),
            !:Depth = !.Depth - 1
        ; Args = [_ | _],
            % TODO: Optimisation, only read the variables that are used in
            % the body.  Further optimisation could leave some on the heap,
            % avoiding stack usage.
            core_get_constructor_det(CGInfo ^ cgi_core, TypeId, CtorId,
                Ctor),
            Varmap = CGInfo ^ cgi_varmap,
            foldl4_corresponding(gen_decon_field(Varmap), Args,
                Ctor ^ c_fields, 1, _, !BindMap, !Depth, !Instrs)
        )
    ;
        ( VarType = builtin_type(_)
        ; VarType = type_variable(_)
        ),
        unexpected($file, $pred,
            "Deconstructions must be on user types")
    ).

:- pred gen_decon_field(varmap::in, var::in, type_field::in, int::in, int::out,
    map(var, int)::in, map(var, int)::out, int::in, int::out,
    cord(pz_instr_obj)::in, cord(pz_instr_obj)::out) is det.

gen_decon_field(Varmap, Var, _Field, !FieldNo, !BindMap, !Depth, !Instrs) :-
    !:Depth = !.Depth + 1,
    add_instrs_list([
        pzio_comment(format("reading field %d", [i(!.FieldNo)])),
        % XXX Read field!
        pzio_comment(format("%s is at depth %d",
            [s(get_var_name(Varmap, Var)), i(!.Depth)]))
        ], !Instrs),
    !:FieldNo = !.FieldNo + 1,

    % Update the BindMap
    det_insert(Var, !.Depth, !BindMap).

%-----------------------------------------------------------------------%

:- func get_type_ctor_info(code_gen_info, type_id, ctor_id) = ctor_tag_info.

get_type_ctor_info(CGInfo, TypeId, CtorId) =
    type_get_ctor_tag(core_get_type(CGInfo ^ cgi_core, TypeId),
        CtorId).

%-----------------------------------------------------------------------%

:- func gen_construction(code_gen_info, type_, ctor_id) =
    cord(pz_instr_obj).

gen_construction(CGInfo, Type, CtorId) = Instrs :-
    ( Type = builtin_type(_),
        unexpected($file, $pred, "No builtin types are constructed with
        e_construction")
    ; Type = type_variable(_),
        unexpected($file, $pred, "Polymorphic values are never constructed")
    ; Type = type_ref(TypeId),
        TagInfo = get_type_ctor_info(CGInfo, TypeId, CtorId),

        % TODO Move the construction out-of-line into a seperate procedure,
        % this is also used when the constructor is used as a higher order
        % value.  It may be later inlined.
        ( TagInfo = ti_constant(PTag, WordBits),
            ShiftMakeTag = CGInfo ^ cgi_builtin_procs ^ bp_shift_make_tag,
            Instrs = from_list([pzio_comment("Construct tagged constant"),
                pzio_instr(pzi_load_immediate(pzw_ptr,
                    immediate32(WordBits))),
                pzio_instr(pzi_load_immediate(pzw_ptr,
                    immediate32(PTag))),
                pzio_instr(pzi_call(ShiftMakeTag))])
        ; TagInfo = ti_constant_notag(Word),
            Instrs = from_list([pzio_comment("Construct constant"),
                pzio_instr(pzi_load_immediate(pzw_ptr, immediate32(Word)))])
        ; TagInfo = ti_tagged_pointer(_),
            util.sorry($file, $pred, "Allocating memory not supported")
        )
    ).

%-----------------------------------------------------------------------%

:- func depth_comment_instr(int) = pz_instr_obj.

depth_comment_instr(Depth) = pzio_comment(format("Depth: %d", [i(Depth)])).

:- func gen_var_access(map(var, int), varmap, var, int) = cord(pz_instr_obj).

gen_var_access(BindMap, Varmap, Var, Depth) = Instrs :-
    lookup(BindMap, Var, VarDepth),
    RelDepth = Depth - VarDepth + 1,
    VarName = get_var_name(Varmap, Var),
    Instrs = from_list([pzio_comment(format("get var %s", [s(VarName)])),
        pzio_instr(pzi_pick(RelDepth))]).

:- pred gen_instrs_args(map(var, int)::in, varmap::in,
    list(var)::in, cord(pz_instr_obj)::out, int::in, int::out) is det.

gen_instrs_args(BindMap, Varmap, Args, InstrsArgs, !Depth) :-
    map_foldl((pred(V::in, I::out, D0::in, D::out) is det :-
            I = gen_var_access(BindMap, Varmap, V, D0),
            D = D0 + 1
        ), Args, InstrsArgs0, !Depth),
    InstrsArgs = cord_list_to_cord(InstrsArgs0).

:- pred gen_instrs_tuple(code_gen_info::in, list(expr)::in,
    int::in, map(var, int)::in, cord(pz_instr_obj)::in, cord(pz_instr_obj)::out,
    pz_blocks::in, pz_blocks::out) is det.

gen_instrs_tuple(_, [], _, _, !Instrs, !Blocks).
gen_instrs_tuple(CGInfo, [Arg | Args], Depth, BindMap, !Instrs, !Blocks) :-
    % BindMap does not change in a list of arguments because arguments
    % do not affect one-another's environment.
    gen_instrs(CGInfo, Arg, Depth, BindMap, !Instrs, !Blocks),
    Arity = code_info_get_arity(Arg ^ e_info),
    ( if Arity ^ a_num \= 1 then
        % Type checking should have already rejected this.
        unexpected($file, $pred, "Bad expression arity used in argument")
    else
        true
    ),
    gen_instrs_tuple(CGInfo, Args, Depth + Arity ^ a_num, BindMap,
        !Instrs, !Blocks).

%-----------------------------------------------------------------------%

:- pred add_instr(T::in, cord(T)::in, cord(T)::out) is det.

add_instr(Instr, Instrs, snoc(Instrs, Instr)).

:- pred add_instrs(cord(T)::in, cord(T)::in, cord(T)::out) is det.

add_instrs(NewInstrs, Instrs, Instrs ++ NewInstrs).

:- pred add_instrs_list(list(T)::in, cord(T)::in, cord(T)::out) is det.

add_instrs_list(List, !Instrs) :-
    add_instrs(cord.from_list(List), !Instrs).

:- type pz_blocks
    --->    pz_blocks(
                pzb_next_block  :: int,
                pzb_cur_block   :: maybe(int),
                pzb_blocks      :: map(int, pz_block)
            ).

:- pred alloc_block(int::out, pz_blocks::in, pz_blocks::out) is det.

alloc_block(BlockId, !Blocks) :-
    BlockId = !.Blocks ^ pzb_next_block,
    !Blocks ^ pzb_next_block := BlockId + 1.

:- pred start_block(int::in, pz_blocks::in, pz_blocks::out) is det.

start_block(BlockId, !Blocks) :-
    OldCurBlock = !.Blocks ^ pzb_cur_block,
    ( OldCurBlock = no,
        !Blocks ^ pzb_cur_block := yes(BlockId)
    ; OldCurBlock = yes(_),
        unexpected($file, $pred, "Already active block")
    ).

:- pred finish_block(cord(pz_instr_obj)::in, pz_blocks::in, pz_blocks::out)
    is det.

finish_block(Instrs, !Blocks) :-
    Block = pz_block(cord.list(Instrs)),
    BlockMap0 = !.Blocks ^ pzb_blocks,
    MaybeBlockId = !.Blocks ^ pzb_cur_block,
    ( MaybeBlockId = yes(BlockId),
        det_insert(BlockId, Block, BlockMap0, BlockMap),
        !Blocks ^ pzb_blocks := BlockMap,
        !Blocks ^ pzb_cur_block := no
    ; MaybeBlockId = no,
        unexpected($file, $pred, "No active block")
    ).

:- pred push_block(int::out, pz_blocks::in, pz_blocks::out) is det.

push_block(BlockId, !Blocks) :-
    MaybeBlockId = !.Blocks ^ pzb_cur_block,
    ( MaybeBlockId = yes(BlockId),
        !Blocks ^ pzb_cur_block := no
    ; MaybeBlockId = no,
        unexpected($file, $pred, "No active block")
    ).

:- pred pop_block(int::in, pz_blocks::in, pz_blocks::out) is det.

pop_block(BlockId, !Blocks) :-
    MaybeBlockId = !.Blocks ^ pzb_cur_block,
    ( MaybeBlockId = no,
        !Blocks ^ pzb_cur_block := yes(BlockId)
    ; MaybeBlockId = yes(_),
        unexpected($file, $pred, "Already active block")
    ).

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
    Comment = pzio_comment(format("%s is at depth %d",
        [s(get_var_name(Varmap, Var)), i(Depth)])),
    insert_vars_depth(Vars, Depth, Varmap, Comments0, !Map),
    Comments = cons(Comment, Comments0).

