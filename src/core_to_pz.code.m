%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module core_to_pz.code.
%
% Copyright (C) 2015-2017 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
% Plasma core to pz conversion - data layout decisions
%
%-----------------------------------------------------------------------%
:- interface.

:- import_module core.
:- import_module pz.

%-----------------------------------------------------------------------%

:- pred gen_proc(core::in, map(func_id, list(pz_instr))::in,
    map(func_id, pzp_id)::in, pz_builtin_ids::in,
    map(type_id, type_tag_info)::in,
    map({type_id, ctor_id}, constructor_data)::in, map(const_data, pzd_id)::in,
    func_id::in, pair(pzp_id, pz_proc)::out) is det.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
:- implementation.

:- import_module assoc_list.
:- import_module cord.
:- import_module string.
:- import_module int.

:- import_module core.code.
:- import_module core.pretty.
:- import_module util.

%-----------------------------------------------------------------------%

gen_proc(Core, OpIdMap, ProcIdMap, BuiltinProcs, TypeTagInfo,
        TypeCtorTagInfo, DataMap, FuncId, PID - Proc) :-
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
                TypeTagInfo, TypeCtorTagInfo, DataMap, Vartypes, Varmap),
            gen_proc_body(CGInfo, Inputs, BodyExpr, Blocks)
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
        ; Type = type_ref(_, _)
        ),
        Width = pzw_ptr
    ).

:- pred gen_proc_body(code_gen_info::in, list(var)::in, expr::in,
    list(pz_block)::out) is det.

gen_proc_body(CGInfo, Params, Expr, Blocks) :-
    Varmap = CGInfo ^ cgi_varmap,
    some [!Blocks] (
        !:Blocks = pz_blocks(0, map.init),
        alloc_block(EntryBlockId, !Blocks),

        initial_bind_map(Params, 0, Varmap, ParamDepthComments, map.init,
            BindMap),

        Depth = length(Params),
        gen_instrs(CGInfo, Expr, Depth, BindMap, cont_return, ExprInstrs,
            !Blocks),

        % Finish block.
        create_block(EntryBlockId, ParamDepthComments ++ ExprInstrs, !Blocks),
        Blocks = values(to_sorted_assoc_list(!.Blocks ^ pzb_blocks))
    ).

%-----------------------------------------------------------------------%

:- func fixup_stack(int, int) = cord(pz_instr_obj).

fixup_stack(BottomItems, Items) = Fixup :-
    ( if BottomItems < 0 ; Items < 0 then
        unexpected($file, $pred,
            format("fixup_stack(%d, %d)", [i(BottomItems), i(Items)]))
    else
        Fixup0 = fixup_stack_2(BottomItems, Items),
        ( if is_empty(Fixup0) then
            Fixup = init
        else
            Fixup = cons(pzio_comment(
                    format("fixup_stack(%d, %d)", [i(BottomItems), i(Items)])),
                map((func(I) = pzio_instr(I)), Fixup0))
        )
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

%-----------------------------------------------------------------------%

:- type code_gen_info
    --->    code_gen_info(
                cgi_core            :: core,
                cgi_op_id_map       :: map(func_id, list(pz_instr)),
                cgi_proc_id_map     :: map(func_id, pzp_id),
                cgi_builtin_ids     :: pz_builtin_ids,
                cgi_type_tags       :: map(type_id, type_tag_info),
                cgi_type_ctor_tags  :: map({type_id, ctor_id},
                                            constructor_data),
                cgi_data_map        :: map(const_data, pzd_id),
                cgi_type_map        :: map(var, type_),
                cgi_varmap          :: varmap
            ).

    % gen_instrs(Info, Expr, Depth, BindMap, Cont, Instrs, !Blocks).
    %
    % Generate instructions (Instrs) for an expression (Expr) and it's
    % continuation (Cont).  The continuation is important for handling
    % returns, switches and lets.  It represents what to execute after this
    % expression.
    %
:- pred gen_instrs(code_gen_info::in, expr::in, int::in,
    map(var, int)::in, continuation::in, cord(pz_instr_obj)::out,
    pz_blocks::in, pz_blocks::out) is det.

gen_instrs(CGInfo, Expr, Depth, BindMap, Continuation, Instrs, !Blocks) :-
    Varmap = CGInfo ^ cgi_varmap,
    Expr = expr(ExprType, CodeInfo),
    (
        ( ExprType = e_var(Var),
            InstrsMain = gen_var_access(BindMap, Varmap, Var, Depth)
        ; ExprType = e_call(CalleeID, Args),
            Core = CGInfo ^ cgi_core,
            core_get_function_det(Core, CalleeID, Callee),
            Decl = func_call_pretty(Core, Callee, Varmap, Args),
            CallComment = singleton(pzio_comment(append_list(list(Decl)))),

            gen_instrs_args(BindMap, Varmap, Args, InstrsArgs, Depth, _),

            ( if search(CGInfo ^ cgi_op_id_map, CalleeID, Instrs0P) then
                % The function is implemented with a short sequence of
                % instructions.
                Instrs0 = map((func(I) = pzio_instr(I)), Instrs0P)
            else
                lookup(CGInfo ^ cgi_proc_id_map, CalleeID, PID),
                Instrs0 = [pzio_instr(pzi_call(PID))]
            ),
            InstrsMain = CallComment ++ InstrsArgs ++ cord.from_list(Instrs0)
        ; ExprType = e_constant(Const),
            ( Const = c_number(Num),
                InstrsMain = singleton(pzio_instr(
                    pzi_load_immediate(pzw_fast, immediate32(Num))))
            ; Const = c_string(String),
                lookup(CGInfo ^ cgi_data_map, cd_string(String), DID),
                InstrsMain = singleton(pzio_instr(
                    pzi_load_immediate(pzw_ptr, immediate_data(DID))))
            ;
                ( Const = c_func(_)
                ; Const = c_ctor(_)
                ),
                util.sorry($file, $pred, "Higher order value")
            )
        ; ExprType = e_construction(CtorId, Args),
            TypeId = one_item(code_info_get_types(CodeInfo)),
            gen_instrs_args(BindMap, Varmap, Args, ArgsInstrs, Depth, _),
            InstrsMain = ArgsInstrs ++
                gen_construction(CGInfo, TypeId, CtorId)
        ),
        Arity = code_info_get_arity(CodeInfo),
        InstrsCont = gen_continuation(Continuation, Depth, Arity ^ a_num),
        Instrs = InstrsMain ++ InstrsCont
    ; ExprType = e_tuple(Exprs),
        gen_instrs_tuple(CGInfo, Exprs, Depth, BindMap, Continuation,
            Instrs, !Blocks)
    ; ExprType = e_let(Vars, LetExpr, InExpr),
        gen_instrs_let(CGInfo, Vars, LetExpr, InExpr, Depth, BindMap,
            Continuation, Instrs, !Blocks)
    ; ExprType = e_match(Var, Cases),
        gen_instrs_match(CGInfo, Var, Cases, Depth, BindMap,
            Continuation, Instrs, !Blocks)
    ).

%-----------------------------------------------------------------------%

:- pred gen_instrs_tuple(code_gen_info::in, list(expr)::in,
    int::in, map(var, int)::in, continuation::in, cord(pz_instr_obj)::out,
    pz_blocks::in, pz_blocks::out) is det.

gen_instrs_tuple(_, [], Depth, _, Continuation, Instrs, !Blocks) :-
    Instrs = gen_continuation(Continuation, Depth, 0).
gen_instrs_tuple(CGInfo, [Arg | Args], Depth, BindMap, Continue, Instrs,
        !Blocks) :-
    ( Args = [],
        gen_instrs(CGInfo, Arg, Depth, BindMap, Continue, Instrs, !Blocks)
    ; Args = [_ | _],
        % BindMap does not change in a list of arguments because arguments
        % do not affect one-another's environment.
        Arity = code_info_get_arity(Arg ^ e_info),
        ( if Arity ^ a_num \= 1 then
            % Type checking should have already rejected this.
            unexpected($file, $pred, "Bad expression arity used in argument")
        else
            true
        ),
        RestDepth = Depth + Arity ^ a_num,
        gen_instrs_tuple(CGInfo, Args, RestDepth, BindMap,
            Continue, InstrsArgs, !Blocks),
        ArgsContinue = cont_instrs(RestDepth, InstrsArgs),

        gen_instrs(CGInfo, Arg, Depth, BindMap, ArgsContinue,
            Instrs, !Blocks)
    ).

%-----------------------------------------------------------------------%

:- pred gen_instrs_let(code_gen_info::in, list(var)::in, expr::in, expr::in,
    int::in, map(var, int)::in, continuation::in, cord(pz_instr_obj)::out,
    pz_blocks::in, pz_blocks::out) is det.

gen_instrs_let(CGInfo, Vars, LetExpr, InExpr, Depth, BindMap, Continuation0,
        Instrs, !Blocks) :-
    % Generate the instructions for the "In" part (the continuation of the
    % "Let" part).
    % Update the BindMap for the "In" part of the expression.  This
    % records the stack slot that we expect to find each variable.
    Varmap = CGInfo ^ cgi_varmap,
    insert_vars_depth(Vars, Depth, Varmap, CommentBinds,
        BindMap, InBindMap),

    % Run the "In" expression.
    Continuation = cont_comment("}", Continuation0),
    LetArity = code_info_get_arity(LetExpr ^ e_info),
    InDepth = Depth + LetArity ^ a_num,
    gen_instrs(CGInfo, InExpr, InDepth, InBindMap, Continuation,
        InInstrs, !Blocks),
    InContinuation = cont_comment("} in {", cont_instrs(InDepth,
        CommentBinds ++ InInstrs)),

    % Generate the instructions for the "let" part, using the "in" part as
    % the continuation.
    gen_instrs(CGInfo, LetExpr, Depth, BindMap, InContinuation, Instrs0,
        !Blocks),
    Instrs = cons(pzio_comment(format("let at depth %d { ", [i(Depth)])),
        Instrs0).

%-----------------------------------------------------------------------%

:- pred gen_instrs_match(code_gen_info::in, var::in, list(expr_case)::in,
    int::in, map(var, int)::in, continuation::in, cord(pz_instr_obj)::out,
    pz_blocks::in, pz_blocks::out) is det.

gen_instrs_match(CGInfo, Var, Cases, Depth, BindMap,
        Continuation, Instrs, !Blocks) :-
    % We can assume that the cases are exhaustive, there's no need to
    % generate match-all code.  A transformation on the core
    % representation will ensure this.

    % First, generate the bodies of each case.
    continuation_make_block(Continuation, BranchContinuation, !Blocks),
    list.foldl3(gen_instrs_case(CGInfo, Depth+1, BindMap, BranchContinuation,
            VarType),
        Cases, 1, _, map.init, CaseInstrMap, !Blocks),

    % Determine the type of switch requred.  These are:
    %   Casecading.
    %   Switch on primary tag (plus value or secondary tag)
    %
    % Later there may be more eg: to support efficient string matching.  Or
    % add computed gotos.
    %
    % Nested matches, or multiple patterns per case will need to be added
    % later, what we do WRT switch type will need to be reconsidered.
    %
    lookup(CGInfo ^ cgi_type_map, Var, VarType),
    SwitchType = var_type_switch_type(CGInfo, VarType),

    % Generate the switch, using the bodies generated above.
    BeginComment = pzio_comment(format("Switch at depth %d", [i(Depth)])),
    Varmap = CGInfo ^ cgi_varmap,
    GetVarInstrs = gen_var_access(BindMap, Varmap, Var, Depth),
    ( SwitchType = enum,
        TestsInstrs = gen_test_and_jump_enum(CGInfo, CaseInstrMap,
            VarType, Depth, Cases, 1)
    ; SwitchType = tags(_TypeId, TagInfo),
        gen_test_and_jump_tags(CGInfo, CaseInstrMap, TagInfo, Cases,
            TestsInstrs, !Blocks)
    ),
    Instrs = cord.singleton(BeginComment) ++ GetVarInstrs ++ TestsInstrs.

:- type switch_type
    --->    enum
    ;       tags(type_id, map(int, type_ptag_info)).

:- func var_type_switch_type(code_gen_info, type_) = switch_type.

var_type_switch_type(_, builtin_type(Builtin)) = SwitchType :-
    ( Builtin = int,
        % This is really stupid, but it'll do for now.
        SwitchType = enum
    ; Builtin = string,
        util.sorry($file, $pred, "Cannot switch on strings")
    ).
var_type_switch_type(_, type_variable(_)) =
    unexpected($file, $pred, "Switch types must be concrete").
var_type_switch_type(CGInfo, type_ref(TypeId, _)) = SwitchType :-
    map.lookup(CGInfo ^ cgi_type_tags, TypeId, TagInfo),
    ( TagInfo = tti_untagged,
        SwitchType = enum
    ; TagInfo = tti_tagged(PTagInfos),
        SwitchType = tags(TypeId, PTagInfos)
    ).

    % The body of each case is placed in a new block.
    %
    % + If the pattern matches then we jump to that block, that block itself
    %   will execute the expression then execute the continuation.
    % + Otherwise fall-through and try the next case.
    %
    % No indexing, jump tables or other optimisations are
    % implemented.
    %
:- pred gen_instrs_case(code_gen_info::in, int::in, map(var, int)::in,
    continuation::in, type_::in, expr_case::in, int::in, int::out,
    map(int, int)::in, map(int, int)::out,
    pz_blocks::in, pz_blocks::out) is det.

gen_instrs_case(CGInfo, !.Depth, BindMap0, Continue, VarType,
        e_case(Pattern, Expr), !CaseNum, !InstrMap, !Blocks) :-
    alloc_block(BlockNum, !Blocks),
    det_insert(!.CaseNum, BlockNum, !InstrMap),
    !:CaseNum = !.CaseNum + 1,

    DepthCommentBeforeDecon = depth_comment_instr(!.Depth),
    % At the start of the new block we place code that will provide any
    % variables bound by the matching pattern.
    gen_deconstruction(CGInfo, Pattern, VarType, BindMap0, BindMap, !Depth,
        InstrsDecon),
    % Generate the body of the new block.
    gen_instrs(CGInfo, Expr, !.Depth, BindMap, Continue, InstrsBranchBody,
        !Blocks),
    InstrsBranch = singleton(DepthCommentBeforeDecon) ++ InstrsDecon ++
        InstrsBranchBody,

    create_block(BlockNum, InstrsBranch, !Blocks).

:- func gen_test_and_jump_enum(code_gen_info, map(int, int), type_,
    int, list(expr_case), int) = cord(pz_instr_obj).

gen_test_and_jump_enum(_, _, _, _, [], _) = cord.init.
gen_test_and_jump_enum(CGInfo, BlockMap, Type, Depth, [Case | Cases], CaseNum)
        = InstrsCase ++ InstrsCases :-
    e_case(Pattern, _) = Case,
    lookup(BlockMap, CaseNum, BlockNum),
    InstrsCase = gen_instrs_case_match_enum(CGInfo, Pattern, Type, BlockNum,
        Depth),
    InstrsCases = gen_test_and_jump_enum(CGInfo, BlockMap, Type, Depth,
        Cases, CaseNum + 1).

    % The variable that we're switching on is on the top of the stack, and
    % we can use it directly.  But we need to put it back when we're done.
    %
:- func gen_instrs_case_match_enum(code_gen_info, expr_pattern, type_,
    int, int) = cord(pz_instr_obj).

gen_instrs_case_match_enum(_, p_num(Num), _, BlockNum, Depth) =
    cord.from_list([
        pzio_comment(format("Case for %d", [i(Num)])),
        % Save the switched-on value for the next case.
        pzio_instr(pzi_pick(1)),
        % Compare Num with TOS and jump if equal.
        pzio_instr(pzi_load_immediate(pzw_fast, immediate32(Num))),
        pzio_instr(pzi_eq(pzw_fast)),
        depth_comment_instr(Depth + 1),
        pzio_instr(pzi_cjmp(BlockNum, pzw_fast))]).
gen_instrs_case_match_enum(_, p_variable(_), _, BlockNum, Depth) =
    cord.from_list([
        pzio_comment("Case match all and bind variable"),
        depth_comment_instr(Depth),
        pzio_instr(pzi_jmp(BlockNum))]).
gen_instrs_case_match_enum(_, p_wildcard, _, BlockNum, Depth) =
    cord.from_list([
        pzio_comment("Case match wildcard"),
        depth_comment_instr(Depth),
        pzio_instr(pzi_jmp(BlockNum))]).
gen_instrs_case_match_enum(CGInfo, p_ctor(CtorId, _), VarType, BlockNum,
        Depth) = SetupInstrs ++ MatchInstrs ++ JmpInstrs :-
    SetupInstrs = from_list([
        pzio_comment("Case match deconstruction"),
        depth_comment_instr(Depth),
        % Save the switched-on value for the next case.
        pzio_instr(pzi_pick(1))]),
    ( VarType = type_ref(TypeId, _),
        MatchInstrs = gen_match_ctor(CGInfo, TypeId, CtorId)
    ;
        ( VarType = builtin_type(_)
        ; VarType = type_variable(_)
        ),
        unexpected($file, $pred,
            "Deconstructions must be on user types")
    ),
    JmpInstrs = from_list([
        depth_comment_instr(Depth + 1),
        pzio_instr(pzi_cjmp(BlockNum, pzw_fast))]).

:- pred gen_test_and_jump_tags(code_gen_info::in, map(int, int)::in,
    map(int, type_ptag_info)::in, list(expr_case)::in,
    cord(pz_instr_obj)::out, pz_blocks::in, pz_blocks::out) is det.

gen_test_and_jump_tags(CGInfo, BlockMap, PTagInfos, Cases, Instrs,
        !Blocks) :-
    % Get the ptag onto the TOS.
    BreakTagId = CGInfo ^ cgi_builtin_ids ^ pbi_break_tag,
    GetPtagInstrs = cord.from_list([
        pzio_comment("Break the tag, leaving the ptag on the TOS"),
        pzio_instr(pzi_pick(1)),
        pzio_instr(pzi_call(BreakTagId))
    ]),

    % For every primary tag, test it, and jump to the case it maps to,
    % if there is none, jump to the default cease.
    foldl2(gen_test_and_jump_ptag(CGInfo, BlockMap, Cases),
        PTagInfos, GetPtagInstrs, Instrs, !Blocks).

:- pred gen_test_and_jump_ptag(code_gen_info::in, map(int, int)::in,
    list(expr_case)::in, int::in, type_ptag_info::in,
    cord(pz_instr_obj)::in, cord(pz_instr_obj)::out,
    pz_blocks::in, pz_blocks::out) is det.

gen_test_and_jump_ptag(CGInfo, BlockMap, Cases, PTag, PTagInfo, !Instrs,
        !Blocks) :-
    alloc_block(Next, !Blocks),
    Instrs = from_list([
        pzio_instr(pzi_pick(1)),
        pzio_instr(pzi_load_immediate(pzw_ptr, immediate32(PTag))),
        pzio_instr(pzi_eq(pzw_ptr)),
        pzio_instr(pzi_cjmp(Next, pzw_fast))
    ]),
    ( PTagInfo = tpti_constant(EnumMap),
        UnshiftValueId =
            CGInfo ^ cgi_builtin_ids ^ pbi_unshift_value,
        GetFieldInstrs = from_list([
            pzio_comment("Drop the primary tag,"),
            pzio_instr(pzi_drop),
            pzio_comment("Unshift the tagged value."),
            pzio_instr(pzi_call(UnshiftValueId))
        ]),
        map_foldl(gen_test_and_jump_ptag_const(BlockMap, Cases),
            to_assoc_list(EnumMap), NextInstrsList, !Blocks),
        NextInstrs = GetFieldInstrs ++ cord_list_to_cord(NextInstrsList),
        create_block(Next, NextInstrs, !Blocks)
    ; PTagInfo = tpti_pointer(CtorId),
        % Drop the the saved copy of the tag and value off the stack.
        % Depending on the code we jump to there we may need to recreate the
        % value.  We could optimise this _a lot_ better.
        DropInstrs = from_list([
            pzio_comment("Drop the tag and value then jump"),
            pzio_instr(pzi_drop),
            pzio_instr(pzi_drop),
            pzio_instr(pzi_jmp(Dest))]),
        create_block(Next, DropInstrs, !Blocks),

        find_matching_case(Cases, 1, CtorId, _MatchParams, _Expr, CaseNum),
        lookup(BlockMap, CaseNum, Dest)
    ; PTagInfo = tpti_pointer_stag(STagMap),
        STagStruct = CGInfo ^ cgi_builtin_ids ^ pbi_stag_struct,
        GetStagInstrs = from_list([
            pzio_comment("Drop the primary tag"),
            pzio_instr(pzi_drop),
            pzio_comment("The pointer is on TOS, get the stag from it"),
            pzio_instr(pzi_load(STagStruct, 1, pzw_fast)),
            pzio_instr(pzi_drop)
        ]),
        map_foldl(gen_test_and_jump_ptag_stag(BlockMap, Cases),
            to_assoc_list(STagMap), SwitchStagInstrs, !Blocks),
        NextInstrs = GetStagInstrs ++ cord_list_to_cord(SwitchStagInstrs),
        create_block(Next, NextInstrs, !Blocks)
    ),
    !:Instrs = !.Instrs ++ Instrs.

:- pred gen_test_and_jump_ptag_const(map(int, int)::in, list(expr_case)::in,
    pair(int, ctor_id)::in, cord(pz_instr_obj)::out,
    pz_blocks::in, pz_blocks::out) is det.

gen_test_and_jump_ptag_const(BlockMap, Cases, ConstVal - CtorId, Instrs,
        !Blocks) :-
    alloc_block(Drop, !Blocks),

    Instrs = from_list([
        pzio_instr(pzi_pick(1)),
        pzio_instr(pzi_load_immediate(pzw_ptr, immediate32(ConstVal))),
        pzio_instr(pzi_eq(pzw_ptr)),
        pzio_instr(pzi_cjmp(Drop, pzw_fast))
    ]),

    find_matching_case(Cases, 1, CtorId, _MatchParams, _Expr, CaseNum),
    lookup(BlockMap, CaseNum, Dest),
    DropInstrs = from_list([
        pzio_comment("Drop the secondary tag then jump"),
        pzio_instr(pzi_drop),
        pzio_instr(pzi_jmp(Dest))]),
    create_block(Drop, DropInstrs, !Blocks).

:- pred gen_test_and_jump_ptag_stag(map(int, int)::in, list(expr_case)::in,
    pair(int, ctor_id)::in, cord(pz_instr_obj)::out,
    pz_blocks::in, pz_blocks::out) is det.

gen_test_and_jump_ptag_stag(BlockMap, Cases, STag - CtorId, Instrs,
        !Blocks) :-
    alloc_block(Drop, !Blocks),

    Instrs = from_list([
        pzio_instr(pzi_pick(1)),
        pzio_instr(pzi_load_immediate(pzw_fast, immediate32(STag))),
        pzio_instr(pzi_eq(pzw_fast)),
        pzio_instr(pzi_cjmp(Drop, pzw_fast))
    ]),

    find_matching_case(Cases, 1, CtorId, _MatchParams, _Expr, CaseNum),
    lookup(BlockMap, CaseNum, Dest),
    DropInstrs = from_list([
        pzio_comment("Drop the value then jump"),
        pzio_instr(pzi_drop),
        pzio_instr(pzi_jmp(Dest))]),
    create_block(Drop, DropInstrs, !Blocks).

:- pred find_matching_case(list(expr_case)::in, int::in, ctor_id::in,
    list(var)::out, expr::out, int::out) is det.

find_matching_case([], _, _, _, _, _) :-
    unexpected($file, $pred, "Case not found").
find_matching_case([Case | Cases], ThisCaseNum, CtorId, Vars, Expr, CaseNum) :-
    Case = e_case(Pattern, Expr0),
    ( Pattern = p_num(_),
        unexpected($file, $pred,
            "Type error: A number can't match a constructor")
    ; Pattern = p_variable(_),
        util.sorry($file, $pred, "How to set vars"),
        Vars = [],
        Expr = Expr0,
        CaseNum = ThisCaseNum
    ; Pattern = p_wildcard,
        Vars = [],
        Expr = Expr0,
        CaseNum = ThisCaseNum
    ; Pattern = p_ctor(ThisCtorId, ThisVars),
        ( if CtorId = ThisCtorId then
            Vars = ThisVars,
            Expr = Expr0,
            CaseNum = ThisCaseNum
        else
            find_matching_case(Cases, ThisCaseNum + 1, CtorId, Vars, Expr,
                CaseNum)
        )
    ).

    % Generate code that attempts to match a data constructor.  It has the
    % stack usage (ptr - w) the input is a copy of the value to switch on,
    % the output is a boolean suitable for "cjmp".
    %
    % TODO: This could be made a call to a pz procedure.  Making it a call
    % (outline) matches the pz style, letting the interpreter do the
    % inlining.  It may also make separate compilation simpler.
    %
:- func gen_match_ctor(code_gen_info, type_id, ctor_id) = cord(pz_instr_obj).

gen_match_ctor(CGInfo, TypeId, CtorId) = Instrs :-
    map.lookup(CGInfo ^ cgi_type_ctor_tags, {TypeId, CtorId}, CtorData),
    TagInfo = CtorData ^ cd_tag_info,
    ( TagInfo = ti_constant(PTag, WordBits),
        ShiftMakeTagId = CGInfo ^ cgi_builtin_ids ^ pbi_shift_make_tag,
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
    ; TagInfo = ti_tagged_pointer(PTag, _, MaybeSTag),
        % TODO: This is currently unused.
        ( MaybeSTag = no,
            BreakTagId = CGInfo ^ cgi_builtin_ids ^ pbi_break_tag,
            % TODO rather than dropping the pointer we should save it and use it
            % for deconstruction later.
            Instrs = from_list([
                pzio_instr(pzi_call(BreakTagId)),
                pzio_instr(pzi_roll(2)),
                pzio_instr(pzi_drop),
                pzio_instr(pzi_load_immediate(pzw_ptr, immediate32(PTag))),
                pzio_instr(pzi_eq(pzw_ptr))])
        ; MaybeSTag = yes(_),
            util.sorry($file, $pred, "Secondary tags")
        )
    ).

:- pred gen_deconstruction(code_gen_info::in, expr_pattern::in, type_::in,
    map(var, int)::in, map(var, int)::out, int::in, int::out,
    cord(pz_instr_obj)::out) is det.

gen_deconstruction(_, p_num(_), _, !BindMap, !Depth, Instrs) :-
    % Drop the switched on variable when entering the branch.
    Instrs = singleton(pzio_instr(pzi_drop)),
    !:Depth = !.Depth - 1.
gen_deconstruction(_, p_wildcard, _, !BindMap, !Depth, Instrs) :-
    % Drop the switched on variable when entering the branch.
    Instrs = singleton(pzio_instr(pzi_drop)),
    !:Depth = !.Depth - 1.
gen_deconstruction(CGInfo, p_variable(Var), _, !BindMap, !Depth,
        Instrs) :-
    Varmap = CGInfo ^ cgi_varmap,
    % Leave the value on the stack and update the bind map so that the
    % expression can find it.
    % NOTE: This call expects the depth where the variable begins.
    insert_vars_depth([Var], !.Depth - 1, Varmap, Instrs,
        !BindMap).
gen_deconstruction(CGInfo, p_ctor(CtorId, Args), VarType, !BindMap, !Depth,
        Instrs) :-
    (
        VarType = type_ref(TypeId, _),
        map.lookup(CGInfo ^ cgi_type_ctor_tags, {TypeId, CtorId}, CtorData),
        TagInfo = CtorData ^ cd_tag_info,
        (
            ( TagInfo = ti_constant(_, _)
            ; TagInfo = ti_constant_notag(_)
            ),
            % Discard the value, it doesn't bind any variables.
            Instrs = singleton(pzio_instr(pzi_drop)),
            !:Depth = !.Depth - 1
        ; TagInfo = ti_tagged_pointer(_, StructId, MaybeSTag),
            ( MaybeSTag = no,
                FirstField = 1
            ; MaybeSTag = yes(_),
                FirstField = 2
            ),

            % Untag the pointer, TODO: skip this if it's known that the tag
            % is zero.
            BreakTag = CGInfo ^ cgi_builtin_ids ^ pbi_break_tag,
            InstrsUntag = cord.from_list([
                    pzio_comment("Untag pointer and deconstruct"),
                    pzio_instr(pzi_call(BreakTag)),
                    pzio_instr(pzi_drop)
                ]),

            % TODO: Optimisation, only read the variables that are used in
            % the body.  Further optimisation could leave some on the heap,
            % avoiding stack usage.
            core_get_constructor_det(CGInfo ^ cgi_core, TypeId, CtorId,
                Ctor),
            Varmap = CGInfo ^ cgi_varmap,
            gen_decon_fields(Varmap, StructId, Args, Ctor ^ c_fields,
                FirstField, InstrsDeconstruct, !BindMap, !Depth),
            InstrDrop = pzio_instr(pzi_drop),
            Instrs = InstrsUntag ++ InstrsDeconstruct ++ singleton(InstrDrop),
            !:Depth = !.Depth - 1
        )
    ;
        ( VarType = builtin_type(_)
        ; VarType = type_variable(_)
        ),
        unexpected($file, $pred,
            "Deconstructions must be on user types")
    ).

:- pred gen_decon_fields(varmap::in, pzs_id::in,
    list(var)::in, list(type_field)::in, int::in, cord(pz_instr_obj)::out,
    map(var, int)::in, map(var, int)::out, int::in, int::out) is det.

gen_decon_fields(_,      _,        [],           [],               _,
        init, !BindMap, !Depth).
gen_decon_fields(_,      _,        [],           [_ | _],          _,
        _,    !BindMap, !Depth) :-
    unexpected($file, $pred, "Mismatched arg/field lists").
gen_decon_fields(_,      _,        [_ | _],      [],               _,
        _,    !BindMap, !Depth) :-
    unexpected($file, $pred, "Mismatched arg/field lists").
gen_decon_fields(Varmap, StructId, [Arg | Args], [Field | Fields], FieldNo,
        InstrsField ++ InstrsFields, !BindMap, !Depth) :-
    gen_decon_field(Varmap, StructId, Arg, Field, FieldNo,
        InstrsField,  !BindMap, !Depth),
    gen_decon_fields(Varmap, StructId, Args, Fields, FieldNo + 1,
        InstrsFields, !BindMap, !Depth).

:- pred gen_decon_field(varmap::in, pzs_id::in, var::in, type_field::in,
    int::in, cord(pz_instr_obj)::out, map(var, int)::in, map(var, int)::out,
    int::in, int::out) is det.

gen_decon_field(Varmap, StructId, Var, _Field, FieldNo, Instrs, !BindMap,
        !Depth) :-
    Instrs = cord.from_list([
        pzio_comment(format("reading field %d", [i(FieldNo)])),
        pzio_instr(pzi_load(StructId, FieldNo, pzw_ptr)),
        pzio_comment(format("%s is at depth %d",
            [s(get_var_name(Varmap, Var)), i(!.Depth)]))
        ]),

    % Update the BindMap
    det_insert(Var, !.Depth, !BindMap),

    % Load is (ptr - * ptr) so we increment the depth here.
    !:Depth = !.Depth + 1.

%-----------------------------------------------------------------------%

:- func gen_construction(code_gen_info, type_, ctor_id) =
    cord(pz_instr_obj).

gen_construction(CGInfo, Type, CtorId) = Instrs :-
    ( Type = builtin_type(_),
        unexpected($file, $pred, "No builtin types are constructed with
        e_construction")
    ; Type = type_variable(_),
        unexpected($file, $pred, "Polymorphic values are never constructed")
    ; Type = type_ref(TypeId, _),
        map.lookup(CGInfo ^ cgi_type_ctor_tags, {TypeId, CtorId}, CtorData),
        CtorProc = CtorData ^ cd_construct_proc,

        Instrs = from_list([
            pzio_comment("Call constructor"),
            pzio_instr(pzi_call(CtorProc))])
    ).

%-----------------------------------------------------------------------%

:- type continuation
    --->    cont_return
    ;       cont_jump(cj_depth :: int, cj_block :: int)
    ;       cont_instrs(int, cord(pz_instr_obj))
    ;       cont_comment(string, continuation).

:- func gen_continuation(continuation, int, int) = cord(pz_instr_obj).

gen_continuation(cont_return, Depth, Items) =
    snoc(fixup_stack(Depth, Items), pzio_instr(pzi_ret)).
gen_continuation(cont_jump(WantDepth, Block), CurDepth, Items) =
        snoc(FixupStack, pzio_instr(pzi_jmp(Block))) :-
    % Fixup the stack to put it at Depth plus Items.
    BottomItems = CurDepth + Items - WantDepth,
    FixupStack = fixup_stack(BottomItems, Items).
gen_continuation(cont_instrs(WantDepth, Instrs), CurDepth, Items) =
        FixupStack ++ Instrs :-
    % Fixup the stack to put it at Depth plus Items.
    BottomItems = CurDepth + Items - WantDepth,
    FixupStack = fixup_stack(BottomItems, Items).
gen_continuation(cont_comment(Comment, Continuation), CurDepth, Items) =
    cons(pzio_comment(Comment),
         gen_continuation(Continuation, CurDepth, Items)).

:- pred continuation_make_block(continuation::in, continuation::out,
    pz_blocks::in, pz_blocks::out) is det.

continuation_make_block(cont_return, cont_return, !Blocks).
continuation_make_block(cont_jump(Depth, Block), cont_jump(Depth, Block),
    !Blocks).
continuation_make_block(cont_instrs(Depth, Instrs), cont_jump(Depth, BlockId),
        !Blocks) :-
    alloc_block(BlockId, !Blocks),
    create_block(BlockId, Instrs, !Blocks).
continuation_make_block(cont_comment(Comment, Cont0),
        cont_comment(Comment, Cont), !Blocks) :-
    continuation_make_block(Cont0, Cont, !Blocks).

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
                pzb_blocks      :: map(int, pz_block)
            ).

:- pred alloc_block(int::out, pz_blocks::in, pz_blocks::out) is det.

alloc_block(BlockId, !Blocks) :-
    BlockId = !.Blocks ^ pzb_next_block,
    !Blocks ^ pzb_next_block := BlockId + 1.

:- pred create_block(int::in, cord(pz_instr_obj)::in,
    pz_blocks::in, pz_blocks::out) is det.

create_block(BlockId, Instrs, !Blocks) :-
    Block = pz_block(cord.list(Instrs)),
    BlockMap0 = !.Blocks ^ pzb_blocks,
    det_insert(BlockId, Block, BlockMap0, BlockMap),
    !Blocks ^ pzb_blocks := BlockMap.

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

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
