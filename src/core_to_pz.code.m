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
    map(func_id, pzp_id)::in, builtin_procs::in,
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
:- import_module util.

%-----------------------------------------------------------------------%

gen_proc(Core, OpIdMap, ProcIdMap, BuiltinProcs, TypeTagInfo, DataMap,
        FuncId, PID - Proc) :-
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
                TypeTagInfo, DataMap, Vartypes, Varmap),
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
                cgi_type_tags       :: map({type_id, ctor_id},
                                            constructor_data),
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
gen_instrs_case_match(CGInfo, p_ctor(CtorId, _), BlockNum, VarType, !Depth,
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
    JmpInstrs = from_list([
        depth_comment_instr(!.Depth + 1),
        pzio_instr(pzi_cjmp(BlockNum, pzw_fast))]),
    add_instrs(SetupInstrs ++ MatchInstrs ++ JmpInstrs, !Instrs).

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
    ; TagInfo = ti_tagged_pointer(PTag),
        BreakTagId = CGInfo ^ cgi_builtin_procs ^ bp_break_tag,
        % TODO rather than dropping the pointer we should save it and use it
        % for deconstruction later.
        Instrs = from_list([
            pzio_instr(pzi_call(BreakTagId)),
            pzio_instr(pzi_roll(2)),
            pzio_instr(pzi_drop),
            pzio_instr(pzi_load_immediate(pzw_ptr, immediate32(PTag))),
            pzio_instr(pzi_eq(pzw_ptr))])
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
    lookup(CGInfo ^ cgi_type_tags, {TypeId, CtorId}) ^ cd_tag_info.

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
        ; TagInfo = ti_tagged_pointer(PTag),
            MakeTag = CGInfo ^ cgi_builtin_procs ^ bp_make_tag,
            core_get_constructor_det(CGInfo ^ cgi_core, TypeId, CtorId,
                _Ctor),
            %NumFields = length(Ctor ^ c_fields),
            Instrs = from_list([pzio_comment("Construct struct"),
                pzio_instr(pzi_load_immediate(pzw_ptr, immediate32(PTag))),
                pzio_instr(pzi_call(MakeTag))]),
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

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
