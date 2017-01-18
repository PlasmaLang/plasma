%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module core_to_pz.data.
%
% Copyright (C) 2015-2017 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
% Plasma core to pz conversion - data layout decisions
%
%-----------------------------------------------------------------------%

:- interface.

:- import_module string.

:- import_module core.
:- import_module pz.

%-----------------------------------------------------------------------%

:- type const_data
    --->    cd_string(string).

:- pred gen_const_data(core::in, func_id::in,
    map(const_data, pzd_id)::in, map(const_data, pzd_id)::out,
    pz::in, pz::out) is det.

%-----------------------------------------------------------------------%

    % How to represent this constructor in memory.
    %
:- type constructor_data
    --->    constructor_data(
                cd_tag_info         :: ctor_tag_info,
                cd_maybe_struct     :: maybe(pzs_id),
                cd_construct_proc   :: pzp_id
            ).

:- type ctor_tag_info
    --->    ti_constant(
                tic_ptag            :: int,
                tic_word_bits       :: int
            )
    ;       ti_constant_notag(
                ticnw_bits          :: int
            )
    ;       ti_tagged_pointer(
                titp_ptag           :: int
            ).

:- pred gen_constructor_data(core::in, builtin_procs::in,
    map({type_id, ctor_id}, constructor_data)::out, pz::in, pz::out) is det.

:- func num_ptag_bits = int.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

:- import_module assoc_list.
:- import_module char.
:- import_module cord.
:- import_module int.

:- import_module core.code.
:- import_module util.

%-----------------------------------------------------------------------%

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

gen_constructor_data(Core, BuiltinProcs, TagMap, !PZ) :-
    TypeIds = core_all_types(Core),
    foldl2(gen_constructor_data_type(Core, BuiltinProcs), TypeIds,
        init, TagMap, !PZ).

:- pred gen_constructor_data_type(core::in, builtin_procs::in, type_id::in,
    map({type_id, ctor_id}, constructor_data)::in,
    map({type_id, ctor_id}, constructor_data)::out,
    pz::in, pz::out) is det.

gen_constructor_data_type(Core, BuiltinProcs, TypeId, !CtorDatas, !PZ) :-
    TagInfos = gen_constructor_tags(Core, TypeId),

    Type = core_get_type(Core, TypeId),
    CtorIds = type_get_ctors(Type),
    foldl2(gen_constructor_data_ctor(Core, BuiltinProcs, TypeId, Type,
            TagInfos), CtorIds, !CtorDatas, !PZ).

:- pred gen_constructor_data_ctor(core::in, builtin_procs::in,
    type_id::in, user_type::in, map(ctor_id, ctor_tag_info)::in, ctor_id::in,
    map({type_id, ctor_id}, constructor_data)::in,
    map({type_id, ctor_id}, constructor_data)::out,
    pz::in, pz::out) is det.

gen_constructor_data_ctor(Core, BuiltinProcs, TypeId, Type, TagInfoMap,
        CtorId, !CtorDatas, !PZ) :-
    map.lookup(TagInfoMap, CtorId, TagInfo),

    maybe_gen_struct(Core, TypeId, CtorId, MaybeStructId, !PZ),

    ModuleName = module_name(Core),
    core_get_constructor_det(Core, TypeId, CtorId, Ctor),
    gen_constructor_proc(ModuleName, BuiltinProcs, Type,
        Ctor, TagInfo, MaybeStructId, ConstructProc, !PZ),

    CD = constructor_data(TagInfo, MaybeStructId, ConstructProc),
    map.det_insert({TypeId, CtorId}, CD, !CtorDatas).

%-----------------------------------------------------------------------%

:- pred maybe_gen_struct(core::in, type_id::in, ctor_id::in,
    maybe(pzs_id)::out, pz::in, pz::out) is det.

maybe_gen_struct(Core, TypeId, CtorId, MaybeStructId, !PZ) :-
    core_get_constructor_det(Core, TypeId, CtorId, Ctor),
    Fields = Ctor ^ c_fields,
    NumFields = length(Fields),
    ( if NumFields > 0 then
        duplicate(NumFields, pzw_ptr, StructFields),
        Struct = pz_struct(StructFields),
        pz_add_struct(StructId, Struct, !PZ),
        MaybeStructId = yes(StructId)
    else
        MaybeStructId = no
    ).

%-----------------------------------------------------------------------%

:- pred gen_constructor_proc(q_name::in, builtin_procs::in,
    user_type::in, constructor::in, ctor_tag_info::in, maybe(pzs_id)::in,
    pzp_id::out, pz::in, pz::out) is det.

gen_constructor_proc(ModuleName, BuiltinProcs, Type, Ctor,
        TagInfo, MaybeStruct, ProcId, !PZ) :-
    % TODO Move the construction out-of-line into a separate procedure,
    % this is also used when the constructor is used as a higher order
    % value.  It may be later inlined.
    ( TagInfo = ti_constant(PTag, WordBits),
        ShiftMakeTag = BuiltinProcs ^ bp_shift_make_tag,
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
        MakeTag = BuiltinProcs ^ bp_make_tag,
        ( MaybeStruct = yes(Struct)
        ; MaybeStruct = no,
            unexpected($file, $pred, "No structure ID")
        ),

        InstrsAlloc = from_list([pzio_comment("Construct struct"),
            pzio_instr(pzi_alloc(Struct))]),

        map_foldl(gen_construction_store(Struct), Ctor ^ c_fields,
            InstrsStore0, 1, _),
        InstrsStore = from_list(reverse(InstrsStore0)),

        InstrsTag = from_list([
            pzio_instr(pzi_load_immediate(pzw_ptr, immediate32(PTag))),
            pzio_instr(pzi_call(MakeTag))]),

        Instrs = InstrsAlloc ++ InstrsStore ++ InstrsTag
    ),

    pz_new_proc_id(i_local, ProcId, !PZ),
    TypeName = type_get_name(Type),
    CtorName = Ctor ^ c_name,
    Name = q_name_snoc(ModuleName,
        format("construct_%s_%s",
            [s(q_name_unqual(TypeName)), s(q_name_unqual(CtorName))])),
    Before = list.duplicate(length(Ctor ^ c_fields), pzw_ptr),
    After = [pzw_ptr],
    RetInstr = pzio_instr(pzi_ret),
    Proc = pz_proc(Name, pz_signature(Before, After),
        yes([pz_block(to_list(snoc(Instrs, RetInstr)))])),
    pz_add_proc(ProcId, Proc, !PZ).

:- pred gen_construction_store(pzs_id::in, T::in,
    pz_instr_obj::out, int::in, int::out) is det.

gen_construction_store(StructId, _, Instr, !FieldNo) :-
    Instr = pzio_instr(pzi_store(StructId, !.FieldNo, pzw_ptr)),
    !:FieldNo = !.FieldNo + 1.

%-----------------------------------------------------------------------%
%
% Pointer tagging
% ===============
%
% All memory allocations are machine-word aligned, this means that there are
% two or three low-bits available for pointer tags (32bit or 64bit systems).
% There may be high bits available also, especially on 64bit systems.  For
% now we assume that there are always two low bits available.
%
% TODO: Optimization
% Using the third bit on a 64 bit system would be good.  This can be handled
% by compiling two versions of the program and storing them both in the .pz
% file.  One for 32-bit and one for 64-bit.  This would happen at a late
% stage of compilation and won't duplicate much work.  It could be skipped
% or stripped from the file to reduce file size if required.
%
% We use tagged pointers to implement discriminated unions.  Most of what we
% do here is based upon the Mercury project.  Each pointer's 2 lowest bits
% are the _primary tag_, a _secondary tag_ can be stored in the pointed-to
% object when required.
%
% Each type's constructors are grouped into those that have arguments, and
% those that do not.  The first primary tag "00" is used for the group of
% constants with the higher bits used to encode each constant.  The
% next remaining primary tags "01" "10" (and "00" if it was not used in the
% first step") are given to the first two-three constructors with arguments
% and the final tag "11" is used for all the remaining constructors,
% utilizing a second tag as the first field in the pointed-to structure to
% distinguish between them.
%
% This scheme has a specific benefit that is if there is only a single
% no-argument constructor then it has the same value as the null pointer.
% Therefore the cons cell has the same encoding as either a normal pointer
% or the null pointer.  Likewise a maybe value can be unboxed in some cases.
% (Not implemented yet).
%
% Exception: strict enums
% If a type is an enum _only_ then it doesn't require any tag bits and is
% encoded simply as a raw value.  This exception enables the bool type to
% use 0 and 1 conveniently.
%
% TODO: Optimisation:
% Special casing certain types, such as unboxing maybes, handling "no tag"
% types, etc.
%
%-----------------------------------------------------------------------%

:- func gen_constructor_tags(core, type_id) = map(ctor_id, ctor_tag_info).

gen_constructor_tags(Core, TypeId) = !:TagInfos :-
    Type = core_get_type(Core, TypeId),
    CtorIds = type_get_ctors(Type),
    map((pred(CId::in, {CId, C}::out) is det :-
            core_get_constructor_det(Core, TypeId, CId, C)
        ), CtorIds, Ctors),
    count_constructor_types(Ctors, NumNoArgs, NumWithArgs),
    ( if NumWithArgs = 0 then
        % This is a simple enum and therefore we can use strict enum
        % tagging.
        map_foldl(make_strict_enum_tag_info, CtorIds, TagInfos, 0, _),
        !:TagInfos =
            from_assoc_list(from_corresponding_lists(CtorIds, TagInfos))
    else
        !:TagInfos = map.init,
        ( if NumNoArgs \= 0 then
            foldl2(make_enum_tag_info(0), Ctors, 0, _, !TagInfos),
            NextPTag = 1
        else
            NextPTag = 0
        ),
        foldl3(make_ctor_tag_info, Ctors, NextPTag, _, 0, _, !TagInfos)
    ).

:- pred count_constructor_types(list({ctor_id, constructor})::in,
    int::out, int::out) is det.

count_constructor_types([], 0, 0).
count_constructor_types([{_, Ctor} | Ctors], NumNoArgs,
        NumWithArgs) :-
    count_constructor_types(Ctors, NumNoArgs0, NumWithArgs0),
    Args = Ctor ^ c_fields,
    ( Args = [],
        NumNoArgs = NumNoArgs0 + 1,
        NumWithArgs = NumWithArgs0
    ; Args = [_ | _],
        NumNoArgs = NumNoArgs0,
        NumWithArgs = NumWithArgs0 + 1
    ).

    % make_enum_tag_info(PTag, Ctor, ThisWordBits, NextWordBits,
    %   !TagInfoMap).
    %
:- pred make_enum_tag_info(int::in, {ctor_id, constructor}::in,
    int::in, int::out,
    map(ctor_id, ctor_tag_info)::in, map(ctor_id, ctor_tag_info)::out) is det.

make_enum_tag_info(PTag, {CtorId, Ctor}, !WordBits, !Map) :-
    Fields = Ctor ^ c_fields,
    ( Fields = [],
        det_insert(CtorId, ti_constant(PTag, !.WordBits), !Map),
        !:WordBits = !.WordBits + 1
    ; Fields = [_ | _]
    ).

    % make_strict_enum_tag_info(Ctor, ThisWordBits, NextWordBits,
    %   !TagInfoMap).
    %
:- pred make_strict_enum_tag_info(ctor_id::in, ctor_tag_info::out,
    int::in, int::out) is det.

make_strict_enum_tag_info(_, TagInfo, !WordBits) :-
    TagInfo = ti_constant_notag(!.WordBits),
    !:WordBits = !.WordBits + 1.

:- pred make_ctor_tag_info({ctor_id, constructor}::in,
    int::in, int::out, int::in, int::out,
    map(ctor_id, ctor_tag_info)::in, map(ctor_id, ctor_tag_info)::out) is det.

make_ctor_tag_info({CtorId, Ctor}, !PTag, !STag, !Map) :-
    Fields = Ctor ^ c_fields,
    ( Fields = []
    ; Fields = [_ | _],
        ( if !.PTag < pow(2, num_ptag_bits) then
            det_insert(CtorId, ti_tagged_pointer(!.PTag), !Map),
            !:PTag = !.PTag + 1
        else
            util.sorry($file, $pred, "Secondary tags not supported")
        )
    ).

%-----------------------------------------------------------------------%

% This must be equal to or less than the number of tag bits set in the
% runtime.  See runtime/pz_run.h.
num_ptag_bits = 2.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
