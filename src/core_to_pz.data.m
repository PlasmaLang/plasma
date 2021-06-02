%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module core_to_pz.data.
%
% Copyright (C) 2015-2020 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
% Plasma core to pz conversion - data layout decisions
%
%-----------------------------------------------------------------------%

:- interface.

:- import_module maybe.
:- import_module string.

:- import_module builtins.
:- import_module common_types.
:- import_module core.
:- import_module core_to_pz.closure.
:- import_module core_to_pz.locn.
:- import_module pz.

%-----------------------------------------------------------------------%

:- type const_data
    --->    cd_string(string).

:- pred gen_const_data(core::in,
    val_locn_map_static::in, val_locn_map_static::out,
    closure_builder::in, closure_builder::out,
    map(string, pzd_id)::in, map(string, pzd_id)::out, pz::in, pz::out) is det.

%-----------------------------------------------------------------------%

    % How to represent this constructor in memory.
    %
:- type constructor_data
    --->    constructor_data(
                cd_tag_info         :: ctor_tag_info,
                cd_construct_proc   :: pzp_id
            ).

:- type ctor_tag_info
    --->    ti_constant(
                tic_ptag            :: ptag,
                tic_word_bits       :: word_bits
            )
    ;       ti_constant_notag(
                ticnw_bits          :: word_bits
            )
    ;       ti_tagged_pointer(
                titp_ptag           :: ptag,
                titp_struct         :: pzs_id,
                titp_maybe_stag     :: maybe(stag)
            ).

:- type ptag == uint8.
:- type stag == uint32.
:- type word_bits == uint32.

:- type type_tag_info
            % Pure enums are untagged.
    --->    tti_untagged
    ;       tti_tagged(
                map(ptag, type_ptag_info)
            )
    ;       tti_abstract.

:- type type_ptag_info
    --->    tpti_constant(map(word_bits, ctor_id))
    ;       tpti_pointer(ctor_id)
    ;       tpti_pointer_stag(map(stag, ctor_id)).

:- pred gen_constructor_data(core::in, pz_builtin_ids::in,
    map(type_id, type_tag_info)::out,
    map({type_id, ctor_id}, constructor_data)::out, pz::in, pz::out) is det.

:- func num_ptag_bits = int.

%-----------------------------------------------------------------------%

:- func type_to_pz_width(type_) = pz_width.

:- func bool_width = pz_width.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

:- import_module assoc_list.
:- import_module char.
:- import_module cord.
:- import_module int.
:- import_module uint32.
:- import_module uint8.

:- import_module context.
:- import_module core.code.

%-----------------------------------------------------------------------%

gen_const_data(Core, !LocnMap, !ModuleClo, !FilenameDataMap, !PZ) :-
    foldl4(gen_const_data_func, core_all_functions(Core),
        !LocnMap, !ModuleClo, !FilenameDataMap, !PZ).

:- pred gen_const_data_func(pair(func_id, function)::in,
    val_locn_map_static::in, val_locn_map_static::out,
    closure_builder::in, closure_builder::out,
    map(string, pzd_id)::in, map(string, pzd_id)::out,
    pz::in, pz::out) is det.

gen_const_data_func(_ - Func, !LocnMap, !ModuleClo,
        !FilenameDataMap, !PZ) :-
    Filename = func_get_context(Func) ^ c_file,
    ( if not search(!.FilenameDataMap, Filename, _) then
        pz_new_data_id(FilenameDataId, !PZ),
        pz_add_data(FilenameDataId, pz_encode_string(Filename), !PZ),
        det_insert(Filename, FilenameDataId, !FilenameDataMap)
    else
        true
    ),

    ( if func_get_body(Func, _, _, _, Expr) then
        gen_const_data_expr(Expr, !LocnMap, !ModuleClo, !PZ)
    else
        true
    ).

:- pred gen_const_data_expr(expr::in,
    val_locn_map_static::in, val_locn_map_static::out,
    closure_builder::in, closure_builder::out, pz::in, pz::out) is det.

gen_const_data_expr(expr(ExprType, _), !LocnMap, !ModuleClo, !PZ) :-
    ( ExprType = e_lets(Lets, Expr),
        foldl3(gen_const_data_lets, Lets, !LocnMap, !ModuleClo, !PZ),
        gen_const_data_expr(Expr, !LocnMap, !ModuleClo, !PZ)
    ; ExprType = e_tuple(Exprs),
        foldl3(gen_const_data_expr, Exprs, !LocnMap, !ModuleClo, !PZ)
    ; ExprType = e_call(_, _, _)
    ; ExprType = e_var(_)
    ; ExprType = e_constant(Const),
        ( Const = c_string(String),
            gen_const_data_string(String, !LocnMap, !ModuleClo, !PZ)
        ; Const = c_number(_)
        ; Const = c_func(_)
        ; Const = c_ctor(_)
        )
    ; ExprType = e_construction(_, _)
    ; ExprType = e_closure(_, _)
    ; ExprType = e_match(_, Cases),
        foldl3(gen_const_data_case, Cases, !LocnMap, !ModuleClo, !PZ)
    ).

:- pred gen_const_data_lets(expr_let::in,
    val_locn_map_static::in, val_locn_map_static::out,
    closure_builder::in, closure_builder::out, pz::in, pz::out) is det.

gen_const_data_lets(e_let(_, Expr), !LocnMap, !ModuleClo, !PZ) :-
    gen_const_data_expr(Expr, !LocnMap, !ModuleClo, !PZ).

:- pred gen_const_data_case(expr_case::in,
    val_locn_map_static::in, val_locn_map_static::out,
    closure_builder::in, closure_builder::out, pz::in, pz::out) is det.

gen_const_data_case(e_case(_, Expr), !LocnMap, !ModuleClo, !PZ) :-
    gen_const_data_expr(Expr, !LocnMap, !ModuleClo, !PZ).

:- pred gen_const_data_string(string::in,
    val_locn_map_static::in, val_locn_map_static::out,
    closure_builder::in, closure_builder::out, pz::in, pz::out) is det.

gen_const_data_string(String, !LocnMap, !ModuleClo, !PZ) :-
    ( if vls_has_str(!.LocnMap, String) then
        true
    else
        pz_new_data_id(DID, !PZ),
        % XXX: currently ASCII.
        Bytes = map(func(C) = pzv_num(to_int(C)), to_char_list(String)) ++
            [pzv_num(0)],
        Data = pz_data(type_array(pzw_8, length(Bytes)), Bytes),
        pz_add_data(DID, Data, !PZ),
        closure_add_field(pzv_data(DID), FieldNum, !ModuleClo),
        vls_insert_str(String, closure_get_struct(!.ModuleClo), FieldNum,
            type_to_pz_width(builtin_type(string)), !LocnMap)
    ).

%-----------------------------------------------------------------------%

gen_constructor_data(Core, BuiltinProcs, TypeTagMap, CtorTagMap,
        !PZ) :-
    Types = core_all_types(Core),
    foldl3(gen_constructor_data_type(Core, BuiltinProcs), Types,
        map.init, TypeTagMap, map.init, CtorTagMap, !PZ).

:- pred gen_constructor_data_type(core::in, pz_builtin_ids::in,
    pair(type_id, user_type)::in,
    map(type_id, type_tag_info)::in, map(type_id, type_tag_info)::out,
    map({type_id, ctor_id}, constructor_data)::in,
    map({type_id, ctor_id}, constructor_data)::out,
    pz::in, pz::out) is det.

gen_constructor_data_type(Core, BuiltinProcs, TypeId - Type, !TypeTagMap,
        !CtorDatas, !PZ) :-
    gen_constructor_tags(Core, Type, TypeTagInfo, CtorTagInfos, !PZ),

    det_insert(TypeId, TypeTagInfo, !TypeTagMap),

    MaybeCtorIds = utype_get_ctors(Type),
    ( MaybeCtorIds = yes(CtorIds),
        foldl2(gen_constructor_data_ctor(Core, BuiltinProcs, TypeId, Type,
            CtorTagInfos), CtorIds, !CtorDatas, !PZ)
    ; MaybeCtorIds = no
        % There's nothing to generate for abstractly-imported types
    ).

:- pred gen_constructor_data_ctor(core::in, pz_builtin_ids::in,
    type_id::in, user_type::in, map(ctor_id, ctor_tag_info)::in, ctor_id::in,
    map({type_id, ctor_id}, constructor_data)::in,
    map({type_id, ctor_id}, constructor_data)::out,
    pz::in, pz::out) is det.

gen_constructor_data_ctor(Core, BuiltinProcs, TypeId, Type, TagInfoMap,
        CtorId, !CtorDatas, !PZ) :-
    map.lookup(TagInfoMap, CtorId, TagInfo),

    maybe_gen_struct(Core, CtorId, TagInfo, !PZ),

    ModuleName = module_name(Core),
    core_get_constructor_det(Core, CtorId, Ctor),
    gen_constructor_proc(ModuleName, BuiltinProcs, Type, Ctor, TagInfo,
        ConstructProc, !PZ),

    CD = constructor_data(TagInfo, ConstructProc),
    map.det_insert({TypeId, CtorId}, CD, !CtorDatas).

%-----------------------------------------------------------------------%

:- pred maybe_gen_struct(core::in, ctor_id::in, ctor_tag_info::in,
    pz::in, pz::out) is det.

maybe_gen_struct(Core, CtorId, TagInfo, !PZ) :-
    core_get_constructor_det(Core, CtorId, Ctor),
    Fields = Ctor ^ c_fields,
    NumFields = length(Fields),
    ( if NumFields > 0 then
        (
            ( TagInfo = ti_constant(_, _)
            ; TagInfo = ti_constant_notag(_)
            ),
            unexpected($file, $pred, "Constant can't have fields")
        ; TagInfo = ti_tagged_pointer(_, StructId, MaybeSTag),
            ( MaybeSTag = yes(_),
                STagFields = 1
            ; MaybeSTag = no,
                STagFields = 0
            )
        ),
        duplicate(NumFields + STagFields, pzw_ptr, StructFields),
        Struct = pz_struct(StructFields),
        pz_add_struct(StructId, Struct, !PZ)
    else
        true
    ).

%-----------------------------------------------------------------------%

:- pred gen_constructor_proc(q_name::in, pz_builtin_ids::in,
    user_type::in, constructor::in, ctor_tag_info::in, pzp_id::out,
    pz::in, pz::out) is det.

gen_constructor_proc(ModuleName, BuiltinProcs, Type, Ctor, TagInfo, ProcId,
        !PZ) :-
    % TODO Move the construction out-of-line into a separate procedure,
    % this is also used when the constructor is used as a higher order
    % value.  It may be later inlined.
    ( TagInfo = ti_constant(PTag, WordBits),
        ShiftMakeTag = BuiltinProcs ^ pbi_shift_make_tag,
        Instrs = from_list([pzio_comment("Construct tagged constant"),
            pzio_instr(pzi_load_immediate(pzw_ptr, im_u32(WordBits))),
            pzio_instr(pzi_load_immediate(pzw_ptr,
                im_u32(cast_from_int(to_int(PTag))))),
            pzio_instr(pzi_call(pzc_import(ShiftMakeTag)))])
    ; TagInfo = ti_constant_notag(Word),
        Instrs = from_list([pzio_comment("Construct constant"),
            pzio_instr(pzi_load_immediate(pzw_ptr, im_u32(Word)))])
    ; TagInfo = ti_tagged_pointer(PTag, Struct, MaybeSTag),
        MakeTag = BuiltinProcs ^ pbi_make_tag,

        InstrsAlloc = from_list([pzio_comment("Construct struct"),
            pzio_instr(pzi_alloc(Struct))]),

        list.map_foldl(gen_construction_store(Struct), Ctor ^ c_fields,
            InstrsStore0, FirstField, _),
        InstrsStore = cord.from_list(reverse(InstrsStore0)),

        ( MaybeSTag = no,
            FirstField = 1,
            InstrsPutTag = init
        ; MaybeSTag = yes(STag),
            FirstField = 2,
            InstrsPutTag = from_list([
                pzio_instr(pzi_load_immediate(pzw_ptr, im_u32(STag))),
                pzio_instr(pzi_roll(2)),
                pzio_instr(pzi_store(Struct, field_num(1), pzw_ptr))])
        ),

        InstrsTag = from_list([
            pzio_instr(pzi_load_immediate(pzw_ptr,
                im_u32(cast_from_int(to_int(PTag))))),
            pzio_instr(pzi_call(pzc_import(MakeTag)))]),

        Instrs = InstrsAlloc ++ InstrsStore ++ InstrsPutTag ++ InstrsTag
    ),

    pz_new_proc_id(ProcId, !PZ),
    TypeName = utype_get_name(Type),
    CtorName = Ctor ^ c_name,
    q_name_parts(CtorName, MaybeModuleName, CtorNameSingle),
    ( MaybeModuleName = yes(CtorModuleName),
        Name = q_name_append_str(ModuleName,
            format("construct_%s_%s_%s",
                [s(replace_all(q_name_to_string(CtorModuleName), ".", "_")),
                 s(nq_name_to_string(q_name_unqual(TypeName))),
                 s(nq_name_to_string(CtorNameSingle))]))
    ; MaybeModuleName = no,
        Name = q_name_append_str(ModuleName,
            format("construct_%s_%s",
                [s(nq_name_to_string(q_name_unqual(TypeName))),
                 s(nq_name_to_string(CtorNameSingle))]))
    ),
    Before = list.duplicate(length(Ctor ^ c_fields), pzw_ptr),
    After = [pzw_ptr],
    RetInstr = pzio_instr(pzi_ret),
    Proc = pz_proc(Name, pz_signature(Before, After),
        yes([pz_block(list(snoc(Instrs, RetInstr)))])),
    pz_add_proc(ProcId, Proc, !PZ).

:- pred gen_construction_store(pzs_id::in, T::in,
    pz_instr_obj::out, int::in, int::out) is det.

gen_construction_store(StructId, _, Instr, !FieldNo) :-
    Instr = pzio_instr(pzi_store(StructId, field_num(!.FieldNo), pzw_ptr)),
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
% Some types don't need to fill out the rest of the field for a given ptag.
% For example something like:
%
%   type Type = C1 Int | C2 | C3.
%
% Can use a primary tag for each of C1, C2 and C3. and have fewer switches.
%
% TODO: Optimisation:
% Special casing certain types, such as unboxing maybes, handling "no tag"
% types, etc.
%
% TODO: Optimisation:
% Some constants, such as 0, should be able to share a primary tag with a
% pointers.
%
%-----------------------------------------------------------------------%

:- pred gen_constructor_tags(core::in, user_type::in,
    type_tag_info::out, map(ctor_id, ctor_tag_info)::out,
    pz::in, pz::out) is det.

gen_constructor_tags(Core, Type, TypeTagInfo, !:CtorTagInfos, !PZ) :-
    MaybeCtorIds = utype_get_ctors(Type),
    ( MaybeCtorIds = yes(CtorIds),
        map((pred(CId::in, {CId, C}::out) is det :-
                core_get_constructor_det(Core, CId, C)
            ), CtorIds, Ctors),
        count_constructor_types(Ctors, NumNoArgs, NumWithArgs),
        ( if NumWithArgs = 0 then
            % This is a simple enum and therefore we can use strict enum
            % tagging.
            TypeTagInfo = tti_untagged,
            map_foldl(make_strict_enum_tag_info, CtorIds, CtorTagInfos, 0u32, _),
            !:CtorTagInfos =
                from_assoc_list(from_corresponding_lists(CtorIds, CtorTagInfos))
        else
            !:CtorTagInfos = map.init,
            some [!PTagMap] (
                !:PTagMap = map.init,
                ( if NumNoArgs \= 0 then
                    foldl3(make_enum_tag_info(0u8), Ctors, 0u32, _, !CtorTagInfos,
                        !PTagMap),
                    NextPTag = 1u8
                else
                    NextPTag = 0u8
                ),
                ( if
                    % We need secondary tags if there are more than
                    % num_ptag_vals constructors with fields plus a ptag for the
                    % constructors without fields.
                    (
                        NumNoArgs = 0,
                        NumWithArgs > num_ptag_vals
                    ;
                        NumNoArgs \= 0,
                        NumWithArgs + 1 > num_ptag_vals
                    )
                then
                    NeedSecTags = need_secondary_tags
                else
                    NeedSecTags = dont_need_secondary_tags
                ),
                TypeName = utype_get_name(Type),
                foldl5(make_ctor_tag_info(TypeName, NeedSecTags), Ctors,
                    NextPTag, _, 0u32, _, !CtorTagInfos, !PTagMap, !PZ),
                TypeTagInfo = tti_tagged(!.PTagMap)
            )
        )
    ; MaybeCtorIds = no,
        % We don't create constructor tags for abstractly-imported types.
        TypeTagInfo = tti_abstract,
        !:CtorTagInfos = init
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
:- pred make_enum_tag_info(ptag::in, {ctor_id, constructor}::in,
    word_bits::in, word_bits::out,
    map(ctor_id, ctor_tag_info)::in, map(ctor_id, ctor_tag_info)::out,
    map(ptag, type_ptag_info)::in, map(ptag, type_ptag_info)::out) is det.

make_enum_tag_info(PTag, {CtorId, Ctor}, !WordBits, !CtorTagMap,
        !TypePTagMap) :-
    Fields = Ctor ^ c_fields,
    ( Fields = [],
        det_insert(CtorId, ti_constant(PTag, !.WordBits), !CtorTagMap),
        add_ptag_constant(PTag, !.WordBits, CtorId, !TypePTagMap),
        !:WordBits = !.WordBits + 1u32
    ; Fields = [_ | _]
    ).

    % make_strict_enum_tag_info(Ctor, ThisWordBits, NextWordBits,
    %   !TagInfoMap).
    %
:- pred make_strict_enum_tag_info(ctor_id::in, ctor_tag_info::out,
    word_bits::in, word_bits::out) is det.

make_strict_enum_tag_info(_, TagInfo, !WordBits) :-
    TagInfo = ti_constant_notag(!.WordBits),
    !:WordBits = !.WordBits + 1u32.

    % Used to inform make_ctor_tag_info if secondary tags will be used some
    % constructors of this type.  This is used to deterime if the first
    % constructor to use the final primary tag should have a secondary tag
    % to differentiate itself from further constructors that would share the
    % primary tag.  If no further constructors exist, then a secondary tag
    % isn't required.
    %
:- type need_secondary_tags
    --->    need_secondary_tags
    ;       dont_need_secondary_tags.

:- pred make_ctor_tag_info(q_name::in, need_secondary_tags::in,
    {ctor_id, constructor}::in, ptag::in, ptag::out, stag::in, stag::out,
    map(ctor_id, ctor_tag_info)::in, map(ctor_id, ctor_tag_info)::out,
    map(ptag, type_ptag_info)::in, map(ptag, type_ptag_info)::out,
    pz::in, pz::out) is det.

make_ctor_tag_info(TypeName, NeedSecTag, {CtorId, Ctor}, !PTag, !STag,
        !CtorTagMap, !TypePTagMap, !PZ) :-
    Fields = Ctor ^ c_fields,
    ( Fields = []
    ; Fields = [_ | _],
        StructName = q_name_to_string(TypeName) ++ "_" ++
            q_name_to_string(Ctor ^ c_name),
        pz_new_struct_id(StructId, StructName, !PZ),
        ( if
            (
                !.PTag < det_from_int(num_ptag_vals - 1)
            ;
                !.PTag = det_from_int(num_ptag_vals - 1),
                NeedSecTag = dont_need_secondary_tags
            )
        then
            det_insert(CtorId, ti_tagged_pointer(!.PTag, StructId, no),
                !CtorTagMap),
            det_insert(!.PTag, tpti_pointer(CtorId), !TypePTagMap),
            !:PTag = !.PTag + 1u8
        else
            det_insert(CtorId, ti_tagged_pointer(!.PTag, StructId, yes(!.STag)),
                !CtorTagMap),
            add_ptag_stag(!.PTag, !.STag, CtorId, !TypePTagMap),
            !:STag = !.STag + 1u32
        )
    ).

%-----------------------------------------------------------------------%

:- pred add_ptag_constant(ptag::in, word_bits::in, ctor_id::in,
    map(ptag, type_ptag_info)::in, map(ptag, type_ptag_info)::out) is det.

add_ptag_constant(PTag, Constant, CtorId, !Map) :-
    ( if search(!.Map, PTag, Entry0) then
        ( Entry0 = tpti_constant(ConstMap0)
        ;
            ( Entry0 = tpti_pointer(_)
            ; Entry0 = tpti_pointer_stag(_)
            ),
            unexpected($file, $pred,
                "Constants and pointers cannot share a ptag")
        )
    else
        ConstMap0 = map.init
    ),
    det_insert(Constant, CtorId, ConstMap0, ConstMap),
    set(PTag, tpti_constant(ConstMap), !Map).

:- pred add_ptag_stag(ptag::in, stag::in, ctor_id::in,
    map(ptag, type_ptag_info)::in, map(ptag, type_ptag_info)::out) is det.

add_ptag_stag(PTag, STag, CtorId, !Map) :-
    ( if search(!.Map, PTag, Entry0) then
        ( Entry0 = tpti_pointer_stag(STagMap0)
        ;
            ( Entry0 = tpti_pointer(_)
            ; Entry0 = tpti_constant(_)
            ),
            unexpected($file, $pred,
                "If one ctor for this ptag has an stag, then all must.")
        )
    else
        STagMap0 = map.init
    ),
    det_insert(STag, CtorId, STagMap0, STagMap),
    set(PTag, tpti_pointer_stag(STagMap), !Map).

%-----------------------------------------------------------------------%

% This must be equal to or less than the number of tag bits set in the
% runtime.  See runtime/pz_run.h.
num_ptag_bits = 2.

:- func num_ptag_vals = int.

num_ptag_vals = pow(2, num_ptag_bits).

%-----------------------------------------------------------------------%

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
        ; Type = func_type(_, _, _, _)
        ),
        Width = pzw_ptr
    ).

% This must match the calculation above, and is provided to avoid a
% dependency in builtins.m
bool_width = pzw_ptr.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
