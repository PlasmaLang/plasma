%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module builtins.
%
% Copyright (C) 2015-2021 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
% Plasma builtins.
%
% Builtins belong in the builtin module which is implicitly imported, both
% with and without the "builtin" qualifier during compilation of any module.
% Builtins may include functions, types and their constructors, interfaces
% and interface implementations.
%
% There are two types of builtin function:
%
% Core builtins
% -------------
%
% Any procedure that could be written in Plasma, but it instead provided by
% the compiler and compiled (from Core representation, hence the name) with
% the program.  bool_to_string is an example, these builtins have their core
% definitions in this module.
%
% PZ inline builtins
% ------------------
%
% This covers arithmetic operators and other small "functions" that are
% equivalent to one or maybe 2-3 PZ instructions.  core_to_pz will convert
% calls to these functions into their native PZ bytecodes.
%
% Non-foreign builtin
% -------------------
%
% These builtins are stored as a sequence of PZ instructions within the
% runtime, they're executed just like normal procedures, their definitions
% are simply provided by the runtime rather than a .pz file.
%
% The runtime decides which builtins are non-foreign and which are foreign.
%
% Foreign builtins
% ----------------
%
% These mostly cover operating system services.  They are implemented in
% pz_run_*.c and are transformed when the program is read into an opcode
% that will cause the C procedure built into the RTS to be executed.  The
% specifics depend on which pz_run_*.c file is used.
%
% The runtime decides which builtins are non-foreign and which are foreign.
%
% PZ builtins
% -----------
%
% Builtins that are not callable Plasma functions, these are PZ procedures
% and are either foreign or non-foreign.
%
%-----------------------------------------------------------------------%

:- interface.

:- import_module map.

:- import_module common_types.
:- import_module core.
:- import_module core.types.
:- import_module pz.
:- import_module pz.pz_ds.
:- import_module q_name.

:- type builtin_item
    --->    bi_func(func_id)
    ;       bi_ctor(ctor_id)
    ;       bi_resource(resource_id)
    ;       bi_type(type_id, arity)
    ;       bi_type_builtin(builtin_type).

    % setup_builtins(Map, BoolTrue, BoolFalse, ListType,
    %   ListNil, ListCons, !Core)
    %
:- pred setup_builtins(map(nq_name, builtin_item)::out,
    ctor_id::out, ctor_id::out, type_id::out, ctor_id::out, ctor_id::out,
    core::in, core::out) is det.

:- func builtin_module_name = q_name.

:- func builtin_add_int = nq_name.
:- func builtin_sub_int = nq_name.
:- func builtin_mul_int = nq_name.
:- func builtin_div_int = nq_name.
:- func builtin_mod_int = nq_name.
:- func builtin_lshift_int = nq_name.
:- func builtin_rshift_int = nq_name.
:- func builtin_and_int = nq_name.
:- func builtin_or_int = nq_name.
:- func builtin_xor_int = nq_name.
:- func builtin_gt_int = nq_name.
:- func builtin_lt_int = nq_name.
:- func builtin_gteq_int = nq_name.
:- func builtin_lteq_int = nq_name.
:- func builtin_eq_int = nq_name.
:- func builtin_neq_int = nq_name.
:- func builtin_and_bool = nq_name.
:- func builtin_or_bool = nq_name.
:- func builtin_concat_string = nq_name.

% Unary operators.
:- func builtin_minus_int = nq_name.
:- func builtin_comp_int = nq_name.
:- func builtin_not_bool = nq_name.

% Operators that are consturctions.
:- func builtin_nil_list = nq_name.
:- func builtin_cons_list = nq_name.

%-----------------------------------------------------------------------%
%
% PZ Builtins
%

:- type pz_builtin_ids
    --->    pz_builtin_ids(
                pbi_make_tag        :: pzi_id,
                pbi_shift_make_tag  :: pzi_id,
                pbi_break_tag       :: pzi_id,
                pbi_break_shift_tag :: pzi_id,
                pbi_unshift_value   :: pzi_id,

                % A struct containing only a secondary tag.
                % TODO: actually make this imported so that the runtime
                % structures can be shared easily.
                pbi_stag_struct     :: pzs_id
            ).

    % Setup procedures that are PZ builtins but not Plasma builtins.  For
    % example things like tagged pointer manipulation.
    %
:- pred setup_pz_builtin_procs(pz_builtin_ids::out, pz::in, pz::out) is det.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module require.
:- import_module set.

:- import_module context.
:- import_module core.code.
:- import_module core.function.
:- import_module core.resource.
:- import_module core_to_pz.
:- import_module pz.code.
:- import_module varmap.

%-----------------------------------------------------------------------%

setup_builtins(!:Map, BoolTrue, BoolFalse, ListType, ListNil, ListCons,
        !Core) :-
    !:Map = init,
    setup_core_types(!Map),
    setup_bool_builtins(BoolType, BoolTrue, BoolFalse, !Map, !Core),
    setup_int_builtins(BoolType, !Map, !Core),
    setup_list_builtins(ListType, ListNil, ListCons, !Map, !Core),
    setup_misc_builtins(BoolType, BoolTrue, BoolFalse, !Map, !Core).

:- pred setup_core_types(map(nq_name, builtin_item)::in,
    map(nq_name, builtin_item)::out) is det.

setup_core_types(!Map) :-
    builtin_type_name(int, IntName),
    det_insert(IntName, bi_type_builtin(int), !Map),

    builtin_type_name(char, CharName),
    det_insert(CharName, bi_type_builtin(char), !Map),

    builtin_type_name(string, StringName),
    det_insert(StringName, bi_type_builtin(string), !Map),

    builtin_type_name(string_pos, StringPosName),
    det_insert(StringPosName, bi_type_builtin(string_pos), !Map).

%-----------------------------------------------------------------------%

:- pred setup_bool_builtins(type_id::out, ctor_id::out, ctor_id::out,
    map(nq_name, builtin_item)::in, map(nq_name, builtin_item)::out,
    core::in, core::out) is det.

setup_bool_builtins(BoolId, TrueId, FalseId, !Map, !Core) :-
    core_allocate_type_id(BoolId, !Core),

    FalseName = nq_name_det("False"),
    FalseQName = q_name_append(builtin_module_name, FalseName),
    core_allocate_ctor_id(FalseId, !Core),
    core_set_constructor(FalseId, FalseQName, BoolId,
        constructor(FalseQName, [], []), !Core),
    det_insert(FalseName, bi_ctor(FalseId), !Map),

    TrueName = nq_name_det("True"),
    TrueQName = q_name_append(builtin_module_name, TrueName),
    core_allocate_ctor_id(TrueId, !Core),
    core_set_constructor(TrueId, TrueQName, BoolId,
        constructor(TrueQName, [], []), !Core),
    det_insert(TrueName, bi_ctor(TrueId), !Map),

    % NOTE: False is first so that it is allocated 0 for its tag, and true
    % will be allocated 1 for its tag, this will make interoperability
    % easier.
    BoolName = nq_name_det("Bool"),
    core_set_type(BoolId,
        type_init(q_name_append(builtin_module_name, BoolName), [],
            [FalseId, TrueId], st_private),
        !Core),
    det_insert(BoolName, bi_type(BoolId, arity(0)), !Map),

    BoolWidth = bool_width,
    NotName = nq_name_det("not_bool"),
    NotQName = q_name_append(builtin_module_name, NotName),
    register_builtin_func(NotName,
        func_init_builtin_inline_pz(NotQName,
            [type_ref(BoolId, [])], [type_ref(BoolId, [])], init, init,
            [pzi_not(BoolWidth)]),
        _, !Map, !Core),

    register_bool_biop(BoolId, builtin_and_bool,
        [pzi_and(BoolWidth)], !Map, !Core),
    register_bool_biop(BoolId, builtin_or_bool,
        [pzi_or(BoolWidth)], !Map, !Core).

:- pred register_bool_biop(type_id::in, nq_name::in, list(pz_instr)::in,
    map(nq_name, builtin_item)::in, map(nq_name, builtin_item)::out,
    core::in, core::out) is det.

register_bool_biop(BoolType, Name, Defn, !Map, !Core) :-
    FName = q_name_append(builtin_module_name, Name),
    register_builtin_func(Name,
        func_init_builtin_inline_pz(FName,
            [type_ref(BoolType, []), type_ref(BoolType, [])],
            [type_ref(BoolType, [])],
            init, init,
            Defn),
        _, !Map, !Core).

%-----------------------------------------------------------------------%

:- pred setup_int_builtins(type_id::in,
    map(nq_name, builtin_item)::in, map(nq_name, builtin_item)::out,
    core::in, core::out) is det.

setup_int_builtins(BoolType, !Map, !Core) :-
    register_int_fn2(builtin_add_int, [pzi_add(pzw_fast)], !Map, !Core),
    register_int_fn2(builtin_sub_int, [pzi_sub(pzw_fast)], !Map, !Core),
    register_int_fn2(builtin_mul_int, [pzi_mul(pzw_fast)], !Map, !Core),
    % Mod and div can maybe be combined into one operator, and optimised at
    % PZ load time.
    register_int_fn2(builtin_div_int, [pzi_div(pzw_fast)], !Map, !Core),
    register_int_fn2(builtin_mod_int, [pzi_mod(pzw_fast)], !Map, !Core),

    % TODO: remove the extend operation once we fix how booleans are
    % stored.
    BoolWidth = bool_width,
    require(unify(BoolWidth, pzw_ptr),
        "Fix this code once we fix bool storage"),
    register_int_comp(BoolType, builtin_gt_int, [
            pzi_gt_s(pzw_fast),
            pzi_ze(pzw_fast, pzw_ptr)],
        !Map, !Core),
    register_int_comp(BoolType, builtin_lt_int, [
            pzi_lt_s(pzw_fast),
            pzi_ze(pzw_fast, pzw_ptr)],
        !Map, !Core),
    register_int_comp(BoolType, builtin_gteq_int, [
            pzi_lt_s(pzw_fast),
            pzi_not(pzw_fast),
            pzi_ze(pzw_fast, pzw_ptr)],
        !Map, !Core),
    register_int_comp(BoolType, builtin_lteq_int, [
            pzi_gt_s(pzw_fast),
            pzi_not(pzw_fast),
            pzi_ze(pzw_fast, pzw_ptr)],
        !Map, !Core),
    register_int_comp(BoolType, builtin_eq_int, [
            pzi_eq(pzw_fast),
            pzi_ze(pzw_fast, pzw_ptr)],
        !Map, !Core),
    register_int_comp(BoolType, builtin_neq_int, [
            pzi_eq(pzw_fast),
            pzi_not(pzw_fast),
            pzi_ze(pzw_fast, pzw_ptr)],
        !Map, !Core),

    register_int_fn1(builtin_minus_int,
        [pzi_load_immediate(pzw_fast, im_i32(0i32)),
         pzi_roll(2),
         pzi_sub(pzw_fast)],
        !Map, !Core),

    % Register the builtin bitwise functions..
    % TODO: make the number of bits to shift a single byte.
    register_int_fn2(builtin_lshift_int,
        [pzi_trunc(pzw_fast, pzw_8),
         pzi_lshift(pzw_fast)], !Map, !Core),
    register_int_fn2(builtin_rshift_int,
        [pzi_trunc(pzw_fast, pzw_8),
         pzi_rshift(pzw_fast)], !Map, !Core),
    register_int_fn2(builtin_and_int, [pzi_and(pzw_fast)], !Map, !Core),
    register_int_fn2(builtin_or_int, [pzi_or(pzw_fast)], !Map, !Core),
    register_int_fn2(builtin_xor_int,
        [pzi_xor(pzw_fast)], !Map, !Core),
    register_int_fn1(builtin_comp_int,
        [pzi_load_immediate(pzw_32, im_i32(-1i32)),
         pzi_se(pzw_32, pzw_fast),
         pzi_xor(pzw_fast)],
        !Map, !Core).

:- pred register_int_fn1(nq_name::in, list(pz_instr)::in,
    map(nq_name, builtin_item)::in, map(nq_name, builtin_item)::out,
    core::in, core::out) is det.

register_int_fn1(Name, Defn, !Map, !Core) :-
    FName = q_name_append(builtin_module_name, Name),
    register_builtin_func(Name,
        func_init_builtin_inline_pz(FName,
            [builtin_type(int)], [builtin_type(int)],
            init, init, Defn),
        _, !Map, !Core).

:- pred register_int_fn2(nq_name::in, list(pz_instr)::in,
    map(nq_name, builtin_item)::in, map(nq_name, builtin_item)::out,
    core::in, core::out) is det.

register_int_fn2(Name, Defn, !Map, !Core) :-
    FName = q_name_append(builtin_module_name, Name),
    register_builtin_func(Name,
        func_init_builtin_inline_pz(FName,
            [builtin_type(int), builtin_type(int)],
            [builtin_type(int)],
            init, init, Defn),
        _, !Map, !Core).

:- pred register_int_comp(type_id::in, nq_name::in, list(pz_instr)::in,
    map(nq_name, builtin_item)::in, map(nq_name, builtin_item)::out,
    core::in, core::out) is det.

register_int_comp(BoolType, Name, Defn, !Map, !Core) :-
    FName = q_name_append(builtin_module_name, Name),
    register_builtin_func(Name,
        func_init_builtin_inline_pz(FName,
            [builtin_type(int), builtin_type(int)],
            [type_ref(BoolType, [])],
            init, init, Defn),
        _, !Map, !Core).

:- pred setup_list_builtins(type_id::out, ctor_id::out, ctor_id::out,
    map(nq_name, builtin_item)::in, map(nq_name, builtin_item)::out,
    core::in, core::out) is det.

setup_list_builtins(ListId, NilId, ConsId, !Map, !Core) :-
    core_allocate_type_id(ListId, !Core),
    T = "T",

    NilQName = q_name_append(builtin_module_name, builtin_nil_list),
    core_allocate_ctor_id(NilId, !Core),
    core_set_constructor(NilId, NilQName, ListId,
        constructor(NilQName, [T], []), !Core),
    det_insert(builtin_nil_list, bi_ctor(NilId), !Map),

    Head = q_name_append_str(builtin_module_name, "head"),
    Tail = q_name_append_str(builtin_module_name, "tail"),
    Cons = q_name_append(builtin_module_name, builtin_cons_list),
    core_allocate_ctor_id(ConsId, !Core),
    core_set_constructor(ConsId, Cons, ListId,
        constructor(Cons, [T],
        [type_field(Head, type_variable(T)),
         type_field(Tail, type_ref(ListId, [type_variable(T)]))]), !Core),
    det_insert(builtin_cons_list, bi_ctor(ConsId), !Map),

    core_set_type(ListId,
        type_init(q_name_append_str(builtin_module_name, "List"), [T],
            [NilId, ConsId], st_private),
        !Core),
    % TODO: Add a constant for the List type name.
    det_insert(nq_name_det("List"), bi_type(ListId, arity(1)), !Map).

%-----------------------------------------------------------------------%

:- pred setup_misc_builtins(type_id::in, ctor_id::in, ctor_id::in,
    map(nq_name, builtin_item)::in, map(nq_name, builtin_item)::out,
    core::in, core::out) is det.

setup_misc_builtins(BoolType, BoolTrue, BoolFalse, !Map, !Core) :-
    register_builtin_resource(nq_name_det("IO"), r_io, RIO, !Map, !Core),

    PrintName = q_name_append_str(builtin_module_name, "print"),
    register_builtin_func(nq_name_det("print"),
        func_init_builtin_rts(PrintName,
            [builtin_type(string)], [], list_to_set([RIO]), init),
        _, !Map, !Core),

    ReadlnName = q_name_append_str(builtin_module_name, "readline"),
    register_builtin_func(nq_name_det("readline"),
        func_init_builtin_rts(ReadlnName,
            [], [builtin_type(string)], list_to_set([RIO]), init),
        _, !Map, !Core),

    IntToStringName = q_name_append_str(builtin_module_name, "int_to_string"),
    register_builtin_func(nq_name_det("int_to_string"),
        func_init_builtin_rts(IntToStringName,
            [builtin_type(int)], [builtin_type(string)], init, init),
        _, !Map, !Core),

    BoolToStringName = q_name_append_str(builtin_module_name, "bool_to_string"),
    BoolToString0 = func_init_builtin_core(BoolToStringName,
        [type_ref(BoolType, [])], [builtin_type(string)], init, init),
    define_bool_to_string(BoolTrue, BoolFalse, BoolToString0, BoolToString),
    register_builtin_func(nq_name_det("bool_to_string"), BoolToString,
        _, !Map, !Core),

    SetParameterName = q_name_append_str(builtin_module_name, "set_parameter"),
    register_builtin_func(nq_name_det("set_parameter"),
        func_init_builtin_rts(SetParameterName,
            [builtin_type(string), builtin_type(int)],
            [type_ref(BoolType, [])],
            list_to_set([RIO]), init),
        _, !Map, !Core),

    GetParameterName = q_name_append_str(builtin_module_name, "get_parameter"),
    register_builtin_func(nq_name_det("get_parameter"),
        func_init_builtin_rts(GetParameterName,
            [builtin_type(string)],
            [type_ref(BoolType, []), builtin_type(int)],
            list_to_set([RIO]), init),
        _, !Map, !Core),

    EnvironmentName = nq_name_det("Environment"),
    EnvironmentQName = q_name_append(builtin_module_name, EnvironmentName),
    register_builtin_resource(EnvironmentName,
        r_other(EnvironmentQName, RIO, s_private, i_imported, builtin_context),
        REnv, !Map, !Core),
    SetenvName = q_name_append_str(builtin_module_name, "setenv"),
    register_builtin_func(nq_name_det("setenv"),
        func_init_builtin_rts(SetenvName,
            [builtin_type(string), builtin_type(string)],
            [type_ref(BoolType, [])],
            list_to_set([REnv]), init),
        _, !Map, !Core),

    TimeName = nq_name_det("Time"),
    TimeQName = q_name_append(builtin_module_name, TimeName),
    register_builtin_resource(TimeName,
        r_other(TimeQName, RIO, s_private, i_imported, builtin_context),
        RTime, !Map, !Core),
    GettimeofdayName = q_name_append_str(builtin_module_name, "gettimeofday"),
    register_builtin_func(nq_name_det("gettimeofday"),
        func_init_builtin_rts(GettimeofdayName, [],
            [type_ref(BoolType, []), builtin_type(int), builtin_type(int)],
            init, list_to_set([RTime])),
        _, !Map, !Core),

    ConcatStringName = q_name_append(builtin_module_name,
        builtin_concat_string),
    register_builtin_func(builtin_concat_string,
        func_init_builtin_rts(ConcatStringName,
            [builtin_type(string), builtin_type(string)],
            [builtin_type(string)],
            init, init),
        _, !Map, !Core),

    DieName = nq_name_det("die"),
    DieQName = q_name_append(builtin_module_name, DieName),
    register_builtin_func(DieName,
        func_init_builtin_rts(DieQName, [builtin_type(string)], [],
            init, init),
        _, !Map, !Core).

%-----------------------------------------------------------------------%

:- pred register_builtin_func(nq_name::in, function::in, func_id::out,
    map(nq_name, builtin_item)::in, map(nq_name, builtin_item)::out,
    core::in, core::out) is det.

register_builtin_func(Name, Func, FuncId, !Map, !Core) :-
    core_allocate_function(FuncId, !Core),
    core_set_function(FuncId, Func, !Core),
    det_insert(Name, bi_func(FuncId), !Map).

:- pred register_builtin_resource(nq_name::in, resource::in,
    resource_id::out,
    map(nq_name, builtin_item)::in, map(nq_name, builtin_item)::out,
    core::in, core::out) is det.

register_builtin_resource(Name, Res, ResId, !Map, !Core) :-
    core_allocate_resource_id(ResId, !Core),
    core_set_resource(ResId, Res, !Core),
    det_insert(Name, bi_resource(ResId), !Map).

%-----------------------------------------------------------------------%

:- pred define_bool_to_string(ctor_id::in, ctor_id::in,
    function::in, function::out) is det.

define_bool_to_string(TrueId, FalseId, !Func) :-
    some [!Varmap] (
        !:Varmap = init,
        CI = code_info_init(o_builtin),

        varmap.add_anon_var(In, !Varmap),
        TrueCase = e_case(p_ctor(make_singleton_set(TrueId), []),
            expr(e_constant(c_string("True")), CI)),
        FalseCase = e_case(p_ctor(make_singleton_set(FalseId), []),
            expr(e_constant(c_string("False")), CI)),
        Expr = expr(e_match(In, [TrueCase, FalseCase]), CI),
        func_set_body(!.Varmap, [In], [], Expr, !Func)
    ).

%-----------------------------------------------------------------------%

builtin_module_name = q_name_single("Builtin").

builtin_add_int = nq_name_det("add_int").
builtin_sub_int = nq_name_det("sub_int").
builtin_mul_int = nq_name_det("mul_int").
builtin_div_int = nq_name_det("div_int").
builtin_mod_int = nq_name_det("mod_int").
builtin_lshift_int = nq_name_det("lshift_int").
builtin_rshift_int = nq_name_det("rshift_int").
builtin_and_int = nq_name_det("and_int").
builtin_or_int = nq_name_det("or_int").
builtin_xor_int = nq_name_det("xor_int").
builtin_gt_int = nq_name_det("gt_int").
builtin_lt_int = nq_name_det("lt_int").
builtin_gteq_int = nq_name_det("gteq_int").
builtin_lteq_int = nq_name_det("lteq_int").
builtin_eq_int = nq_name_det("eq_int").
builtin_neq_int = nq_name_det("neq_int").
builtin_and_bool = nq_name_det("and_bool").
builtin_or_bool = nq_name_det("or_bool").
builtin_concat_string = nq_name_det("concat_string").

builtin_minus_int = nq_name_det("minus_int").
builtin_comp_int = nq_name_det("comp_int").
builtin_not_bool = nq_name_det("not_bool").

% Constructors
builtin_nil_list = nq_name_det("Nil").
builtin_cons_list = nq_name_det("Cons").

%-----------------------------------------------------------------------%

setup_pz_builtin_procs(BuiltinProcs, !PZ) :-
    % pz_signature([pzw_ptr, pzw_ptr], [pzw_ptr])
    pz_new_import(MakeTag, make_tag_qname, !PZ),

    % pz_signature([pzw_ptr, pzw_ptr], [pzw_ptr])
    pz_new_import(ShiftMakeTag, shift_make_tag_qname, !PZ),

    % pz_signature([pzw_ptr], [pzw_ptr, pzw_ptr])
    pz_new_import(BreakTag, break_tag_qname, !PZ),

    % pz_signature([pzw_ptr], [pzw_ptr, pzw_ptr])
    pz_new_import(BreakShiftTag, break_shift_tag_qname, !PZ),

    % pz_signature([pzw_ptr], [pzw_ptr])
    pz_new_import(UnshiftValue, unshift_value_qname, !PZ),

    STagStruct = pz_struct([pzw_fast]),
    pz_new_struct_id(STagStructId, "Secondary tag struct", !PZ),
    pz_add_struct(STagStructId, STagStruct, !PZ),

    BuiltinProcs = pz_builtin_ids(MakeTag, ShiftMakeTag, BreakTag,
        BreakShiftTag, UnshiftValue, STagStructId).

%-----------------------------------------------------------------------%

:- func make_tag_qname = q_name.

make_tag_qname = q_name_append_str(builtin_module_name, "make_tag").

:- func shift_make_tag_qname = q_name.

shift_make_tag_qname = q_name_append_str(builtin_module_name, "shift_make_tag").

:- func break_tag_qname = q_name.

break_tag_qname = q_name_append_str(builtin_module_name, "break_tag").

:- func break_shift_tag_qname = q_name.

break_shift_tag_qname = q_name_append_str(builtin_module_name, "break_shift_tag").

:- func unshift_value_qname = q_name.

unshift_value_qname = q_name_append_str(builtin_module_name, "unshift_value").

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
