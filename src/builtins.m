%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module builtins.
%
% Copyright (C) 2015-2016 Plasma Team
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
:- import_module pz.
:- import_module q_name.

:- type builtin_item
    --->    bi_func(func_id)
    ;       bi_ctor(ctor_id).

    % setup_builtins(Map, BoolTrue, BoolFalse, !Core)
    %
:- pred setup_builtins(map(q_name, builtin_item)::out,
    ctor_id::out, ctor_id::out, core::in, core::out) is det.

:- func builtin_module_name = q_name.

:- func builtin_add_int = q_name.
:- func builtin_sub_int = q_name.
:- func builtin_mul_int = q_name.
:- func builtin_div_int = q_name.
:- func builtin_mod_int = q_name.
:- func builtin_lshift_int = q_name.
:- func builtin_rshift_int = q_name.
:- func builtin_and_int = q_name.
:- func builtin_or_int = q_name.
:- func builtin_xor_int = q_name.
:- func builtin_gt_int = q_name.
:- func builtin_lt_int = q_name.
:- func builtin_gteq_int = q_name.
:- func builtin_lteq_int = q_name.
:- func builtin_eq_int = q_name.
:- func builtin_neq_int = q_name.
:- func builtin_and_bool = q_name.
:- func builtin_or_bool = q_name.
:- func builtin_concat_string = q_name.

% Unary operators.
:- func builtin_minus_int = q_name.
:- func builtin_comp_int = q_name.
:- func builtin_not_bool = q_name.

%-----------------------------------------------------------------------%
%
% PZ Builtins
%

:- type builtin_procs
    --->    builtin_procs(
                bp_make_tag         :: pzp_id,
                bp_shift_make_tag   :: pzp_id,
                bp_break_tag        :: pzp_id,
                bp_break_shift_tag  :: pzp_id,
                bp_unshift_value    :: pzp_id
            ).

    % Setup procedures that are PZ builtins but not Plasma builtins.
    %
:- pred setup_builtin_procs(builtin_procs::out, pz::in, pz::out) is det.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module maybe.
:- import_module require.
:- import_module set.
:- import_module string.

:- import_module context.
:- import_module core.code.
:- import_module core.function.
:- import_module core.types.
:- import_module pz.code.
:- import_module varmap.

%-----------------------------------------------------------------------%

setup_builtins(!:Map, BoolTrue, BoolFalse, !Core) :-
    !:Map = init,
    setup_bool_builtins(BoolType, BoolTrue, BoolFalse, !Map, !Core),
    setup_int_builtins(BoolType, !Map, !Core),
    setup_misc_builtins(BoolType, BoolTrue, BoolFalse, !Map, !Core).

:- pred setup_bool_builtins(type_id::out, ctor_id::out, ctor_id::out,
    map(q_name, builtin_item)::in,
    map(q_name, builtin_item)::out, core::in, core::out) is det.

setup_bool_builtins(BoolId, TrueId, FalseId, !Map, !Core) :-
    core_allocate_type_id(BoolId, !Core),

    FalseName = q_name("False"),
    core_allocate_ctor_id(FalseId, FalseName, !Core),
    core_set_constructor(BoolId, FalseId, constructor(FalseName, []), !Core),
    det_insert(FalseName, bi_ctor(FalseId), !Map),

    TrueName = q_name("True"),
    core_allocate_ctor_id(TrueId, TrueName, !Core),
    core_set_constructor(BoolId, TrueId, constructor(TrueName, []), !Core),
    det_insert(TrueName, bi_ctor(TrueId), !Map),

    % NOTE: False is first so that it is allocated 0 for its tag, this will
    % make interoperability easier.
    core_set_type(BoolId,
        init(q_name_snoc(builtin_module_name, "Bool"),
            [FalseId, TrueId]),
        !Core),

    NotName = q_name("not_bool"),
    register_builtin_func(NotName,
        func_init(q_name_append(builtin_module_name, NotName), nil_context,
            s_private,
            [type_ref(BoolId)], [type_ref(BoolId)], init,
            init),
        _, !Map, !Core),

    foldl2(register_bool_biop(BoolId), [
        builtin_and_bool,
        builtin_or_bool], !Map, !Core).

:- pred register_bool_biop(type_id::in, q_name::in,
    map(q_name, builtin_item)::in, map(q_name, builtin_item)::out,
    core::in, core::out) is det.

register_bool_biop(BoolType, Name, !Map, !Core) :-
    FName = q_name_append(builtin_module_name, Name),
    register_builtin_func(Name,
        func_init(FName, nil_context, s_private,
            [type_ref(BoolType), type_ref(BoolType)],
            [type_ref(BoolType)],
            init, init),
        _, !Map, !Core).

:- pred setup_int_builtins(type_id::in, map(q_name, builtin_item)::in,
    map(q_name, builtin_item)::out, core::in, core::out) is det.

setup_int_builtins(BoolType, !Map, !Core) :-
    foldl2(register_int_biop, [
        builtin_add_int,
        builtin_sub_int,
        builtin_mul_int,
        builtin_div_int,
        builtin_mod_int,
        % TODO: make the number of bits to shift a single byte.
        builtin_lshift_int,
        builtin_rshift_int,
        builtin_and_int,
        builtin_or_int,
        builtin_xor_int], !Map, !Core),

    foldl2(register_int_comp(BoolType), [
        builtin_gt_int,
        builtin_lt_int,
        builtin_gteq_int,
        builtin_lteq_int,
        builtin_eq_int,
        builtin_neq_int], !Map, !Core),

    foldl2(register_int_uop, [
        builtin_minus_int,
        builtin_comp_int], !Map, !Core).

:- pred register_int_biop(q_name::in,
    map(q_name, builtin_item)::in, map(q_name, builtin_item)::out,
    core::in, core::out) is det.

register_int_biop(Name, !Map, !Core) :-
    FName = q_name_append(builtin_module_name, Name),
    register_builtin_func(Name,
        func_init(FName, nil_context, s_private,
            [builtin_type(int), builtin_type(int)],
            [builtin_type(int)],
            init, init),
        _, !Map, !Core).

:- pred register_int_comp(type_id::in, q_name::in,
    map(q_name, builtin_item)::in, map(q_name, builtin_item)::out,
    core::in, core::out) is det.

register_int_comp(BoolType, Name, !Map, !Core) :-
    FName = q_name_append(builtin_module_name, Name),
    register_builtin_func(Name,
        func_init(FName, nil_context, s_private,
            [builtin_type(int), builtin_type(int)],
            [type_ref(BoolType)],
            init, init),
        _, !Map, !Core).

:- pred register_int_uop(q_name::in,
    map(q_name, builtin_item)::in, map(q_name, builtin_item)::out,
    core::in, core::out) is det.

register_int_uop(Name, !Map, !Core) :-
    FName = q_name_append(builtin_module_name, Name),
    register_builtin_func(Name,
        func_init(FName, nil_context, s_private,
            [builtin_type(int)], [builtin_type(int)],
            init, init),
        _, !Map, !Core).

:- pred setup_misc_builtins(type_id::in, ctor_id::in, ctor_id::in,
    map(q_name, builtin_item)::in, map(q_name, builtin_item)::out,
    core::in, core::out) is det.

setup_misc_builtins(BoolType, BoolTrue, BoolFalse, !Map, !Core) :-
    PrintName = q_name_snoc(builtin_module_name, "print"),
    register_builtin_func(q_name("print"),
        func_init(PrintName, nil_context, s_private,
            [builtin_type(string)], [], set([r_io]), init),
        _, !Map, !Core),


    IntToStringName = q_name_snoc(builtin_module_name, "int_to_string"),
    register_builtin_func(q_name("int_to_string"),
        func_init(IntToStringName, nil_context, s_private,
            [builtin_type(int)], [builtin_type(string)], init, init),
        _, !Map, !Core),

    BoolToStringName = q_name_snoc(builtin_module_name, "bool_to_string"),
    BoolToString0 = func_init(BoolToStringName, nil_context, s_private,
        [type_ref(BoolType)], [builtin_type(string)], init, init),
    define_bool_to_string(BoolTrue, BoolFalse, BoolToString0, BoolToString),
    register_builtin_func(q_name("bool_to_string"), BoolToString,
        _, !Map, !Core),

    FreeName = q_name_snoc(builtin_module_name, "free"),
    register_builtin_func(q_name("free"),
        func_init(FreeName, nil_context, s_private,
            [builtin_type(string)], [], set([r_io]), init),
        _, !Map, !Core),

    ConcatStringName = q_name_append(builtin_module_name,
        builtin_concat_string),
    register_builtin_func(ConcatStringName,
        func_init(ConcatStringName, nil_context, s_private,
            [builtin_type(string), builtin_type(string)],
            [builtin_type(string)],
            init, init),
        _, !Map, !Core),

    DieName = q_name_snoc(builtin_module_name, "die"),
    register_builtin_func(DieName,
        func_init(DieName, nil_context, s_private,
            [builtin_type(string)], [], init, init),
        _, !Map, !Core).

:- pred register_builtin_func(q_name::in, function::in, func_id::out,
    map(q_name, builtin_item)::in, map(q_name, builtin_item)::out,
    core::in, core::out) is det.

register_builtin_func(Name, Func, FuncId, !Map, !Core) :-
    core_allocate_function(FuncId, !Core),
    core_set_function(FuncId, Func, !Core),
    det_insert(Name, bi_func(FuncId), !Map).

%-----------------------------------------------------------------------%

:- pred define_bool_to_string(ctor_id::in, ctor_id::in,
    function::in, function::out) is det.

define_bool_to_string(TrueId, FalseId, !Func) :-
    some [!Varmap] (
        !:Varmap = init,
        CI = code_info_init(nil_context),

        varmap.add_anon_var(In, !Varmap),
        TrueCase = e_case(p_ctor(TrueId, []),
            expr(e_constant(c_string("True")), CI)),
        FalseCase = e_case(p_ctor(FalseId, []),
            expr(e_constant(c_string("False")), CI)),
        Expr = expr(e_match(In, [TrueCase, FalseCase]), CI),
        func_set_body(!.Varmap, [In], Expr, !Func)
    ).

%-----------------------------------------------------------------------%

builtin_module_name = q_name("builtin").

builtin_add_int = q_name("add_int").
builtin_sub_int = q_name("sub_int").
builtin_mul_int = q_name("mul_int").
builtin_div_int = q_name("div_int").
builtin_mod_int = q_name("mod_int").
builtin_lshift_int = q_name("lshift_int").
builtin_rshift_int = q_name("rshift_int").
builtin_and_int = q_name("and_int").
builtin_or_int = q_name("or_int").
builtin_xor_int = q_name("xor_int").
builtin_gt_int = q_name("gt_int").
builtin_lt_int = q_name("lt_int").
builtin_gteq_int = q_name("gteq_int").
builtin_lteq_int = q_name("lteq_int").
builtin_eq_int = q_name("eq_int").
builtin_neq_int = q_name("neq_int").
builtin_and_bool = q_name("and_bool").
builtin_or_bool = q_name("or_bool").
builtin_concat_string = q_name("concat_string").

builtin_minus_int = q_name("minus_int").
builtin_comp_int = q_name("comp_int").
builtin_not_bool = q_name("not_bool").

%-----------------------------------------------------------------------%

setup_builtin_procs(BuiltinProcs, !PZ) :-
    pz_new_proc_id(i_imported, MakeTag, !PZ),
    pz_add_proc(MakeTag, pz_proc(make_tag_qname,
        pz_signature([pzw_ptr, pzw_ptr], [pzw_ptr]), no), !PZ),

    pz_new_proc_id(i_imported, ShiftMakeTag, !PZ),
    pz_add_proc(ShiftMakeTag, pz_proc(shift_make_tag_qname,
        pz_signature([pzw_ptr, pzw_ptr], [pzw_ptr]), no), !PZ),

    pz_new_proc_id(i_imported, BreakTag, !PZ),
    pz_add_proc(BreakTag, pz_proc(break_tag_qname,
        pz_signature([pzw_ptr], [pzw_ptr, pzw_ptr]), no), !PZ),

    pz_new_proc_id(i_imported, BreakShiftTag, !PZ),
    pz_add_proc(BreakShiftTag, pz_proc(break_shift_tag_qname,
        pz_signature([pzw_ptr], [pzw_ptr, pzw_ptr]), no), !PZ),

    pz_new_proc_id(i_imported, UnshiftValue, !PZ),
    pz_add_proc(UnshiftValue, pz_proc(unshift_value_qname,
        pz_signature([pzw_ptr], [pzw_ptr]), no), !PZ),

    BuiltinProcs = builtin_procs(MakeTag, ShiftMakeTag, BreakTag,
        BreakShiftTag, UnshiftValue).

%-----------------------------------------------------------------------%

:- func make_tag_qname = q_name.

make_tag_qname = q_name_snoc(builtin_module_name, "make_tag").

:- func shift_make_tag_qname = q_name.

shift_make_tag_qname = q_name_snoc(builtin_module_name, "shift_make_tag").

:- func break_tag_qname = q_name.

break_tag_qname = q_name_snoc(builtin_module_name, "break_tag").

:- func break_shift_tag_qname = q_name.

break_shift_tag_qname = q_name_snoc(builtin_module_name, "break_shift_tag").

:- func unshift_value_qname = q_name.

unshift_value_qname = q_name_snoc(builtin_module_name, "unshift_value").

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
