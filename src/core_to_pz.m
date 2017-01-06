%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module core_to_pz.
%
% Copyright (C) 2015-2017 Plasma Team
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

:- import_module list.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module set.

:- import_module common_types.
:- import_module core.function.
:- import_module core.types.
:- import_module pz.code.
:- import_module varmap.

:- include_module core_to_pz.code.
:- include_module core_to_pz.data.
:- import_module core_to_pz.code.
:- import_module core_to_pz.data.

%-----------------------------------------------------------------------%

core_to_pz(BuiltinMap, Core, !:PZ) :-
    !:PZ = init_pz,
    FuncIds = core_all_functions(Core),

    % Generate constants.
    foldl2(gen_const_data(Core), FuncIds, init, DataMap, !PZ),

    % Make decisions about tagged pointers
    gen_type_ctor_tags(Core, TypeTagMap),

    % Generate functions.
    OpIdMap = builtin_operator_map(BuiltinMap),
    RealFuncIds = to_sorted_list(set(FuncIds) `difference`
        set(keys(OpIdMap))),
    setup_builtin_procs(BuiltinProcs, !PZ),
    foldl2(make_proc_id_map(Core), RealFuncIds, init, ProcIdMap, !PZ),
    map(gen_proc(Core, OpIdMap, ProcIdMap, BuiltinProcs, TypeTagMap, DataMap),
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
%-----------------------------------------------------------------------%
