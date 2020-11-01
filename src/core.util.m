%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module core.util.
%
% Copyright (C) 2017-2018, 2020 Plasma Team
% Distributed under the terms of the MIT see ../LICENSE.code
%
% Utility code for the core stage.
%
%-----------------------------------------------------------------------%
:- interface.

:- import_module io.

:- import_module compile_error.
:- import_module util.log.
:- import_module util.result.

%-----------------------------------------------------------------------%

    % Process all non-imported functions that havn't generated errors in
    % prior passes.
    %
:- pred process_noerror_funcs(log_config,
    pred(core, func_id, function, result_partial(function, compile_error)),
    errors(compile_error),  core, core, io, io).
:- mode process_noerror_funcs(in,
    pred(in, in, in, out) is det,
    out, in, out, di, uo) is det.

:- pred process_noerror_scc_funcs(log_config,
    pred(core, func_id, function, result_partial(function, compile_error)),
    errors(compile_error),  core, core, io, io).
:- mode process_noerror_scc_funcs(in,
    pred(in, in, in, out) is det,
    out, in, out, di, uo) is det.

:- pred check_noerror_funcs(log_config,
    func(core, func_id, function) = errors(compile_error),
    errors(compile_error), core, core, io, io).
:- mode check_noerror_funcs(in,
    func(in, in, in) = (out) is det,
    out, in, out, di, uo) is det.

%-----------------------------------------------------------------------%

:- pred create_anon_var_with_type(type_::in, var::out,
    varmap::in, varmap::out, map(var, type_)::in, map(var, type_)::out) is det.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
:- implementation.

:- import_module cord.
:- import_module string.

%-----------------------------------------------------------------------%

process_noerror_funcs(Verbose, Pred, Errors, !Core, !IO) :-
    Funcs = core_all_defined_functions(!.Core),
    map_foldl2(process_func(Verbose, Pred), Funcs, ErrorsList, !Core, !IO),
    Errors = cord_list_to_cord(ErrorsList).

:- pred process_func(log_config,
    pred(core, func_id, function, result_partial(function, compile_error)),
    pair(func_id, function), errors(compile_error), core, core, io, io).
:- mode process_func(in,
    pred(in, in, in, out) is det,
    in, out, in, out, di, uo) is det.

process_func(Verbose, Pred, FuncId - Func0, Errors, !Core, !IO) :-
    ( if not func_has_error(Func0) then
        FuncName = func_get_name(Func0),
        verbose_output(Verbose,
            format("  processing %s\n", [s(q_name_to_string(FuncName))]),
            !IO),
        Pred(!.Core, FuncId, Func0, Result),
        ( Result = ok(Func, Errors)
        ; Result = errors(Errors),
            func_raise_error(Func0, Func)
        ),
        core_set_function(FuncId, Func, !Core)
    else
        Errors = init
    ).

%-----------------------------------------------------------------------%

process_noerror_scc_funcs(Verbose, Pred, Errors, !Core, !IO) :-
    SCCs = core_all_defined_functions_sccs(!.Core),
    FuncIds = map(make_func_pair(!.Core),
        condense(map(to_sorted_list, reverse(SCCs)))),
    map_foldl2(process_func(Verbose, Pred), FuncIds, ErrorsList, !Core, !IO),
    Errors = cord_list_to_cord(ErrorsList).

:- func make_func_pair(core, func_id) = pair(func_id, function).

make_func_pair(Core, FuncId) = FuncId - Func :-
    core_get_function_det(Core, FuncId, Func).

%-----------------------------------------------------------------------%

check_noerror_funcs(Verbose, Func, Errors, !Core, !IO) :-
    process_noerror_funcs(Verbose,
        (pred(C::in, Id::in, F::in, R::out) is det :-
            ErrorsI = Func(C, Id, F),
            ( if has_fatal_errors(ErrorsI) then
                R = errors(ErrorsI)
            else
                R = ok(F, ErrorsI)
            )
        ), Errors, !Core, !IO).

%-----------------------------------------------------------------------%

create_anon_var_with_type(Type, Var, !Varmap, !Vartypes) :-
    add_anon_var(Var, !Varmap),
    det_insert(Var, Type, !Vartypes).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
