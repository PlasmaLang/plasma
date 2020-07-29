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

:- import_module compile_error.
:- import_module util.result.

    % Process all non-imported functions that havn't generated errors in
    % prior passes.
    %
:- pred process_noerror_funcs(
    pred(core, func_id, function, result_partial(function, compile_error)),
    errors(compile_error),  core, core).
:- mode process_noerror_funcs(
    pred(in, in, in, out) is det,
    out, in, out) is det.

:- pred process_noerror_scc_funcs(
    pred(core, func_id, function, result_partial(function, compile_error)),
    errors(compile_error),  core, core).
:- mode process_noerror_scc_funcs(
    pred(in, in, in, out) is det,
    out, in, out) is det.

:- pred check_noerror_funcs(
    func(core, func_id, function) = errors(compile_error),
    errors(compile_error), core, core).
:- mode check_noerror_funcs(
    func(in, in, in) = (out) is det,
    out, in, out) is det.

%-----------------------------------------------------------------------%

:- pred create_anon_var_with_type(type_::in, var::out,
    varmap::in, varmap::out, map(var, type_)::in, map(var, type_)::out) is det.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
:- implementation.

:- import_module cord.

%-----------------------------------------------------------------------%

process_noerror_funcs(Pred, Errors, !Core) :-
    FuncIds = core_all_defined_functions(!.Core),
    map_foldl(process_func(Pred), FuncIds, ErrorsList, !Core),
    Errors = cord_list_to_cord(ErrorsList).

:- pred process_func(
    pred(core, func_id, function, result_partial(function, compile_error)),
    func_id, errors(compile_error), core, core).
:- mode process_func(
    pred(in, in, in, out) is det,
    in, out, in, out) is det.

process_func(Pred, FuncId, Errors, !Core) :-
    core_get_function_det(!.Core, FuncId, Func0),
    ( if not func_has_error(Func0) then
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

process_noerror_scc_funcs(Pred, Errors, !Core) :-
    SCCs = core_all_defined_functions_sccs(!.Core),
    FuncIds = condense(map(to_sorted_list, reverse(SCCs))),
    map_foldl(process_func(Pred), FuncIds, ErrorsList, !Core),
    Errors = cord_list_to_cord(ErrorsList).

%-----------------------------------------------------------------------%

check_noerror_funcs(Func, Errors, !Core) :-
    process_noerror_funcs(
        (pred(C::in, Id::in, F::in, R::out) is det :-
            ErrorsI = Func(C, Id, F),
            ( if has_fatal_errors(ErrorsI) then
                R = errors(ErrorsI)
            else
                R = ok(F, ErrorsI)
            )
        ), Errors, !Core).

%-----------------------------------------------------------------------%

create_anon_var_with_type(Type, Var, !Varmap, !Vartypes) :-
    add_anon_var(Var, !Varmap),
    det_insert(Var, Type, !Vartypes).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
