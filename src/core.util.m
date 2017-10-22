%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module core.util.
%
% Copyright (C) 2017 Plasma Team
% Distributed under the terms of the MIT see ../LICENSE.code
%
% Utility code for the core stage.
%
%-----------------------------------------------------------------------%
:- interface.

:- import_module compile_error.
:- import_module result.

    % Process all non-imported functions that havn't generated errors in
    % prior passes.
    %
:- pred process_noerror_funcs(
    pred(core, func_id, function, result(function, compile_error)),
    errors(compile_error),  core, core).
:- mode process_noerror_funcs(
    pred(in, in, in, out) is det,
    out, in, out) is det.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
:- implementation.

:- import_module cord.

%-----------------------------------------------------------------------%

process_noerror_funcs(Pred, Errors, !Core) :-
    FuncIds = core_all_nonimported_functions(!.Core),
    map_foldl(process_func(Pred), FuncIds, ErrorsList, !Core),
    Errors = cord_list_to_cord(ErrorsList).

:- pred process_func(
    pred(core, func_id, function, result(function, compile_error)),
    func_id, errors(compile_error), core, core).
:- mode process_func(
    pred(in, in, in, out) is det,
    in, out, in, out) is det.

process_func(Pred, FuncId, Errors, !Core) :-
    core_get_function_det(!.Core, FuncId, Func0),
    ( if not func_has_error(Func0) then
        Pred(!.Core, FuncId, Func0, Result),
        ( Result = ok(Func),
            Errors = init
        ; Result = errors(Errors),
            func_raise_error(Func0, Func)
        ),
        core_set_function(FuncId, Func, !Core)
    else
        Errors = init
    ).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
