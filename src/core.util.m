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

:- pred process_funcs(
    pred(func_id, function, errors(compile_error), core, core),
    errors(compile_error),  core, core).
:- mode process_funcs(
    pred(in, in, out, in, out) is det,
    out, in, out) is det.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
:- implementation.

:- import_module cord.

%-----------------------------------------------------------------------%

process_funcs(Pred, Errors, !Core) :-
    FuncIds = core_all_nonimported_functions(!.Core),
    map_foldl(process_func(Pred), FuncIds, ErrorsList, !Core),
    Errors = cord_list_to_cord(ErrorsList).

:- pred process_func(
    pred(func_id, function, errors(compile_error), core, core),
    func_id, errors(compile_error), core, core).
:- mode process_func(
    pred(in, in, out, in, out) is det,
    in, out, in, out) is det.

process_func(Pred, FuncId, Errors, !Core) :-
    core_get_function_det(!.Core, FuncId, Func),
    Pred(FuncId, Func, Errors, !Core).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
