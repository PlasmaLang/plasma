%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module compile_error.
%
% Copyright (C) 2015-2016 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
% This module defines possible Plasma compilation errors.
%
%-----------------------------------------------------------------------%
:- interface.

:- import_module set.
:- import_module string.

:- import_module common_types.
:- import_module core.
:- import_module result.

%-----------------------------------------------------------------------%

:- type compile_error
    --->    ce_function_already_defined(string)
    ;       ce_builtin_type_with_args(string)
    ;       ce_using_observing_not_distinct(set(resource))
    ;       ce_type_var_with_args(string)
    ;       ce_arity_mismatch_func(arity, arity)
    ;       ce_arity_mismatch_expr(arity, arity)
    ;       ce_parameter_number(int, int).

:- instance error(compile_error).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

:- import_module list.

:- instance error(compile_error) where [
    error_or_warning(_) = error,
    func(to_string/1) is ce_to_string
].

:- func ce_to_string(compile_error) = string.

ce_to_string(ce_function_already_defined(Name)) =
    format("Function already defined: %s", [s(Name)]).
ce_to_string(ce_builtin_type_with_args(Name)) =
    format("Builtin type '%s' does not take arguments", [s(Name)]).
ce_to_string(ce_type_var_with_args(Name)) =
    format("Type variables (like '%s') cannot take arguments", [s(Name)]).
ce_to_string(ce_using_observing_not_distinct(Resources)) =
    format("A resource cannot appear in both the using and observing " ++
            "lists," ++
            " found resources: %s",
        [s(join_list(", ", map(resource_to_string, to_sorted_list(Resources))))]).
ce_to_string(ce_arity_mismatch_func(Decl, Infer)) =
    format("Function has %d declared results but returns %d results",
        [i(Decl ^ a_num), i(Infer ^ a_num)]).
ce_to_string(ce_arity_mismatch_expr(Got, Expect)) =
    format("Expression returns %d values, but %d values were expected",
        [i(Got ^ a_num), i(Expect ^ a_num)]).
ce_to_string(ce_parameter_number(Exp, Got)) =
    format("Wrong number of parameters in function call, "
            ++ "expected %d got %d",
        [i(Exp), i(Got)]).

%-----------------------------------------------------------------------%
