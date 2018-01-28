%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module compile_error.
%
% Copyright (C) 2015-2018 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
% This module defines possible Plasma compilation errors.
%
%-----------------------------------------------------------------------%
:- interface.

:- import_module list.
:- import_module maybe.
:- import_module string.

:- import_module common_types.
:- import_module core.
:- import_module core.resource.
:- import_module result.

%-----------------------------------------------------------------------%

:- type compile_error
    --->    ce_function_already_defined(string)
    ;       ce_type_already_defined(string)
    ;       ce_type_not_known(string)
    ;       ce_type_has_incorrect_num_of_args(string, int, int)
    ;       ce_builtin_type_with_args(string)
    ;       ce_uses_observes_not_distinct(list(resource))
    ;       ce_type_var_with_args(string)
    ;       ce_match_has_no_cases
    ;       ce_match_does_not_cover_all_cases
    ;       ce_match_unreached_cases
    ;       ce_match_duplicate_case
    ;       ce_match_on_function_type
    ;       ce_arity_mismatch_func(arity, arity)
    ;       ce_arity_mismatch_expr(arity, arity)
    ;       ce_arity_mismatch_tuple
    ;       ce_arity_mismatch_match(list(maybe(arity)))
    ;       ce_parameter_number(int, int)
    ;       ce_resource_unavailable
    ;       ce_too_many_bangs_in_statement
    ;       ce_no_bang
    ;       ce_no_return_statement(arity).

:- instance error(compile_error).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

:- instance error(compile_error) where [
    error_or_warning(_) = error,
    func(to_string/1) is ce_to_string
].

:- func ce_to_string(compile_error) = string.

ce_to_string(ce_function_already_defined(Name)) =
    format("Function already defined: %s", [s(Name)]).
ce_to_string(ce_type_already_defined(Name)) =
    format("Type already defined: %s", [s(Name)]).
ce_to_string(ce_type_not_known(Name)) =
    format("Unknown type: %s", [s(Name)]).
ce_to_string(ce_type_has_incorrect_num_of_args(Name, Want, Got)) =
    format("Wrong number of type args for '%s', expected: %d, got: %d",
        [s(Name), i(Want), i(Got)]).
ce_to_string(ce_builtin_type_with_args(Name)) =
    format("Builtin type '%s' does not take arguments", [s(Name)]).
ce_to_string(ce_type_var_with_args(Name)) =
    format("Type variables (like '%s') cannot take arguments", [s(Name)]).
ce_to_string(ce_match_has_no_cases) =
    "Match expression has no cases".
ce_to_string(ce_match_does_not_cover_all_cases) =
    "Match does not cover all cases".
ce_to_string(ce_match_unreached_cases) =
    "This case will never be tested because erlier cases cover all values".
ce_to_string(ce_match_duplicate_case) =
    "This case occurs multiple times in this match".
ce_to_string(ce_match_on_function_type) =
    "Attempt to pattern match on a function".
ce_to_string(ce_uses_observes_not_distinct(Resources)) =
    format("A resource cannot appear in both the uses and observes " ++
            "lists," ++
            " found resources: %s",
        [s(join_list(", ", map(resource_to_string, Resources)))]).
ce_to_string(ce_arity_mismatch_func(Decl, Infer)) =
    format("Function has %d declared results but returns %d results",
        [i(Decl ^ a_num), i(Infer ^ a_num)]).
ce_to_string(ce_arity_mismatch_expr(Got, Expect)) =
    format("Expression returns %d values, but %d values were expected",
        [i(Got ^ a_num), i(Expect ^ a_num)]).
ce_to_string(ce_arity_mismatch_tuple) =
    "Arity mismatch in tuple, could be called by arguments to call".
ce_to_string(ce_arity_mismatch_match(Arities)) =
    "Match expression has cases with different arrites, they are " ++
    string.join_list(", ", map(
        (func(MA) = S :-
            ( MA = yes(A), S = string(A ^ a_num)
            ; MA = no,     S = "_"
            )
        ), Arities)).
ce_to_string(ce_parameter_number(Exp, Got)) =
    format("Wrong number of parameters in function call, "
            ++ "expected %d got %d",
        [i(Exp), i(Got)]).
ce_to_string(ce_resource_unavailable) =
    "One or more resources needed for this call is unavailable in this " ++
    "function".
ce_to_string(ce_too_many_bangs_in_statement) =
    "Statement has more than one ! call".
ce_to_string(ce_no_bang) =
    "Call uses or observes a resource but has no !".
ce_to_string(ce_no_return_statement(Arity)) =
    format("Function returns %d results but this path has no return statement",
        [i(Arity ^ a_num)]).

%-----------------------------------------------------------------------%
