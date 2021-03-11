%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module compile_error.
%
% Copyright (C) 2015-2018, 2020 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
% This module defines possible Plasma compilation errors.
%
%-----------------------------------------------------------------------%
:- interface.

:- import_module list.
:- import_module maybe.

:- import_module common_types.
:- import_module core.
:- import_module core.resource.
:- import_module parse_util.
:- import_module q_name.
:- import_module util.
:- import_module util.result.

%-----------------------------------------------------------------------%

:- type compile_error
            % This creates a dependency on the parser, I'm uneasy about
            % this.
    --->    ce_read_source_error(read_src_error)
    ;       ce_invalid_module_name(q_name)
    ;       ce_source_file_name_not_match_module(q_name, string)
    ;       ce_object_file_name_not_match_module(q_name, string)
    ;       ce_module_not_found(q_name)
    ;       ce_module_unavailable(q_name, q_name)
    ;       ce_interface_contains_wrong_module(string, q_name, q_name)
    ;       ce_import_would_clobber(q_name)
    ;       ce_function_already_defined(string)
    ;       ce_entry_function_wrong_signature
    ;       ce_type_already_defined(q_name)
    ;       ce_type_not_known(q_name)
    ;       ce_type_has_incorrect_num_of_args(q_name, int, int)
    ;       ce_builtin_type_with_args(q_name)
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
    ;       ce_resource_unavailable_call
    ;       ce_resource_unavailable_arg
    ;       ce_resource_unavailable_output
    ;       ce_resource_unknown(q_name)
    ;       ce_resource_not_public_in_resource(nq_name, nq_name)
    ;       ce_resource_not_public(q_name)
    ;       ce_too_many_bangs_in_statement
    ;       ce_no_bang
    ;       ce_unnecessary_bang
    ;       ce_no_return_statement(arity).

:- instance error(compile_error).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
:- implementation.

:- import_module string.

%-----------------------------------------------------------------------%

:- instance error(compile_error) where [
    func(error_or_warning/1) is ce_error_or_warning,
    func(to_string/1) is ce_to_string
].

:- func ce_error_or_warning(compile_error) = error_or_warning.

ce_error_or_warning(Error) =
    ( if Error = ce_unnecessary_bang then
        warning
    else
        error
    ).

:- func ce_to_string(compile_error) = string.

ce_to_string(ce_read_source_error(E)) =
    to_string(E).
ce_to_string(ce_invalid_module_name(Name)) =
    format("'%s' is not a valid module name", [s(q_name_to_string(Name))]).
ce_to_string(ce_source_file_name_not_match_module(Expect, Got)) =
    format("The source filename `%s` does not match the module name `%s`",
        [s(Got), s(q_name_to_string(Expect))]).
ce_to_string(ce_object_file_name_not_match_module(Expect, Got)) =
    format("The output filename `%s` does not match the module name `%s`",
        [s(Got), s(q_name_to_string(Expect))]).
ce_to_string(ce_module_not_found(Name)) =
    format("The interface file for the imported module (%s) cannot be found.  Was the module listed in BUILD.plz?",
        [s(q_name_to_string(Name))]).
ce_to_string(ce_module_unavailable(Importee, Importer)) =
    format("The module %s can't be included because it is not listed in all the build file's module lists that include module %s",
        [s(q_name_to_string(Importee)), s(q_name_to_string(Importer))]).
ce_to_string(ce_interface_contains_wrong_module(File, Expect, Got)) =
    format("The interface file '%s' describes the wrong module, " ++
        "got: '%s' expected: '%s'",
        [s(File), s(q_name_to_string(Got)), s(q_name_to_string(Expect))]).
ce_to_string(ce_import_would_clobber(ModuleName)) =
    format("Thie import of '%s' would clobber a previous import of " ++
            "the same module",
        [s(q_name_to_string(ModuleName))]).
ce_to_string(ce_entry_function_wrong_signature) =
    "A function that is marked as an entrypoint does not have the correct " ++
    "signature for an entrypoint.".

ce_to_string(ce_function_already_defined(Name)) =
    format("Function already defined: %s", [s(Name)]).
ce_to_string(ce_type_already_defined(Name)) =
    format("Type already defined: %s", [s(q_name_to_string(Name))]).
ce_to_string(ce_type_not_known(Name)) =
    format("Unknown type: %s", [s(q_name_to_string(Name))]).
ce_to_string(ce_type_has_incorrect_num_of_args(Name, Want, Got)) =
    format("Wrong number of type args for '%s', expected: %d, got: %d",
        [s(q_name_to_string(Name)), i(Want), i(Got)]).
ce_to_string(ce_builtin_type_with_args(Name)) =
    format("Builtin type '%s' does not take arguments",
        [s(q_name_to_string(Name))]).
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
ce_to_string(Error) = Message :-
    % These to errors are broken and can't be properly distinguished.
    ( Error = ce_arity_mismatch_func(Got, Expect)
    ; Error = ce_arity_mismatch_expr(Got, Expect)
    ),
    Message = format("Arity error got %d values, but %d values were expected",
        [i(Got ^ a_num), i(Expect ^ a_num)]).
%ce_to_string(ce_arity_mismatch_func(Decl, Infer)) =
%    format("Function has %d declared results but returns %d results",
%        [i(Decl ^ a_num), i(Infer ^ a_num)]).
%ce_to_string(ce_arity_mismatch_expr(Got, Expect)) =
%    format("Expression returns %d values, but %d values were expected",
%        [i(Got ^ a_num), i(Expect ^ a_num)]).
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
ce_to_string(ce_resource_unavailable_call) =
    "One or more resources needed for this call is unavailable in this " ++
    "function".
ce_to_string(ce_resource_unavailable_arg) =
    "One or more resources needed for an argument to a call is not " ++
    "provided in by the passed-in value".
ce_to_string(ce_resource_unavailable_output) =
    "The function returns a higher order value that uses or observes one " ++
    "or more resources, however the resources arn't declared in the " ++
    "function's return type".
ce_to_string(ce_resource_unknown(Res)) =
    format("Unknown resource '%s'", [s(q_name_to_string(Res))]).
ce_to_string(ce_resource_not_public_in_resource(Res, From)) =
    format("The resource %s is exported, but it depends on %s which is not",
        [s(nq_name_to_string(Res)), s(nq_name_to_string(From))]).
ce_to_string(ce_resource_not_public(Res)) =
    format("This function or type is exported, " ++
            "but it depends on the resource %s which is not",
        [s(q_name_to_string(Res))]).
ce_to_string(ce_too_many_bangs_in_statement) =
    "Statement has more than one ! call".
ce_to_string(ce_no_bang) =
    "Call uses or observes a resource but has no !".
ce_to_string(ce_unnecessary_bang) =
    "Call has a ! but does not need it".
ce_to_string(ce_no_return_statement(Arity)) =
    format("Function returns %d results but this path has no return statement",
        [i(Arity ^ a_num)]).

%-----------------------------------------------------------------------%
