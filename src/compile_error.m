%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module compile_error.
%
% Copyright (C) 2015-2018, 2020-2021 Plasma Team
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
:- import_module util.pretty.
:- import_module util.result.

%-----------------------------------------------------------------------%

:- type compile_error
    % Errors for reading source code or the organisation of code (module and
    % file names don't match, etc).
            % This creates a dependency on the parser, I'm uneasy about
            % this.
    --->    ce_read_source_error(read_src_error)
    ;       ce_invalid_module_name(q_name)
    ;       ce_source_file_name_not_match_module(q_name, string)
    ;       ce_object_file_name_not_match_module(q_name, string)
    ;       ce_module_not_found(q_name)
    ;       ce_interface_contains_wrong_module(string, q_name, q_name)
    ;       ce_import_would_clobber(q_name)

    % Generic errors with the binding of symbols.
    ;       ce_function_already_defined(string)
    ;       ce_main_function_wrong_signature

    % Type related errors
    ;       ce_type_already_defined(q_name)
    ;       ce_type_duplicate_constructor(q_name)
    ;       ce_type_not_known(q_name)
    ;       ce_type_var_unknown(string)
    ;       ce_type_has_incorrect_num_of_args(q_name, int, int)
    ;       ce_builtin_type_with_args(q_name)
    ;       ce_type_var_with_args(string)
    ;       ce_type_unification_failed(pretty, pretty)
    ;       ce_type_unification_occurs(pretty, pretty)

    % Pattern matching
    ;       ce_match_has_no_cases
    ;       ce_match_does_not_cover_all_cases
    ;       ce_match_unreached_cases
    ;       ce_match_duplicate_case
    ;       ce_match_on_function_type

    % Arity related.
    ;       ce_arity_mismatch_func(arity, arity)
    ;       ce_arity_mismatch_expr(arity, arity)
    ;       ce_arity_mismatch_tuple
    ;       ce_arity_mismatch_match(list(maybe(arity)))
    ;       ce_parameter_number(int, int)
    ;       ce_no_return_statement(arity)

    % Resource system
    ;       ce_uses_observes_not_distinct(list(resource))
    ;       ce_resource_unavailable_call
    ;       ce_resource_unavailable_arg
    ;       ce_resource_unavailable_output
    ;       ce_resource_unknown(q_name)
    ;       ce_resource_not_public_in_resource(nq_name, nq_name)
    ;       ce_resource_not_public(q_name)
    ;       ce_too_many_bangs_in_statement
    ;       ce_no_bang
    ;       ce_unnecessary_bang.

:- instance error(compile_error).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
:- implementation.

:- import_module cord.
:- import_module string.

%-----------------------------------------------------------------------%

:- instance error(compile_error) where [
    func(error_or_warning/1) is ce_error_or_warning,
    func(pretty/1) is ce_to_pretty
].

:- func ce_error_or_warning(compile_error) = error_or_warning.

ce_error_or_warning(Error) =
    ( if
        ( Error = ce_unnecessary_bang
        ; Error = ce_main_function_wrong_signature
        )
    then
        warning
    else
        error
    ).

:- func ce_to_pretty(compile_error) = list(pretty).

ce_to_pretty(ce_read_source_error(E)) = pretty(E).
ce_to_pretty(ce_invalid_module_name(Name)) =
    [p_quote("'", q_name_pretty(Name)),
     p_spc] ++ p_words("is not a valid module name").
ce_to_pretty(ce_source_file_name_not_match_module(Expect, Got)) =
    p_words("The source filename") ++ p_spc_nl ++
        [p_quote("'", p_str(Got))] ++ p_spc_nl ++
        p_words("does not match the module name") ++ p_spc_nl ++
        [p_quote("'", q_name_pretty(Expect))].
ce_to_pretty(ce_object_file_name_not_match_module(Expect, Got)) =
    p_words("The output filename") ++ p_spc_nl ++
        [p_quote("`", p_str(Got))] ++ p_spc_nl ++
        p_words("does not match the module name") ++ p_spc_nl ++
        [p_quote("'", q_name_pretty(Expect))].
ce_to_pretty(ce_module_not_found(Name)) =
    p_words("The interface file for the imported module") ++ p_spc_nl ++
        [p_str("("), q_name_pretty(Name), p_str("),")] ++ p_spc_nl ++
        p_words("cannot be found").
ce_to_pretty(ce_interface_contains_wrong_module(File, Expect, Got)) =
    p_words("The interface file") ++ p_spc_nl ++
        [p_quote("'", p_str(File))] ++ p_spc_nl ++
        p_words("describes the wrong module, got:") ++ p_spc_nl ++
        [p_quote("'", q_name_pretty(Got))] ++ p_spc_nl ++
        [p_str("expected:")] ++ p_spc_nl ++
        [p_quote("'", q_name_pretty(Expect))].
ce_to_pretty(ce_import_would_clobber(ModuleName)) =
    p_words("Thie import of") ++ p_spc_nl ++
        [p_quote("'", q_name_pretty(ModuleName))] ++ p_spc_nl ++
    p_words("would clobber a previous import of the same module").

ce_to_pretty(ce_function_already_defined(Name)) =
    p_words("Function already defined:") ++ p_spc_nl ++
        [p_str(Name)].
ce_to_pretty(ce_main_function_wrong_signature) =
    p_words("An exported function named 'main' did not have the correct " ++
        "signature to be a program entrypoint.").

ce_to_pretty(ce_type_already_defined(Name)) =
    p_words("Type already defined: ") ++ p_spc_nl ++
        [q_name_pretty(Name)].
ce_to_pretty(ce_type_duplicate_constructor(Name)) =
    p_words("This type already has a constructor named") ++ p_spc_nl ++
        [p_quote("'", q_name_pretty(Name))].
ce_to_pretty(ce_type_not_known(Name)) =
    p_words("Unknown type:") ++ p_spc_nl ++
        [q_name_pretty(Name)].
ce_to_pretty(ce_type_var_unknown(Name)) =
    p_words("Type variable") ++ p_spc_nl ++
        [p_quote("'", p_str(Name))] ++ p_spc_nl ++
        p_words("does not appear on left of '=' in type definition").
ce_to_pretty(ce_type_has_incorrect_num_of_args(Name, Want, Got)) =
    p_words("Wrong number of type args for ") ++ p_spc_nl ++
        [p_quote("'", q_name_pretty(Name)), p_str(",")] ++ p_spc_nl ++
        [p_str("expected: "), p_str(string(Want)), p_str(",")] ++ p_spc_nl ++
        [p_str("got: "), p_str(string(Got))].
ce_to_pretty(ce_builtin_type_with_args(Name)) =
    p_words("Builtin type") ++ p_spc_nl ++
        [p_quote("'", q_name_pretty(Name))] ++ p_spc_nl ++
        p_words("does not take arguments").
ce_to_pretty(ce_type_var_with_args(Name)) =
    p_words("Type variables (like") ++ p_spc_nl ++
        [p_quote("'", p_str(Name))] ++ p_spc_nl ++
        p_words("cannot take arguments").
ce_to_pretty(ce_type_unification_failed(Type1, Type2)) =
    [p_str("Type error:")] ++
        p_spc_nl ++ [p_quote("\"", Type1)] ++ p_spc_nl ++ [p_str("and")] ++
        p_spc_nl ++ [p_quote("\"", Type2)] ++ p_spc_nl ++
        p_words("are not the same").
ce_to_pretty(ce_type_unification_occurs(Var, Type)) =
    [p_str("Type error: "),
        p_str("The type "), p_quote("\"", Var)] ++ p_spc_nl ++
        p_words("cannot be bound to") ++ p_spc_nl ++
        [p_quote("\"", Type)] ++ p_spc_nl ++
        p_words("because it can't contain itself.").

ce_to_pretty(ce_match_has_no_cases) =
    p_words("Match expression has no cases").
ce_to_pretty(ce_match_does_not_cover_all_cases) =
    p_words("Match does not cover all cases").
ce_to_pretty(ce_match_unreached_cases) =
    p_words("This case will never be tested because erlier cases cover " ++
        "all values").
ce_to_pretty(ce_match_duplicate_case) =
    p_words("This case occurs multiple times in this match").
ce_to_pretty(ce_match_on_function_type) =
    p_words("Attempt to pattern match on a function").

ce_to_pretty(Error) = Pretty :-
    % These to errors are broken and can't be properly distinguished.
    ( Error = ce_arity_mismatch_func(Got, Expect)
    ; Error = ce_arity_mismatch_expr(Got, Expect)
    ),
    Pretty = p_words(format(
        "Arity error got %d values, but %d values were expected",
        [i(Got ^ a_num), i(Expect ^ a_num)])).
%ce_to_pretty(ce_arity_mismatch_func(Decl, Infer)) =
%    format("Function has %d declared results but returns %d results",
%        [i(Decl ^ a_num), i(Infer ^ a_num)]).
%ce_to_pretty(ce_arity_mismatch_expr(Got, Expect)) =
%    format("Expression returns %d values, but %d values were expected",
%        [i(Got ^ a_num), i(Expect ^ a_num)]).
ce_to_pretty(ce_arity_mismatch_tuple) =
    p_words("Arity mismatch in tuple, could be called by arguments to call").
ce_to_pretty(ce_arity_mismatch_match(Arities)) =
    p_words("Match expression has cases with different arrites, they are:") ++
    p_spc_nl ++ [p_expr(pretty_comma_seperated(
        map((func(MA) = S :-
                ( MA = yes(A), S = p_str(string(A ^ a_num))
                ; MA = no,     S = p_str("_")
                )
            ), Arities)
        ))].

ce_to_pretty(ce_parameter_number(Exp, Got)) =
    p_words(format("Wrong number of parameters in function call, "
            ++ "expected %d got %d",
        [i(Exp), i(Got)])).
ce_to_pretty(ce_no_return_statement(Arity)) =
    p_words(format(
        "Function returns %d results but this path has no return statement",
        [i(Arity ^ a_num)])).

ce_to_pretty(ce_uses_observes_not_distinct(Resources)) =
    p_words("A resource cannot appear in both the uses and observes " ++
            "lists, found resources:") ++
        p_spc_nl ++
        pretty_comma_seperated(map(func(R) = p_str(resource_to_string(R)),
            Resources)).
ce_to_pretty(ce_resource_unavailable_call) =
    p_words("One or more resources needed for this call is unavailable " ++
        "in this function").
ce_to_pretty(ce_resource_unavailable_arg) =
    p_words("One or more resources needed for an argument to a call " ++
        "is not provided in by the passed-in value").
ce_to_pretty(ce_resource_unavailable_output) =
    p_words("The function returns a higher order value that uses or " ++
        "observes one or more resources, however the resources arn't " ++
        "declared in the function's return type").
ce_to_pretty(ce_resource_unknown(Res)) =
    p_words("Unknown resource") ++
        p_spc_nl ++ [p_quote("'", q_name_pretty(Res))].
ce_to_pretty(ce_resource_not_public_in_resource(Res, From)) =
    p_words("The resource") ++
    p_spc_nl ++ [nq_name_pretty(Res)] ++ p_spc_nl ++
    p_words("is exported, but it depends on") ++
    p_spc_nl ++ [nq_name_pretty(From)] ++ p_spc_nl ++
    p_words("which is not").
ce_to_pretty(ce_resource_not_public(Res)) =
    p_words("This function or type is exported, " ++
            "but it depends on the resource") ++ p_spc_nl ++
    [q_name_pretty(Res)] ++ p_spc_nl ++
    p_words("which is not").
ce_to_pretty(ce_too_many_bangs_in_statement) =
    p_words("Statement has more than one ! call").
ce_to_pretty(ce_no_bang) =
    p_words("Call uses or observes a resource but has no !").
ce_to_pretty(ce_unnecessary_bang) =
    p_words("Call has a ! but does not need it").

%-----------------------------------------------------------------------%
