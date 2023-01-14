%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module compile_error.
%
% Copyright (C) 2015-2018, 2020-2023 Plasma Team
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

:- type filename ---> filename(string).

:- type compile_error
    % Errors for reading source code or the organisation of code (module and
    % file names don't match, etc).
            % This creates a dependency on the parser, I'm uneasy about
            % this.
    --->    ce_read_source_error(read_src_error)
    ;       ce_module_name_not_match_build(q_name, string)
    ;       ce_source_file_name_not_match_module(q_name, filename)
    ;       ce_object_file_name_not_match_module(q_name, filename)
    ;       ce_module_not_found(q_name)
    ;       ce_module_unavailable(q_name, q_name)
    ;       ce_interface_contains_wrong_module(filename, q_name, q_name)
    ;       ce_import_would_clobber(q_name, maybe(q_name))
    ;       ce_import_duplicate(q_name)

    % Generic errors with the binding of symbols.
    ;       ce_function_already_defined(string)
    ;       ce_entry_function_wrong_signature

    % Type related errors
    ;       ce_type_already_defined(q_name)
    ;       ce_type_duplicate_constructor(q_name)
    ;       ce_type_not_known(q_name)
    ;       ce_type_not_public_in_type(nq_name, nq_name)
    ;       ce_type_not_public_in_func(nq_name, nq_name)
    ;       ce_type_var_unknown(string)
    ;       ce_type_has_incorrect_num_of_args(q_name, int, int)
    ;       ce_builtin_type_with_args(q_name)
    ;       ce_type_var_with_args(string)
    ;       ce_type_error(type_error)
    ;       ce_type_floundering(list(pretty), list(pretty))

    % Pattern matching
    ;       ce_match_has_no_cases
    ;       ce_match_does_not_cover_all_cases
    ;       ce_match_unreached_cases
    ;       ce_match_duplicate_case
    ;       ce_match_on_function_type
    ;       ce_case_does_not_define_all_variables(list(string))

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
    ;       ce_resource_not_public_in_type(nq_name, nq_name)
    ;       ce_resource_not_public_in_function(nq_name, nq_name)
    ;       ce_too_many_bangs_in_statement
    ;       ce_no_bang
    ;       ce_unnecessary_bang.

:- type type_error
    --->    type_unification_failed(pretty, pretty, maybe(type_error))
    ;       type_unification_occurs(pretty, pretty).

:- instance error(compile_error).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
:- implementation.

:- import_module cord.
:- import_module string.

%-----------------------------------------------------------------------%

:- instance error(compile_error) where [
    func(error_or_warning/1) is ce_error_or_warning,
    pred(pretty/4) is ce_to_pretty
].

:- func ce_error_or_warning(compile_error) = error_or_warning.

ce_error_or_warning(Error) =
    ( if
        Error = ce_unnecessary_bang
      ; Error = ce_import_duplicate(_)
    then
        warning
    else
        error
    ).

:- pred ce_to_pretty(string::in, compile_error::in, list(pretty)::out,
    list(pretty)::out) is det.

ce_to_pretty(SrcPath, ce_read_source_error(E), Para, Extra) :-
    pretty(SrcPath, E, Para, Extra).
ce_to_pretty(_, ce_module_name_not_match_build(Module, ModuleInBuild),
        Para, []) :-
    Para = p_words("The module name from the source file") ++ p_spc_nl ++
        [p_quote("'", q_name_pretty(Module))] ++ p_spc_nl ++
        p_words("does not match the module name from the BUILD.plz file") ++
        p_spc_nl ++
        [p_quote("'", p_str(ModuleInBuild))].
ce_to_pretty(_, ce_source_file_name_not_match_module(Expect, Got), Para, []) :-
    Para = p_words("The source filename") ++ p_spc_nl ++
        p_file(Got) ++ p_spc_nl ++
        p_words("does not match the module name") ++ p_spc_nl ++
        [p_quote("'", q_name_pretty(Expect))].
ce_to_pretty(_, ce_object_file_name_not_match_module(Expect, Got), Para, []) :-
    Para = p_words("The output filename") ++ p_spc_nl ++
        p_file(Got) ++ p_spc_nl ++
        p_words("does not match the module name") ++ p_spc_nl ++
        [p_quote("'", q_name_pretty(Expect))].
ce_to_pretty(_, ce_module_not_found(Name), Para, []) :-
    Para = p_words("The interface file for the imported module") ++ p_spc_nl ++
        [p_str("("), q_name_pretty(Name), p_str(")")] ++ p_spc_nl ++
        p_words("cannot be found. Was the module listed in BUILD.plz?").
ce_to_pretty(_, ce_module_unavailable(Importee, Importer), Para, []) :-
    Para = p_words("The module") ++ p_spc_nl ++
        [q_name_pretty(Importee)] ++ p_spc_nl ++
        p_words("can't be included because it is not listed in all the " ++
            "build file's module lists that include module") ++ p_spc_nl ++
        [q_name_pretty(Importer)].
ce_to_pretty(_, ce_interface_contains_wrong_module(File, Expect, Got), Para, []) :-
    Para = p_words("The interface file") ++ p_spc_nl ++
        p_file(File) ++ p_spc_nl ++
        p_words("describes the wrong module, got:") ++ p_spc_nl ++
        [p_quote("'", q_name_pretty(Got))] ++ p_spc_nl ++
        [p_str("expected:")] ++ p_spc_nl ++
        [p_quote("'", q_name_pretty(Expect))].
ce_to_pretty(_, ce_import_would_clobber(ModuleName, MaybeAsName), Para, []) :-
    ParaA = p_words("The import of") ++ p_spc_nl ++
        [p_quote("'", q_name_pretty(ModuleName))] ++ p_spc_nl,
    ( MaybeAsName = no,
        ParaB = p_words("clobbers a previous import to that name")
    ; MaybeAsName = yes(AsName),
        ParaB = [p_str("clobbers")] ++ p_spc_nl ++
            [p_quote("'", q_name_pretty(AsName))] ++ p_spc_nl ++
            p_words("which is used by a previous import")
    ),
    Para = ParaA ++ ParaB.
ce_to_pretty(_, ce_import_duplicate(ModuleName), Para, []) :-
    Para = p_words("The import of") ++ p_spc_nl ++
        [p_quote("'", q_name_pretty(ModuleName))] ++ p_spc_nl ++
    p_words("is redundant, this module is already imported").

ce_to_pretty(_, ce_function_already_defined(Name), Para, []) :-
    Para = p_words("Function already defined:") ++ p_spc_nl ++
        [p_str(Name)].
ce_to_pretty(_, ce_entry_function_wrong_signature, Para, []) :-
    Para = p_words("A function that is marked as an entrypoint does not " ++
        "have the correct signature for an entrypoint.").

ce_to_pretty(_, ce_type_already_defined(Name), Para, []) :-
    Para = p_words("Type already defined: ") ++ p_spc_nl ++
        [q_name_pretty(Name)].
ce_to_pretty(_, ce_type_duplicate_constructor(Name), Para, []) :-
    Para = p_words("This type already has a constructor named") ++ p_spc_nl ++
        [p_quote("'", q_name_pretty(Name))].
ce_to_pretty(_, ce_type_not_known(Name), Para, []) :-
    Para = p_words("Unknown type:") ++ p_spc_nl ++
        [q_name_pretty(Name)].
ce_to_pretty(_, ce_type_not_public_in_type(Referer, Referee), Para, []) :-
    Para = p_words("The type") ++ p_spc_nl ++
        [nq_name_pretty(Referer)] ++ p_spc_nl ++
        p_words("is exported, but it refers to another type") ++ p_spc_nl ++
        [nq_name_pretty(Referee)] ++ p_spc_nl ++
        p_words("which is not.").
ce_to_pretty(_, ce_type_not_public_in_func(Func, Type), Para, []) :-
    Para = p_words("The function") ++ p_spc_nl ++
        [nq_name_pretty(Func)] ++ p_spc_nl ++
        p_words("is exported, but it refers to the type") ++ p_spc_nl ++
        [nq_name_pretty(Type)] ++ p_spc_nl ++
        p_words("which is not.").
ce_to_pretty(_, ce_type_var_unknown(Name), Para, []) :-
    Para = p_words("Type variable") ++ p_spc_nl ++
        [p_quote("'", p_str(Name))] ++ p_spc_nl ++
        p_words("does not appear on left of '=' in type definition").
ce_to_pretty(_, ce_type_has_incorrect_num_of_args(Name, Want, Got), Para, []) :-
    Para = p_words("Wrong number of type args for ") ++ p_spc_nl ++
        [p_quote("'", q_name_pretty(Name)), p_str(",")] ++ p_spc_nl ++
        [p_str("expected: "), p_str(string(Want)), p_str(",")] ++ p_spc_nl ++
        [p_str("got: "), p_str(string(Got))].
ce_to_pretty(_, ce_builtin_type_with_args(Name), Para, []) :-
    Para = p_words("Builtin type") ++ p_spc_nl ++
        [p_quote("'", q_name_pretty(Name))] ++ p_spc_nl ++
        p_words("does not take arguments").
ce_to_pretty(_, ce_type_var_with_args(Name), Para, []) :-
    Para = p_words("Type variables (like") ++ p_spc_nl ++
        [p_quote("'", p_str(Name))] ++ p_spc_nl ++
        p_words("cannot take arguments").
ce_to_pretty(_, ce_type_error(TypeError), Para, []) :-
    Para = type_error_pretty(TypeError).
ce_to_pretty(_, ce_type_floundering(Vars, Clauses), Para, Extra) :-
    Para = p_words("Ambigious types"),
    Extra = [
        p_expr([p_str("The unbound solver variables are: "), p_nl_hard,
            p_list(pretty_seperated([p_nl_hard], Vars))]), p_nl_double,
        p_expr([p_str("The unresolved solver clauses are: "), p_nl_hard,
            p_list(pretty_seperated([p_nl_double], Clauses))])].

ce_to_pretty(_, ce_match_has_no_cases,
    p_words("Match expression has no cases"), []).
ce_to_pretty(_, ce_match_does_not_cover_all_cases,
    p_words("Match does not cover all cases"), []).
ce_to_pretty(_, ce_match_unreached_cases,
    p_words("This case will never be tested because earlier cases cover " ++
        "all values"),
    []).
ce_to_pretty(_, ce_match_duplicate_case,
    p_words("This case occurs multiple times in this match"), []).
ce_to_pretty(_, ce_match_on_function_type,
    p_words("Attempt to pattern match on a function"), []).
ce_to_pretty(_, ce_case_does_not_define_all_variables(Vars),
    p_words("This branch did not initialise variables initialised on other
        branches, they are:") ++
    [p_nl_soft, p_str(" "),
        p_list(pretty_seperated([p_str(", "), p_nl_soft],
            map(p_str, Vars)))], []).

ce_to_pretty(_, Error, Pretty, []) :-
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
ce_to_pretty(_, ce_arity_mismatch_tuple,
    p_words("Arity mismatch in tuple, could be called by arguments to call"),
    []).
ce_to_pretty(_, ce_arity_mismatch_match(Arities), Para, []) :-
    Para = p_words("Match expression has cases with different arrites, " ++
            " they are:") ++
        p_spc_nl ++ [p_expr(pretty_comma_seperated(
            map((func(MA) = S :-
                    ( MA = yes(A), S = p_str(string(A ^ a_num))
                    ; MA = no,     S = p_str("_")
                    )
                ), Arities)
            ))].

ce_to_pretty(_, ce_parameter_number(Exp, Got), Para, []) :-
    Para = p_words(format("Wrong number of parameters in function call, "
            ++ "expected %d got %d",
        [i(Exp), i(Got)])).
ce_to_pretty(_, ce_no_return_statement(Arity), Para, []) :-
    Para = p_words(format(
        "Function returns %d results but this path has no return statement",
        [i(Arity ^ a_num)])).

ce_to_pretty(_, ce_uses_observes_not_distinct(Resources), Para, []) :-
    Para = p_words("A resource cannot appear in both the uses and observes " ++
            "lists, found resources:") ++
        p_spc_nl ++
        pretty_comma_seperated(map(func(R) = p_str(resource_to_string(R)),
            Resources)).
ce_to_pretty(_, ce_resource_unavailable_call,
    p_words("One or more resources needed for this call is unavailable " ++
        "in this function"),
    []).
ce_to_pretty(_, ce_resource_unavailable_arg,
    p_words("One or more resources needed for an argument to a call " ++
        "is not provided in by the passed-in value"),
    []).
ce_to_pretty(_, ce_resource_unavailable_output,
    p_words("The function returns a higher order value that uses or " ++
        "observes one or more resources, however the resources arn't " ++
        "declared in the function's return type"),
    []).
ce_to_pretty(_, ce_resource_unknown(Res),
    p_words("Unknown resource") ++
        p_spc_nl ++ [p_quote("'", q_name_pretty(Res))],
    []).
ce_to_pretty(_, ce_resource_not_public_in_resource(Res, From), Para, []) :-
    Para = p_words("The resource") ++
        p_spc_nl ++ [nq_name_pretty(Res)] ++ p_spc_nl ++
        p_words("is exported, but it depends on") ++
        p_spc_nl ++ [nq_name_pretty(From)] ++ p_spc_nl ++
        p_words("which is not").
ce_to_pretty(_, ce_resource_not_public_in_type(Type, Res), Para, []) :-
    Para = p_words("The type") ++
        p_spc_nl ++ [nq_name_pretty(Type)] ++ p_spc_nl ++
        p_words("is exported, but it refers to the resource") ++ p_spc_nl ++
        [nq_name_pretty(Res)] ++ p_spc_nl ++
        p_words("which is not exported").
ce_to_pretty(_, ce_resource_not_public_in_function(Func, Res), Para, []) :-
    Para = p_words("The function") ++
        p_spc_nl ++ [nq_name_pretty(Func)] ++ p_spc_nl ++
        p_words("is exported, but it refers to the resource") ++ p_spc_nl ++
        [nq_name_pretty(Res)] ++ p_spc_nl ++
        p_words("which is not exported").
ce_to_pretty(_, ce_too_many_bangs_in_statement,
    p_words("Statement has more than one ! call"), []).
ce_to_pretty(_, ce_no_bang,
    p_words("Call uses or observes a resource but has no !"), []).
ce_to_pretty(_, ce_unnecessary_bang,
    p_words("Call has a ! but does not need it"), []).

:- func type_error_pretty(type_error) = list(pretty).

type_error_pretty(type_unification_failed(Type1, Type2, MaybeWhy)) = Error :-
    Error = [p_quote("\"", Type1)] ++ p_spc_nl ++ [p_str("and")] ++
        p_spc_nl ++ [p_quote("\"", Type2)] ++ p_spc_nl ++
        p_words("are not the same") ++ WhyError,
    ( MaybeWhy = yes(Why),
        WhyError = p_words(", because") ++ [p_nl_hard] ++
            type_error_pretty(Why)
    ; MaybeWhy = no,
        WhyError = []
    ).
type_error_pretty(type_unification_occurs(Var, Type)) =
    [p_str("Type error: "),
        p_str("The type "), p_quote("\"", Var)] ++ p_spc_nl ++
        p_words("cannot be bound to") ++ p_spc_nl ++
        [p_quote("\"", Type)] ++ p_spc_nl ++
        p_words("because it can't contain itself.").

:- func p_file(filename) = list(pretty).

p_file(filename(File)) = [p_quote("'", p_str(File))].

%-----------------------------------------------------------------------%
