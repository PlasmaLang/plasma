%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module util.result.
%
% Copyright (C) 2015, 2018, 2020 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
% A result type, like maybe_error however it can track multiple compilation
% errors.
%
%-----------------------------------------------------------------------%

:- interface.

:- import_module io.
:- import_module cord.
:- import_module list.

:- import_module context.

%-----------------------------------------------------------------------%

:- type result(T, E)
    --->    ok(T)
    ;       errors(errors(E)).

:- type result_partial(T, E)
    --->    ok(T, errors(E))
    ;       errors(errors(E)).

:- type errors(E) == cord(error(E)).

:- type error(E)
    --->    error(
                e_context       :: context,
                e_error         :: E
            ).

%-----------------------------------------------------------------------%

:- typeclass error(E) where [
        func to_string(E) = string,
        func error_or_warning(E) = error_or_warning
    ].

:- type error_or_warning
    --->    error
    ;       warning.

%-----------------------------------------------------------------------%

:- pred add_error(context::in, E::in, errors(E)::in, errors(E)::out)
    is det.

    % add_errors(NewErrors, !Errors)
    %
    % Add NewErrors to !Errors.
    %
:- pred add_errors(errors(E)::in, errors(E)::in, errors(E)::out) is det.

    % Add errors if the result contains any.
    %
:- pred add_errors_from_result(result(T, E)::in,
    errors(E)::in, errors(E)::out) is det.

%-----------------------------------------------------------------------%

:- func error(context, E) = errors(E).

:- func return_error(context, E) = result(T, E).

%-----------------------------------------------------------------------%

:- pred has_fatal_errors(errors(E)::in) is semidet <= error(E).

:- pred report_errors(errors(E)::in, io::di, io::uo) is det
    <= error(E).

%-----------------------------------------------------------------------%

:- pred result_list_to_result(list(result(T, E))::in,
    result(list(T), E)::out) is det.
:- func result_list_to_result(list(result(T, E))) = result(list(T), E).

:- func result_map((func(T) = U), result(T, E)) = result(U, E).

%-----------------------------------------------------------------------%

:- func errors_map((func(E1) = E2), errors(E1)) = errors(E2).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

:- import_module string.

%-----------------------------------------------------------------------%

add_error(Context, ErrorType, !Errors) :-
    Error = error(Context, ErrorType),
    !:Errors = snoc(!.Errors, Error).

%-----------------------------------------------------------------------%

add_errors(NewErrors, !Errors) :-
    !:Errors = !.Errors ++ NewErrors.

add_errors_from_result(ok(_), !Errors).
add_errors_from_result(errors(NewErrors), !Errors) :-
    add_errors(NewErrors, !Errors).

%-----------------------------------------------------------------------%

error(Context, Error) =
    singleton(error(Context, Error)).

return_error(Context, Error) =
    errors(singleton(error(Context, Error))).

%-----------------------------------------------------------------------%

has_fatal_errors(Errors) :-
    member(Error, Errors),
    error_or_warning(Error ^ e_error) = error.

report_errors(Errors, !IO) :-
    ErrorStrings = map(error_to_string, Errors),
    write_string(join_list("\n", list(ErrorStrings)), !IO),
    nl(!IO).

:- func error_to_string(error(E)) = string <= error(E).

error_to_string(error(Context, Error)) =
    context_string(Context) ++ ": " ++ to_string(Error).

%-----------------------------------------------------------------------%

result_list_to_result(Results, Result) :-
    list.foldl(build_result, Results, ok([]), Result0),
    ( Result0 = ok(RevList),
        Result = ok(reverse(RevList))
    ; Result0 = errors(_),
        Result = Result0
    ).
result_list_to_result(Results) = Result :-
    result_list_to_result(Results, Result).

:- pred build_result(result(T, E)::in,
    result(list(T), E)::in, result(list(T), E)::out) is det.

build_result(ok(X), ok(Xs), ok([X | Xs])).
build_result(ok(_), R@errors(_), R).
build_result(errors(E), ok(_), errors(E)).
build_result(errors(E), errors(Es0), errors(Es)) :-
    add_errors(E, Es0, Es).

%-----------------------------------------------------------------------%

result_map(Func, ok(X)) = ok(Func(X)).
result_map(_, errors(E)) = errors(E).

%-----------------------------------------------------------------------%

errors_map(Func, Errors) = map(error_map(Func), Errors).

:- func error_map((func(E1) = E2), error(E1)) = error(E2).

error_map(Func, error(Context, E)) = error(Context, Func(E)).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
