%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module util.result.
%
% Copyright (C) Plasma Team
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
:- import_module map.
:- import_module maybe.

:- import_module context.
:- import_module util.pretty.

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
        % pretty(Error, ParaPart, ExtraPart)
        pred pretty(string::in, E::in, list(pretty)::out, list(pretty)::out) is det,
        func error_or_warning(E) = error_or_warning
    ].

:- instance error(string).

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

:- func return_error_p(context, E) = result_partial(T, E).

:- func maybe_to_result(context, func(string) = string, maybe_error(T)) =
    result(T, string).

%-----------------------------------------------------------------------%

:- pred has_fatal_errors(errors(E)::in) is semidet <= error(E).

    % report_errors(SourcePath, Errors, !IO).
    %
:- pred report_errors(string::in, errors(E)::in, io::di, io::uo) is det
    <= error(E).

%-----------------------------------------------------------------------%

:- pred result_list_to_result(list(result(T, E))::in,
    result(list(T), E)::out) is det.
:- func result_list_to_result(list(result(T, E))) = result(list(T), E).

:- func result_map((func(T) = U), result(T, E)) = result(U, E).

%-----------------------------------------------------------------------%

    % foldl over a list except the accumulator includes a result that must
    % be unpact before processing the next item.  If mercury had monads this
    % would be bind.
    %
:- pred foldl_result(pred(X, A, result(A, E)), list(X),
    A, result(A, E)).
:- mode foldl_result(pred(in, in, out) is det, in, in, out) is det.

    % Set or update the value within a map at the given key.  if the update
    % function fails then return that error.
    %
:- pred map_set_or_update_result(func(V) = result(V, E),
    K, V, map(K, V), result(map(K, V), E)).
:- mode map_set_or_update_result(in, in, in, in, out) is det.

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

return_error_p(Context, Error) =
    errors(singleton(error(Context, Error))).

maybe_to_result(_, _, ok(X)) = ok(X).
maybe_to_result(Context, Wrap, error(Msg)) =
    return_error(Context, Wrap(Msg)).

%-----------------------------------------------------------------------%

has_fatal_errors(Errors) :-
    member(Error, Errors),
    error_or_warning(Error ^ e_error) = error.

report_errors(SourcePath, Errors, !IO) :-
    ErrorStrings = map(func(E) = error_to_string(SourcePath, E) ++ "\n",
        list(Errors)),
    write_string(append_list(ErrorStrings), !IO).

:- func error_to_string(string, error(E)) = string <= error(E).

error_to_string(SourcePath, error(Context, Error)) = String :-
    Type = error_or_warning(Error),
    ( if not is_nil_context(Context) then
        ( Type = error,
            Prefix = [p_str(context_string(SourcePath, Context)), p_str(":"),
                p_spc, p_tabstop]
        ; Type = warning,
            Prefix = [p_str(context_string(SourcePath, Context)), p_str(":"),
                p_spc, p_tabstop, p_str("Warning: ")]
        )
    else
        ( Type = error,
            EoW = "Error: "
        ; Type = warning,
            EoW = "Warning: "
        ),
        Prefix = [p_str(EoW), p_tabstop]
    ),
    pretty(SourcePath, Error, Para, Extra),
    ( Extra = [],
        Pretty = [p_para(Prefix ++ Para)]
    ; Extra = [_ | _],
        Pretty = [p_para(Prefix ++ Para), p_nl_hard] ++ Extra
    ),
    String = append_list(list(pretty(options(80, 2), 0, Pretty))).

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

foldl_result(_, [], Acc, ok(Acc)).
foldl_result(Pred, [X | Xs], Acc0, MaybeAcc) :-
    Pred(X, Acc0, MaybeAcc1),
    ( MaybeAcc1 = ok(Acc1),
        foldl_result(Pred, Xs, Acc1, MaybeAcc)
    ; MaybeAcc1 = errors(Error),
        MaybeAcc = errors(Error)
    ).

%-----------------------------------------------------------------------%

map_set_or_update_result(UpdateFn, Key, Value, !.Map, MaybeMap) :-
    ( if search(!.Map, Key, Old) then
        MaybeNew = UpdateFn(Old),
        ( MaybeNew = ok(New),
            det_update(Key, New, !Map),
            MaybeMap = ok(!.Map)
        ; MaybeNew = errors(Error),
            MaybeMap = errors(Error)
        )
    else
        set(Key, Value, !Map),
        MaybeMap = ok(!.Map)
    ).

%-----------------------------------------------------------------------%

errors_map(Func, Errors) = map(error_map(Func), Errors).

:- func error_map((func(E1) = E2), error(E1)) = error(E2).

error_map(Func, error(Context, E)) = error(Context, Func(E)).

%-----------------------------------------------------------------------%

:- instance error(string) where [
        pretty(_, S, p_words(S), []),
        error_or_warning(_) = error
    ].

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
