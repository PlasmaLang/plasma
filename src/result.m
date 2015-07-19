%-----------------------------------------------------------------------%
% Utility code
% vim: ts=4 sw=4 et
%
% Copyright (C) 2015 Paul Bone
% All rights reserved
%
%-----------------------------------------------------------------------%
:- module result.

:- interface.

:- import_module io.
:- import_module list.

:- import_module context.

%-----------------------------------------------------------------------%

:- type result(T, E)
    --->    ok(T)
    ;       errors(list(error(E))).

:- type result_partial(T, E)
    --->    ok(T, list(error(E)))
    ;       errors(list(error(E))).

:- type errors(E) == list(error(E)).

:- type error(E)
    --->    error(
                e_context       :: context,
                e_level         :: error_or_warning,
                e_error         :: E
            ).

:- type error_or_warning
    --->    error
    ;       warning.

%-----------------------------------------------------------------------%

:- pred add_error(context::in, E::in, errors(E)::in, errors(E)::out)
    is det.

%-----------------------------------------------------------------------%

:- pred report_errors(list(error(E))::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

:- import_module string.

%-----------------------------------------------------------------------%

add_error(Context, ErrorType, !Errors) :-
    Error = error(Context, error, ErrorType),
    !:Errors = [Error | !.Errors].

%-----------------------------------------------------------------------%

report_errors(Errors, !IO) :-
    map(error_to_string, Errors, ErrorStrings),
    write_string(join_list("\n", ErrorStrings), !IO),
    nl(!IO),
    set_exit_status(1, !IO).

:- pred error_to_string(error(E)::in, string::out) is det.

error_to_string(error(Context, EorW, Error), String) :-
    Context = context(Filename, Line),
    ( EorW = error,
        EorWStr = "Error"
    ; EorW = warning,
        EorWStr = "Warning"
    ),
    ErrorStr = string(Error),
    format("%s:%d: %s: %s", [s(Filename), i(Line), s(EorWStr), s(ErrorStr)],
        String).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
