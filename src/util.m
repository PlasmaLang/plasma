%-----------------------------------------------------------------------%
% Utility code
% vim: ts=4 sw=4 et
%
% Copyright (C) 2015 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
%-----------------------------------------------------------------------%
:- module util.

:- interface.

:- import_module io.
:- import_module maybe.
:- import_module string.

%-----------------------------------------------------------------------%

    % Print the error to stderror and set the exit code to 1.
    %
    % Does not terminate the program.
    %
:- pred exit_error(string::in, io::di, io::uo) is det.

    % maybe_default(_, yes(X)) = X.
    % maybe_default(D, no)     = D.
    %
    % TODO: Contribute to standard library.
    %
:- func maybe_default(X, maybe(X)) = X.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

%-----------------------------------------------------------------------%

exit_error(ErrMsg, !IO) :-
    write_string(stderr_stream, ErrMsg ++ "\n", !IO),
    set_exit_status(1, !IO).

%-----------------------------------------------------------------------%

maybe_default(_, yes(X)) = X.
maybe_default(D, no) = D.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
