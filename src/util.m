%-----------------------------------------------------------------------%
% Utility code
% vim: ts=4 sw=4 et
%
% Copyright (C) 2015-2016 Plasma Team
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

    % This excpetion and its routines are temporary, they should be used for
    % code that finds a compilation error, but error handling is not
    % properly setup in that area of the compiler.  This helps by making
    % these errors a little more friendly, and by allowing us to search the
    % source code for these locations when we eventually add error handling.
    %
:- type compile_error_exception
    --->    compile_error_exception(string, string, string).

:- pred compile_error(string::in, string::in, string::in) is erroneous.

    % This is an alternative to the sorry/1 predicate in the Mercury
    % standard library.  This predicate uses a dedicated exception type and
    % is caught explicitly by plasmac's main/2 predicate.
    %
:- type unimplemented_exception
    --->    unimplemented_exception(string, string, string).

:- pred sorry(string::in, string::in, string::in) is erroneous.

:- func sorry(string, string, string) = T.
:- mode sorry(in, in, in) = (out) is erroneous.

    % Like sorry except that these exceptions are used for things we think
    % are unlikely.  Like trying to roll more than 256 items on the PZ
    % stack.  If they happen to real people then we'll try to address them
    % and can probably do something about them.
    %
:- type design_limitation_exception
    --->    design_limitation_exception(string, string, string).

:- pred limitation(string::in, string::in, string::in) is erroneous.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

:- import_module exception.

%-----------------------------------------------------------------------%

exit_error(ErrMsg, !IO) :-
    write_string(stderr_stream, ErrMsg ++ "\n", !IO),
    set_exit_status(1, !IO).

%-----------------------------------------------------------------------%

maybe_default(_, yes(X)) = X.
maybe_default(D, no) = D.

%-----------------------------------------------------------------------%

compile_error(File, Pred, Message) :-
    throw(compile_error_exception(File, Pred, Message)).

sorry(File, Pred, Message) :-
    throw(unimplemented_exception(File, Pred, Message)).
sorry(File, Pred, Message) = _ :-
    sorry(File, Pred, Message).

limitation(File, Pred, Message) :-
    throw(design_limitation_exception(File, Pred, Message)).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
