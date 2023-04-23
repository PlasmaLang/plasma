%-----------------------------------------------------------------------%
% Logging code
% vim: ts=4 sw=4 et
%
% Copyright (C) Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
%-----------------------------------------------------------------------%
:- module util.log.

:- interface.

:- import_module io.
:- import_module string.

%-----------------------------------------------------------------------%

    % The desired logging level.
    %
:- type log_config
    --->    silent
    ;       verbose.

:- pred verbose_output(log_config::in, string::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
:- implementation.

%-----------------------------------------------------------------------%

verbose_output(silent, _, !IO).
verbose_output(verbose, Message, !IO) :-
    io.write_string(Message, !IO).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
