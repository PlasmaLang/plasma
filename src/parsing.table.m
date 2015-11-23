%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module parsing.table.
%
% Table for LL parser.
%
% Copyright (C) 2015 Paul Bone
% Distributed under the terms of the GPLv2 see ../LICENSE.tools
%
%-----------------------------------------------------------------------%
:- interface.

:- type table(T, NT, TE).

:- type terminal_or_eoi(T)
    --->    terminal(T)
    ;       end_of_input.

:- pred table_insert(NT::in, terminal_or_eoi(T)::in, TE::in,
    table(T, NT, TE)::in, table(T, NT, TE)::out) is det.

:- pred table_search(table(T, NT, TE)::in, NT::in, terminal_or_eoi(T)::in,
    TE::out) is semidet.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

:- import_module map.

:- type table(T, NT, TE) == map({NT, terminal_or_eoi(T)}, TE).

table_insert(NT, T, Entry, !Table) :-
    det_insert({NT, T}, Entry, !Table).

table_search(Table, NT, T, Entry) :-
    search(Table, {NT, T}, Entry).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
