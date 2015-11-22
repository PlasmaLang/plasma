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

:- type table(T, NT, R).

:- type terminal_or_eoi(T)
    --->    terminal(T)
    ;       end_of_input.

:- pred table_insert(NT::in, terminal_or_eoi(T)::in,
    bnf_production(T, NT, R)::in,
    table(T, NT, R)::in, table(T, NT, R)::out) is det.

:- pred table_search(table(T, NT, R)::in, NT::in, terminal_or_eoi(T)::in,
    bnf_production(T, NT, R)::out) is semidet.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

:- import_module map.

:- type table(T, NT, R) == map({NT, terminal_or_eoi(T)},
    bnf_production(T, NT, R)).

table_insert(NT, T, Rule, !Table) :-
    det_insert({NT, T}, Rule, !Table).

table_search(Table, NT, T, Rule) :-
    search(Table, {NT, T}, Rule).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
