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

:- func init = table(T, NT, TE).

:- pred table_insert(NT::in, T::in, TE::in,
    table(T, NT, TE)::in, table(T, NT, TE)::out) is det.

:- pred table_search(table(T, NT, TE)::in, NT::in, T::in, TE::out) is semidet.

    % Get the set of terminals that are valid for this non-terminal.  This
    % allows the parser to inform the user which terminals it may have
    % expected when it recieved a non-matching terminal and NT was on the
    % top of the stack.
    %
:- pred table_valid_terminals(table(T, NT, TE)::in, NT::in,
    list(T)::out) is det.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

:- import_module map.

:- type table(T, NT, TE)
    --->    table(
                t_map       :: map({NT, T}, TE),
                t_nt_map    :: map(NT, list(T))
            ).

init = table(map.init, map.init).

table_insert(NT, T, Entry, Table@table(Map0, NTMap0), table(Map, NTMap)) :-
    det_insert({NT, T}, Entry, Map0, Map),
    table_valid_terminals(Table, NT, Ts0),
    Ts = [T | Ts0],
    set(NT, Ts, NTMap0, NTMap).

table_search(Table, NT, T, Entry) :-
    search(Table ^ t_map, {NT, T}, Entry).

table_valid_terminals(Table, NT, Ts) :-
    ( search(Table ^ t_nt_map, NT, TsPrime) ->
        Ts = TsPrime
    ;
        Ts = []
    ).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
