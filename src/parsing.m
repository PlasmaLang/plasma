%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module parsing.
%
% Parsing utils.
%
% Copyright (C) 2015 Paul Bone
% Distributed under the terms of the GPLv2 see ../LICENSE.tools
%
%-----------------------------------------------------------------------%

:- interface.

:- import_module list.
:- import_module maybe.

:- include_module parsing.bnf.
:- include_module parsing.gen.

:- import_module parsing.bnf.

%-----------------------------------------------------------------------%

:- type parser(T, NT, R).

:- typeclass from_string(R) where [
        func from_string(string) = R
    ].

:- type token(T)
    --->    token(
                t_terminal      :: T,
                t_data          :: string
            ).

:- pred parse(parser(T, NT, R), list(token(T)), maybe_error(R))
    <= from_string(R).
:- mode parse(in, in, out) is det.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

:- import_module array.
:- import_module bool.
:- import_module io.
:- import_module int.
:- import_module map.
:- import_module require.
:- import_module set.
:- import_module string.

:- include_module parsing.table.

:- import_module parsing.table.

%-----------------------------------------------------------------------%

:- type table_entry(T, NT, R)
    --->    table_entry(
                te_new_stack_items  :: list(stack_item(T, NT, R))
            ).

:- type parser(T, NT, R)
    --->    parser(
                p_start         :: NT,
                p_table         :: table(T, NT, table_entry(T, NT, R))
            ).

%-----------------------------------------------------------------------%

:- type stack_item(T, NT, R)
    --->    stack_nt(NT)
    ;       stack_t(T)
    ;       stack_reduce(int, func(list(R)) = R).

parse(Parser, Input, Result) :-
    Stack = [stack_nt(Parser ^ p_start)],
    parse(Parser, Input, Stack, [], Result).

:- pred parse(parser(T, NT, R), list(token(T)), list(stack_item(T, NT, R)),
        list(R), maybe_error(R))
    <= from_string(R).
:- mode parse(in, in, in, in, out) is det.

parse(Parser, Input0, Stack0, ResultStack0, Result) :-
    ( Stack0 = [Tos | Stack1],
        ( Tos = stack_t(TS),
            ( Input0 = [token(TI, String) | Input],
                ( TI = TS ->
                    % Input and TOS match, discard both and proceed.
                    ResultStack = [from_string(String) | ResultStack0],
                    parse(Parser, Input, Stack1, ResultStack, Result)
                ;
                    % Not matched, parsing error.
                    Result = error(format("Parse error: expected %s got %s",
                        [s(string(TS)), s(string(TI))]))
                )
            ; Input0 = [],
                Result = error("Mismatch at end of parsing")
            )
        ; Tos = stack_nt(NTS),
            % Check table
            ( Input0 = [token(TI, _) | _],
                Terminal = terminal(TI)
            ; Input0 = [],
                Terminal = end_of_input
            ),
            ( table_search(Parser ^ p_table, NTS, Terminal, Entry) ->
                Stack = Entry ^ te_new_stack_items ++ Stack1,
                parse(Parser, Input0, Stack, ResultStack0, Result)
            ;
                Result = error("No transition")
            )
        ; Tos = stack_reduce(Num, Func),
            det_split_list(Num, ResultStack0, Nodes0, ResultStack1),
            reverse(Nodes0, Nodes),
            Node = Func(Nodes),
            ResultStack = [Node | ResultStack1],
            parse(Parser, Input0, Stack1, ResultStack, Result)
        )
    ; Stack0 = [],
        ( Input0 = [],
            ( ResultStack0 = [R] ->
                Result = ok(R)
            ;
                Result = error("Couldn't build result")
            )
        ; Input0 = [_ | _],
            Result = error("Tokens at end of input")
        )
    ).

:- pred det_pop_items(int::in, list(T)::in, list(T)::out, list(T)::out)
    is det.

det_pop_items(N, List, Prefix, Suffix) :-
    det_pop_items(N, List, [], Prefix, Suffix).

:- pred det_pop_items(int::in, list(T)::in,
    list(T)::in, list(T)::out, list(T)::out) is det.

det_pop_items(N, List0, !Prefix, Suffix) :-
    ( N < 0 ->
        unexpected($file, $pred, "N < 0")
    ; N = 0 ->
        Suffix = List0
    ;
        ( List0 = [],
            unexpected($file, $pred, "list too short")
        ; List0 = [X | List],
            det_pop_items(N - 1, List, [X | !.Prefix], !:Prefix, Suffix)
        )
    ).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
