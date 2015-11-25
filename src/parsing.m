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

:- import_module context.
:- import_module parsing.bnf.
:- import_module result.

%-----------------------------------------------------------------------%

:- type parser(T, NT, R).

:- typeclass token_to_result(T, R) where [
        func token_to_result(T, maybe(string), context) = R
    ].

:- type token(T)
    --->    token(
                t_terminal      :: T,
                t_data          :: maybe(string),
                t_context       :: context
            ).

:- type parse_error(T)
    --->    pe_unexpected_token(
                peut_expected           :: list(T),
                peut_got                :: T
            )
    ;       pe_unexpected_eof(
                peue_expected           :: list(T)
            )
    ;       pe_junk_at_end(
                pejae_got               :: T
            ).

:- pred parse(parser(T, NT, R), list(token(T)), result(R, parse_error(T)))
    <= token_to_result(T, R).
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
                p_eof_terminal  :: T,
                p_table         :: table(T, NT, table_entry(T, NT, R))
            ).

%-----------------------------------------------------------------------%

:- type stack_item(T, NT, R)
    --->    stack_nt(NT)
    ;       stack_t(T)
    ;       stack_reduce(int, func(list(R)) = R).

parse(Parser, Input0, Result) :-
    Stack = [stack_nt(Parser ^ p_start)],
    (
        last(Input0, Last),
        Last = token(Parser ^ p_eof_terminal, _, _)
    ->
        Input = Input0
    ;
        Input = Input0 ++ [token(Parser ^ p_eof_terminal, no, nil_context)]
    ),
    parse(Parser, Input, Stack, [], Result).

:- pred parse(parser(T, NT, R), list(token(T)), list(stack_item(T, NT, R)),
        list(R), result(R, parse_error(T)))
    <= token_to_result(T, R).
:- mode parse(in, in, in, in, out) is det.

parse(Parser, Input0, Stack0, ResultStack0, Result) :-
    ( Stack0 = [Tos | Stack1],
        ( Tos = stack_t(TS),
            ( Input0 = [token(TI, MaybeString, Context) | Input],
                ( TI = TS ->
                    % Input and TOS match, discard both and proceed.
                    TokenResult = token_to_result(TI, MaybeString, Context),
                    ResultStack = [TokenResult | ResultStack0],
                    parse(Parser, Input, Stack1, ResultStack, Result)
                ;
                    % Not matched, parsing error.
                    Error = pe_unexpected_token([TS], TI),
                    Result = return_error(Context, Error)
                )
            ; Input0 = [],
                Error = pe_unexpected_eof([TS]),
                Result = return_error(nil_context, Error)
            )
        ; Tos = stack_nt(NTS),
            % Check table
            ( Input0 = [token(TI, _, _) | _],
                Terminal = TI
            ; Input0 = [],
                Terminal = Parser ^ p_eof_terminal
            ),
            ( table_search(Parser ^ p_table, NTS, Terminal, Entry) ->
                Stack = Entry ^ te_new_stack_items ++ Stack1,
                parse(Parser, Input0, Stack, ResultStack0, Result)
            ;
                table_valid_terminals(Parser ^ p_table, NTS, ValidTerminals),
                ( Input0 = [token(TIPrime, _, Context) | _],
                    Error = pe_unexpected_token(ValidTerminals, TIPrime)
                ; Input0 = [],
                    Error = pe_unexpected_eof(ValidTerminals),
                    Context = nil_context
                ),
                Result = return_error(Context, Error)
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
                unexpected($file, $pred, "Couldn't build result")
            )
        ; Input0 = [token(TI, _, Context) | _],
            Error = pe_junk_at_end(TI),
            Result = return_error(Context, Error)
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
