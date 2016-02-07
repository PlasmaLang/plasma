%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module parsing.bnf.
%
% BNF data structre.
%
% Copyright (C) 2015 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
%-----------------------------------------------------------------------%
:- interface.

:- import_module list.

:- type bnf(T, NT, R)
    --->    bnf(
                bnf_start           :: NT,
                bnf_eof_terminal    :: T,
                bnf_rules           :: list(bnf_rule(T, NT, R))
            ).

:- type bnf_rule(T, NT, R)
    --->    bnf_rule(
                bnf_name            :: string,
                bnf_lhs             :: NT,
                bnf_rhss            :: list(bnf_rhs(T, NT, R))
            ).

:- type bnf_rhs(T, NT, R)
    --->    bnf_rhs(
                bnf_rhs             :: list(bnf_atom(T, NT)),
                bnf_func            :: func(list(R)) = maybe(R)
            ).

:- type bnf_atom(T, NT)
    --->    t(T)
    ;       nt(NT).

%-----------------------------------------------------------------------%

% These functions are useful when constructing BNF rules.

    % Return the first parameter.
    %
:- func const(A, B) = maybe(A).

    % Return the only item in the list, throwing an exception if the list is
    % empty or there is more than one item.
    %
:- func identity(list(T)) = maybe(T).

    % Return the nth item from the list.
    %
:- func identity_nth(int, list(T)) = maybe(T).

:- func det_func(pred(A, B), A) = maybe(B).
:- mode det_func(pred(in, out) is semidet, in) = out is det.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

%-----------------------------------------------------------------------%

const(A, _) = yes(A).

identity(Nodes) =
    ( if Nodes = [Node] then
        yes(Node)
    else
        no
    ).

identity_nth(Index, Nodes) =
    ( if list.index1(Nodes, Index, Node) then
        yes(Node)
    else
        no
    ).

det_func(Pred, Input) =
    ( if Pred(Input, Output) then
        yes(Output)
    else
        no
    ).

