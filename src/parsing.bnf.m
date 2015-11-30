%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module parsing.bnf.
%
% BNF data structre.
%
% Copyright (C) 2015 Paul Bone
% Distributed under the terms of the GPLv2 see ../LICENSE.tools
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
                bnf_func            :: func(list(R)) = R
            ).

:- type bnf_atom(T, NT)
    --->    t(T)
    ;       nt(NT).

%-----------------------------------------------------------------------%

% These functions are useful when constructing BNF rules.

    % Return the first parameter.
    %
:- func const(A, B) = A.

    % Return the only item in the list, throwing an exception if the list is
    % empty or there is more than one item.
    %
:- func identity(list(T)) = T.

    % Return the nth item from the list.
    %
:- func identity_nth(int, list(T)) = T.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

%-----------------------------------------------------------------------%

const(A, _) = A.

identity(Nodes) =
    ( Nodes = [Node] ->
        Node
    ;
        unexpected($file, $pred, string(Nodes))
    ).

identity_nth(N, Nodes) =
    list.det_index1(Nodes, N).

