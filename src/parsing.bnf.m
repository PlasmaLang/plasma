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
                bnf_productions     :: list(bnf_production(T, NT, R))
            ).

:- type bnf_production(T, NT, R)
    --->    bnf_production(
                bnf_name            :: string,
                bnf_lhs             :: NT,
                bnf_rhs             :: list(bnf_atom(T, NT)),
                bnf_func            :: func(list(R)) = R
            ).

:- type bnf_atom(T, NT)
    --->    t(T)
    ;       nt(NT).

