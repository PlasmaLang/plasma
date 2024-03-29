%-----------------------------------------------------------------------%
% Plasma pre-core representation
% vim: ts=4 sw=4 et
%
% Copyright (C) Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
% This module represents the pre-core representation.
%
%-----------------------------------------------------------------------%
:- module pre.
%-----------------------------------------------------------------------%

:- interface.

:- include_module pre.ast_to_core.
:- include_module pre.env.
:- include_module pre.import.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
:- implementation.

:- include_module pre.bang.
:- include_module pre.branches.
:- include_module pre.closures.
:- include_module pre.from_ast.
:- include_module pre.pre_ds.
:- include_module pre.pretty.
:- include_module pre.to_core.
:- include_module pre.util.

%-----------------------------------------------------------------------%


