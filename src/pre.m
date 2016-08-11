%-----------------------------------------------------------------------%
% Plasma pre-core representation
% vim: ts=4 sw=4 et
%
% Copyright (C) 2016 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
% This module represents the pre-core representation.
%
%-----------------------------------------------------------------------%
:- module pre.
%-----------------------------------------------------------------------%

:- interface.

:- import_module list.
:- import_module set.

:- import_module context.
:- import_module varmap.

:- include_module pre.ast_to_core.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
:- implementation.

:- include_module pre.env.
:- include_module pre.from_ast.
:- include_module pre.nonlocals.


%-----------------------------------------------------------------------%


