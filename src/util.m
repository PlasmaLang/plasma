%-----------------------------------------------------------------------%
% Utility code
% vim: ts=4 sw=4 et
%
% Copyright (C) 2015-2020 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
%-----------------------------------------------------------------------%
:- module util.

:- interface.

:- include_module util.exception.
:- include_module util.mercury.
:- include_module util.path.

:- import_module bag.
:- import_module cord.
:- import_module io.
:- import_module list.
:- import_module maybe.
:- import_module set.

:- import_module context.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

:- import_module exception.
:- import_module int.
:- import_module pair.
:- import_module require.
:- import_module string.
:- import_module uint32.
:- import_module uint64.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
