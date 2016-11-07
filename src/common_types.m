%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module common_types.
%
% Copyright (C) 2015-2016 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
% This module defines types useful to multiple Plasma tools.
%
%-----------------------------------------------------------------------%
:- interface.

:- import_module int.
:- import_module string.

    % Is a declration visible outside of its defining module.
    %
:- type sharing
    --->    s_public
    ;       s_private.

    % Has a declration been imported from another module?
    %
:- type imported
    --->    i_local
    ;       i_imported.

    % The arity of an expression is the number of results it returns.
    %
:- type arity
    --->    arity(a_num :: int).

%-----------------------------------------------------------------------%

    % A constant in an expression.
    %
:- type const_type
    --->    c_string(string)
    ;       c_number(int)
    ;       c_func(func_id)
    ;       c_ctor(ctor_id).

%-----------------------------------------------------------------------%

:- type func_id
    --->    func_id(int).

%-----------------------------------------------------------------------%

:- type type_id
    --->    type_id(int).

%-----------------------------------------------------------------------%

:- type ctor_id
    --->    ctor_id(int).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
