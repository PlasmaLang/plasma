%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module common_types.
%
% Copyright (C) 2015-2018 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
% This module defines types useful to multiple Plasma tools.
%
%-----------------------------------------------------------------------%
:- interface.

:- import_module int.
:- import_module set.
:- import_module string.

    % Is a declaration visible outside of its defining module.
    %
:- type sharing
    --->    s_public
    ;       s_private.

    % Has a declaration been imported from another module?
    %
:- type imported
    --->    i_local
    ;       i_imported.

    % The arity of an expression is the number of results it returns.
    %
:- type arity
    --->    arity(a_num :: int).

    % The number of a particular field within a structure.  This is 1-based,
    % that is the first field is field_num_(1).
    %
:- type field_num
    --->    field_num(field_num_int :: int).

:- func field_num_first = field_num.

:- func field_num_next(field_num) = field_num.

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

:- type resource_id
    --->    resource_id(int).

:- type maybe_resources
    --->    resources(
                r_uses          :: set(resource_id),
                r_observes      :: set(resource_id)
            )
    ;       unknown_resources.

%-----------------------------------------------------------------------%

:- type ctor_id
    --->    ctor_id(int).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

%-----------------------------------------------------------------------%

field_num_first = field_num(1).

field_num_next(field_num(Num)) = field_num(Num + 1).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
