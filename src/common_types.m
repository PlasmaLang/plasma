%-----------------------------------------------------------------------%
% Plasma code representation
% vim: ts=4 sw=4 et
%
% Copyright (C) 2015-2016 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
%-----------------------------------------------------------------------%
:- module common_types.
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
    ;       c_number(int).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
