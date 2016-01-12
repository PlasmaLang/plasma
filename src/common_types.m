%-----------------------------------------------------------------------%
% Plasma code representation
% vim: ts=4 sw=4 et
%
% Copyright (C) 2015-2016 Paul Bone
% Distributed under the terms of the GPLv2 see ../LICENSE.tools
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

%-----------------------------------------------------------------------%

    % A constant in an expression.
    %
:- type const_type
    --->    c_string(string)
    ;       c_number(int).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
