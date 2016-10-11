%-----------------------------------------------------------------------%
% Plasma types representation
% vim: ts=4 sw=4 et
%
% Copyright (C) 2015-2016 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
%-----------------------------------------------------------------------%
:- module core.types.
%-----------------------------------------------------------------------%

:- interface.

:- import_module string.
:- import_module list.

%-----------------------------------------------------------------------%

:- type type_
    --->    builtin_type(builtin_type)
    ;       type_variable(type_var)
    ;       type_(
                t_symbol        :: q_name,
                t_args          :: list(type_)
            ).

:- type type_var == string.

:- type builtin_type
    --->    int
            % string may not always be builtin.
    ;       string.

:- pred builtin_type_name(builtin_type, string).
:- mode builtin_type_name(in, out) is det.
:- mode builtin_type_name(out, in) is semidet.

%-----------------------------------------------------------------------%

:- type constructor
    --->    constructor(
                c_name          :: q_name
            ).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
:- implementation.

%-----------------------------------------------------------------------%

builtin_type_name(int,      "Int").
builtin_type_name(string,   "String").

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
