%-----------------------------------------------------------------------%
% Plasma types representation
% vim: ts=4 sw=4 et
%
% Copyright (C) 2015 Paul Bone
% Distributed under the terms of the GPLv2 see ../LICENSE.tools
%
%-----------------------------------------------------------------------%
:- module core.types.
%-----------------------------------------------------------------------%

:- interface.

:- import_module string.
:- import_module list.

:- type type_
    --->    builtin_type(builtin_type)
    ;       type_variable(string)
    ;       type_(
                t_symbol        :: symbol,
                t_args          :: list(type_)
            ).

:- type builtin_type
    --->    int
            % string may not always be builtin.
    ;       string.

:- pred builtin_type_name(builtin_type, string).
:- mode builtin_type_name(in, out) is det.
:- mode builtin_type_name(out, in) is semidet.

:- implementation.

builtin_type_name(int,      "Int").
builtin_type_name(string,   "String").

