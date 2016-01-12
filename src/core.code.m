%-----------------------------------------------------------------------%
% Plasma code representation
% vim: ts=4 sw=4 et
%
% Copyright (C) 2015-2016 Paul Bone
% Distributed under the terms of the GPLv2 see ../LICENSE.tools
%
%-----------------------------------------------------------------------%
:- module core.code.
%-----------------------------------------------------------------------%

:- interface.

:- import_module common_types.

:- type expr
    --->    expr(
                s_type      :: expr_type,
                s_info      :: code_info
            ).

:- type expr_type
    --->    e_sequence(list(expr))
    ;       e_call(func_id, list(expr))
    ;       e_var(var)
    ;       e_const(const_type)
    ;       e_func(func_id).

%-----------------------------------------------------------------------%

:- type code_info.

:- func code_info_init = code_info.

:- type using_marker
    --->    has_using_marker
    ;       no_using_marker.

:- func code_info_using_marker(code_info) = using_marker.

:- pred code_info_set_using_marker(using_marker::in,
    code_info::in, code_info::out) is det.

%-----------------------------------------------------------------------%

:- implementation.

:- type code_info
    --->    code_info(
                ci_using_marker     :: using_marker
            ).

code_info_init = code_info(no_using_marker).

code_info_using_marker(Info) = Info ^ ci_using_marker.

code_info_set_using_marker(UsingMarker, !Info) :-
    !Info ^ ci_using_marker := UsingMarker.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
