%-----------------------------------------------------------------------%
% Plasma code representation
% vim: ts=4 sw=4 et
%
% Copyright (C) 2015-2016 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
%-----------------------------------------------------------------------%
:- module core.code.
%-----------------------------------------------------------------------%

:- interface.

:- import_module common_types.

:- type expr
    --->    expr(
                e_type      :: expr_type,
                e_info      :: code_info
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

:- func expr_get_callees(expr) = set(func_id).

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

expr_get_callees(Expr) = Callees :-
    ExprType = Expr ^ e_type,
    ( ExprType = e_sequence(Exprs),
        Callees = union_list(map(expr_get_callees, Exprs))
    ; ExprType = e_call(Callee, Args),
        ArgsCallees = union_list(map(expr_get_callees, Args)),
        Callees = insert(ArgsCallees, Callee)
    ; ExprType = e_var(_),
        Callees = init
    ; ExprType = e_const(_),
        Callees = init
    ; ExprType = e_func(Callee),
        % For the purposes of compiler analysis like typechecking this is a
        % callee.
        Callees = make_singleton_set(Callee)
    ).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
