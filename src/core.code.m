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

:- import_module context.
:- import_module common_types.

:- type expr
    --->    expr(
                e_type      :: expr_type,
                e_info      :: code_info
            ).

:- type expr_type
    --->    e_tuple(list(expr))
            % TODO: Allow anonymous binds.
    ;       e_let(list(var), expr, expr)
    ;       e_call(func_id, list(var))
    ;       e_var(var)
    ;       e_const(const_type)
    ;       e_match(var, list(expr_case)).

:- type expr_case
    --->    e_case(expr_pattern, expr).

:- type expr_pattern
    --->    e_num(int)
    ;       e_variable(var)
    ;       e_wildcard.

%-----------------------------------------------------------------------%

:- type code_info.

:- func code_info_init(context) = code_info.

:- type using_marker
    --->    has_using_marker
    ;       no_using_marker.

:- func code_info_get_context(code_info) = context.

:- func code_info_using_marker(code_info) = using_marker.

:- pred code_info_set_using_marker(using_marker::in,
    code_info::in, code_info::out) is det.

    % Throws an exception if the arity has not been set.
    %
:- func code_info_get_arity(code_info) = arity.

:- pred code_info_set_arity(arity::in, code_info::in, code_info::out) is det.

:- func code_info_get_types(code_info) = list(type_).

:- func code_info_get_maybe_types(code_info) = maybe(list(type_)).

:- pred code_info_set_types(list(type_)::in, code_info::in, code_info::out)
    is det.

:- func code_info_join(code_info, code_info) = code_info.

%-----------------------------------------------------------------------%

:- func expr_get_callees(expr) = set(func_id).

%-----------------------------------------------------------------------%

:- implementation.

:- import_module string.
:- import_module require.

%-----------------------------------------------------------------------%

:- type code_info
    --->    code_info(
                ci_context          :: context,

                ci_using_marker     :: using_marker,

                % How many results does this expression return?
                ci_arity            :: maybe(arity),

                % The type of each result
                ci_types            :: maybe(list(type_))
            ).

code_info_init(Context) = code_info(Context, no_using_marker, no, no).

code_info_get_context(Info) = Info ^ ci_context.

code_info_using_marker(Info) = Info ^ ci_using_marker.

code_info_set_using_marker(UsingMarker, !Info) :-
    !Info ^ ci_using_marker := UsingMarker.

code_info_get_arity(Info) = Arity :-
    MaybeArity = Info ^ ci_arity,
    ( MaybeArity = yes(Arity)
    ; MaybeArity = no,
        unexpected($file, $pred, "Arity has not been set, " ++
            "typechecking must execute before expression arity is known")
    ).

code_info_set_arity(Arity, !Info) :-
    !Info ^ ci_arity := yes(Arity).

code_info_get_types(Info) = Types :-
    MaybeTypes = Info ^ ci_types,
    ( MaybeTypes = yes(Types)
    ; MaybeTypes = no,
        unexpected($file, $pred, "Types unknown")
    ).

code_info_get_maybe_types(Info) = Info ^ ci_types.

code_info_set_types(Types, !Info) :-
    !Info ^ ci_types := yes(Types).

%-----------------------------------------------------------------------%

code_info_join(CIA, CIB) = CI :-
    Context = CIA ^ ci_context,
    ( if
        ( CIA ^ ci_using_marker = has_using_marker
        ; CIB ^ ci_using_marker = has_using_marker
        )
    then
        Using = has_using_marker
    else
        Using = no_using_marker
    ),
    Arity = CIB ^ ci_arity,
    Types = CIB ^ ci_types,
    CI = code_info(Context, Using, Arity, Types).

%-----------------------------------------------------------------------%

expr_get_callees(Expr) = Callees :-
    ExprType = Expr ^ e_type,
    ( ExprType = e_tuple(Exprs),
        Callees = union_list(map(expr_get_callees, Exprs))
    ; ExprType = e_let(_, ExprA, ExprB),
        Callees = union(expr_get_callees(ExprA), expr_get_callees(ExprB))
    ; ExprType = e_call(Callee, _),
        Callees = make_singleton_set(Callee)
    ; ExprType = e_var(_),
        Callees = init
    ; ExprType = e_const(Const),
        ( Const = c_func(Callee),
            % For the purposes of compiler analysis like typechecking this is a
            % callee.
            Callees = make_singleton_set(Callee)
        ;
            ( Const = c_number(_)
            ; Const = c_string(_)
            ),
            Callees = init
        )
    ; ExprType = e_match(_, Cases),
        Callees = union_list(map(case_get_callees, Cases))
    ).

:- func case_get_callees(expr_case) = set(func_id).

case_get_callees(e_case(_, Expr)) = expr_get_callees(Expr).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
