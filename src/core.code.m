%-----------------------------------------------------------------------%
% Plasma code representation
% vim: ts=4 sw=4 et
%
% Copyright (C) 2015-2017 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
%-----------------------------------------------------------------------%
:- module core.code.
%-----------------------------------------------------------------------%

:- interface.

:- import_module context.
:- import_module common_types.

%-----------------------------------------------------------------------%

:- type expr
    --->    expr(
                e_type      :: expr_type,
                e_info      :: code_info
            ).

:- type expr_type
    --->    e_tuple(list(expr))
            % TODO: Allow anonymous binds.
    ;       e_let(list(var), expr, expr)
    ;       e_call(callee, list(var))
    ;       e_var(var)
    ;       e_constant(const_type)
    ;       e_construction(ctor_id, list(var))
    ;       e_match(var, list(expr_case)).

:- type expr_case
    --->    e_case(expr_pattern, expr).

:- type expr_pattern
    --->    p_num(int)
    ;       p_variable(var)
    ;       p_wildcard
    ;       p_ctor(ctor_id, list(var)).

:- type callee
    --->    c_plain(func_id)
    ;       c_ho(var).

%-----------------------------------------------------------------------%

:- type code_info.

:- func code_info_init(context) = code_info.

:- type bang_marker
    --->    has_bang_marker
    ;       no_bang_marker.

:- func code_info_get_context(code_info) = context.

:- func code_info_bang_marker(code_info) = bang_marker.

:- pred code_info_set_bang_marker(bang_marker::in,
    code_info::in, code_info::out) is det.

:- pred code_info_get_arity(code_info::in, arity::out) is semidet.

    % Throws an exception if the arity has not been set.
    %
:- func code_info_get_arity_det(code_info) = arity.

:- pred code_info_set_arity(arity::in, code_info::in, code_info::out) is det.

:- func code_info_get_types(code_info) = list(type_).

:- func code_info_get_maybe_types(code_info) = maybe(list(type_)).

:- pred code_info_set_types(list(type_)::in, code_info::in, code_info::out)
    is det.

:- func code_info_join(code_info, code_info) = code_info.

%-----------------------------------------------------------------------%

:- func expr_get_callees(expr) = set(func_id).

%-----------------------------------------------------------------------%

:- pred rename_expr(set(var)::in, expr::in, expr::out,
    map(var, var)::in, map(var, var)::out, varmap::in, varmap::out) is det.

:- pred rename_pattern(set(var)::in, expr_pattern::in, expr_pattern::out,
    map(var, var)::in, map(var, var)::out, varmap::in, varmap::out) is det.

%-----------------------------------------------------------------------%

:- implementation.

:- import_module string.
:- import_module require.

%-----------------------------------------------------------------------%

:- type code_info
    --->    code_info(
                ci_context          :: context,

                ci_bang_marker      :: bang_marker,

                % How many results does this expression return?
                ci_arity            :: maybe(arity),

                % The type of each result
                ci_types            :: maybe(list(type_))
            ).

code_info_init(Context) = code_info(Context, no_bang_marker, no, no).

code_info_get_context(Info) = Info ^ ci_context.

code_info_bang_marker(Info) = Info ^ ci_bang_marker.

code_info_set_bang_marker(BangMarker, !Info) :-
    !Info ^ ci_bang_marker := BangMarker.

code_info_get_arity(Info, Arity) :-
    yes(Arity) = Info ^ ci_arity.

code_info_get_arity_det(Info) = Arity :-
    ( if code_info_get_arity(Info, ArityP) then
        Arity = ArityP
    else
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
        ( CIA ^ ci_bang_marker = has_bang_marker
        ; CIB ^ ci_bang_marker = has_bang_marker
        )
    then
        Bang = has_bang_marker
    else
        Bang = no_bang_marker
    ),
    Arity = CIB ^ ci_arity,
    Types = CIB ^ ci_types,
    CI = code_info(Context, Bang, Arity, Types).

%-----------------------------------------------------------------------%

expr_get_callees(Expr) = Callees :-
    ExprType = Expr ^ e_type,
    ( ExprType = e_tuple(Exprs),
        Callees = union_list(map(expr_get_callees, Exprs))
    ; ExprType = e_let(_, ExprA, ExprB),
        Callees = union(expr_get_callees(ExprA), expr_get_callees(ExprB))
    ; ExprType = e_call(Callee, _),
        ( Callee = c_plain(FuncId),
            Callees = make_singleton_set(FuncId)
        ; Callee = c_ho(_),
            Callees = init
        )
    ; ExprType = e_var(_),
        Callees = init
    ; ExprType = e_constant(Const),
        ( Const = c_func(Callee),
            % For the purposes of compiler analysis like typechecking this is a
            % callee.
            Callees = make_singleton_set(Callee)
        ;
            ( Const = c_number(_)
            ; Const = c_string(_)
            ;
                % XXX: This could be a problem if constructors can be used
                % as functions (in higher-order contexts)
                Const = c_ctor(_)
            ),
            Callees = init
        )
    ; ExprType = e_construction(_, _),
        Callees = set.init
    ; ExprType = e_match(_, Cases),
        Callees = union_list(map(case_get_callees, Cases))
    ).

:- func case_get_callees(expr_case) = set(func_id).

case_get_callees(e_case(_, Expr)) = expr_get_callees(Expr).

%-----------------------------------------------------------------------%

rename_expr(Vars, expr(ExprType0, Info), expr(ExprType, Info),
        !Renaming, !Varmap) :-
    ( ExprType0 = e_tuple(Exprs0),
        map_foldl2(rename_expr(Vars), Exprs0, Exprs, !Renaming, !Varmap),
        ExprType = e_tuple(Exprs)
    ; ExprType0 = e_let(LetVars0, LetExpr0, InExpr0),
        map_foldl2(rename_var(Vars), LetVars0, LetVars, !Renaming, !Varmap),
        rename_expr(Vars, LetExpr0, LetExpr, !Renaming, !Varmap),
        rename_expr(Vars, InExpr0, InExpr, !Renaming, !Varmap),
        ExprType = e_let(LetVars, LetExpr, InExpr)
    ; ExprType0 = e_call(Callee0, Args0),
        map_foldl2(rename_var(Vars), Args0, Args, !Renaming, !Varmap),
        ( Callee0 = c_plain(_),
            Callee = Callee0
        ; Callee0 = c_ho(CalleeVar0),
            rename_var(Vars, CalleeVar0, CalleeVar, !Renaming, !Varmap),
            Callee = c_ho(CalleeVar)
        ),
        ExprType = e_call(Callee, Args)
    ; ExprType0 = e_var(Var0),
        rename_var(Vars, Var0, Var, !Renaming, !Varmap),
        ExprType = e_var(Var)
    ; ExprType0 = e_constant(_),
        ExprType = ExprType0
    ; ExprType0 = e_construction(Constr, Args0),
        map_foldl2(rename_var(Vars), Args0, Args, !Renaming, !Varmap),
        ExprType = e_construction(Constr, Args)
    ; ExprType0 = e_match(Var0, Cases0),
        rename_var(Vars, Var0, Var, !Renaming, !Varmap),
        map_foldl2(rename_case(Vars), Cases0, Cases, !Renaming, !Varmap),
        ExprType = e_match(Var, Cases)
    ).

:- pred rename_case(set(var)::in, expr_case::in, expr_case::out,
    map(var, var)::in, map(var, var)::out, varmap::in, varmap::out) is det.

rename_case(Vars, e_case(Pat0, Expr0), e_case(Pat, Expr), !Renaming,
        !Varmap) :-
    rename_pattern(Vars, Pat0, Pat, !Renaming, !Varmap),
    rename_expr(Vars, Expr0, Expr, !Renaming, !Varmap).

:- pred rename_var(set(var)::in, var::in, var::out,
    map(var, var)::in, map(var, var)::out, varmap::in, varmap::out) is det.

rename_var(Vars, Var0, Var, !Renaming, !Varmap) :-
    ( if member(Var0, Vars) then
        ( if search(!.Renaming, Var0, VarPrime) then
            Var = VarPrime
        else
            % XXX: Name the variable.
            add_anon_var(Var, !Varmap),
            det_insert(Var0, Var, !Renaming)
        )
    else
        Var = Var0
    ).

%-----------------------------------------------------------------------%

rename_pattern(_, p_num(Num), p_num(Num), !Renaming, !Varmap).
rename_pattern(Vars, p_variable(Var0), p_variable(Var), !Renaming, !Varmap) :-
    rename_var(Vars, Var0, Var, !Renaming, !Varmap).
rename_pattern(_, p_wildcard, p_wildcard, !Renaming, !Varmap).
rename_pattern(Vars, p_ctor(C, Args0), p_ctor(C, Args), !Renaming, !Varmap) :-
    map_foldl2(rename_var(Vars), Args0, Args, !Renaming, !Varmap).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
