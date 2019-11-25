%-----------------------------------------------------------------------%
% Plasma code representation
% vim: ts=4 sw=4 et
%
% Copyright (C) 2015-2019 Plasma Team
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
    ;       e_call(callee, list(var), maybe_resources)
    ;       e_var(var)
    ;       e_constant(const_type)
    ;       e_construction(ctor_id, list(var))
    ;       e_closure(func_id, list(var))
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

    % Merge to code_infos,  The context of the first overrides the 2nd,
    % while the types and arity (result information) of the 2nd overrides
    % the first.  This is suitable for composing let expressions from two
    % other expressions' code_infos.
    %
:- func code_info_join(code_info, code_info) = code_info.

%-----------------------------------------------------------------------%

:- func expr_get_callees(expr) = set(func_id).

%-----------------------------------------------------------------------%

:- pred insert_result_expr(expr::in, expr::in, expr::out) is det.

%-----------------------------------------------------------------------%

:- pred rename_expr(set(var)::in, expr::in, expr::out,
    map(var, var)::in, map(var, var)::out, varmap::in, varmap::out) is det.

:- pred rename_pattern(set(var)::in, expr_pattern::in, expr_pattern::out,
    map(var, var)::in, map(var, var)::out, varmap::in, varmap::out) is det.

:- pred expr_make_vars_unique(expr::in, expr::out,
    set(var)::in, set(var)::out, varmap::in, varmap::out) is det.

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
    ; ExprType = e_call(Callee, _, _),
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
    ; ExprType = e_closure(Callee, _),
        Callees = make_singleton_set(Callee)
    ; ExprType = e_match(_, Cases),
        Callees = union_list(map(case_get_callees, Cases))
    ).

:- func case_get_callees(expr_case) = set(func_id).

case_get_callees(e_case(_, Expr)) = expr_get_callees(Expr).

%-----------------------------------------------------------------------%

insert_result_expr(LastExpr, Expr0, Expr) :-
    ExprType = Expr0 ^ e_type,
    (
        ( ExprType = e_call(_, _, _)
        ; ExprType = e_var(_)
        ; ExprType = e_constant(_)
        ; ExprType = e_construction(_, _)
        ; ExprType = e_closure(_, _)
        ),
        Expr = expr(e_let([], Expr0, LastExpr),
            code_info_join(Expr0 ^ e_info, LastExpr ^ e_info))
    ; ExprType = e_tuple(Exprs),
        ( Exprs = [_ | _],
            Expr = expr(e_let([], Expr0, LastExpr),
                code_info_join(Expr0 ^ e_info, LastExpr ^ e_info))
        ; Exprs = [],
            Expr = LastExpr
        )
    ; ExprType = e_match(Var, Cases0),
        map(insert_result_case(LastExpr), Cases0, Cases),
        Expr = expr(e_match(Var, Cases),
            code_info_join(Expr0 ^ e_info, LastExpr ^ e_info))

    ; ExprType = e_let(Vars, LetExpr, InExpr0),
        insert_result_expr(LastExpr, InExpr0, InExpr),
        Expr = expr(e_let(Vars, LetExpr, InExpr),
            code_info_join(LetExpr ^ e_info, InExpr ^ e_info))
    ).

:- pred insert_result_case(expr::in, expr_case::in, expr_case::out) is det.

insert_result_case(LastExpr, e_case(Pat, Expr0), e_case(Pat, Expr)) :-
    insert_result_expr(LastExpr, Expr0, Expr).

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
    ; ExprType0 = e_call(Callee0, Args0, MaybeResources),
        map_foldl2(rename_var(Vars), Args0, Args, !Renaming, !Varmap),
        ( Callee0 = c_plain(_),
            Callee = Callee0
        ; Callee0 = c_ho(CalleeVar0),
            rename_var(Vars, CalleeVar0, CalleeVar, !Renaming, !Varmap),
            Callee = c_ho(CalleeVar)
        ),
        ExprType = e_call(Callee, Args, MaybeResources)
    ; ExprType0 = e_var(Var0),
        rename_var(Vars, Var0, Var, !Renaming, !Varmap),
        ExprType = e_var(Var)
    ; ExprType0 = e_constant(_),
        ExprType = ExprType0
    ; ExprType0 = e_construction(Constr, Args0),
        map_foldl2(rename_var(Vars), Args0, Args, !Renaming, !Varmap),
        ExprType = e_construction(Constr, Args)
    ; ExprType0 = e_closure(FuncId, Args0),
        map_foldl2(rename_var(Vars), Args0, Args, !Renaming, !Varmap),
        ExprType = e_closure(FuncId, Args)
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
            add_fresh_var(get_var_name_no_suffix(!.Varmap, Var0), Var, !Varmap),
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

    % TODO: This is higher complexity than it needs to be.  if it finds a
    % variable that needs to be renamed it will perform the rename
    % (traversing the sub-expression(s)) and then traverse those again to
    % look for more variables to rename.
    %
expr_make_vars_unique(Expr0, Expr, !SeenVars, !Varmap) :-
    expr(Type, Info) = Expr0,
    ( Type = e_tuple(Exprs0),
        map_foldl2(expr_make_vars_unique, Exprs0, Exprs, !SeenVars, !Varmap),
        Expr = expr(e_tuple(Exprs), Info)
    ; Type = e_let(Vars0, Let0, In0),
        VarsToRename = set(Vars0) `intersect` !.SeenVars,
        ( if not is_empty(VarsToRename) then
            some [!Renaming] (
                !:Renaming = map.init,
                map_foldl2(rename_var(VarsToRename), Vars0, Vars, !Renaming,
                    !Varmap),
                rename_expr(VarsToRename, Let0, Let1, !Renaming, !Varmap),
                rename_expr(VarsToRename, In0, In1, !.Renaming, _, !Varmap)
            )
        else
            Vars = Vars0,
            Let1 = Let0,
            In1 = In0
        ),
        !:SeenVars = !.SeenVars `union` set(Vars),
        expr_make_vars_unique(Let1, Let, !SeenVars, !Varmap),
        expr_make_vars_unique(In1, In, !SeenVars, !Varmap),
        Expr = expr(e_let(Vars, Let, In), Info)
    ; Type = e_match(Var, Cases0),
        map_foldl2(case_make_vars_unique, Cases0, Cases, !SeenVars, !Varmap),
        Expr = expr(e_match(Var, Cases), Info)
    ;
        ( Type = e_call(_, _, _)
        ; Type = e_var(_)
        ; Type = e_constant(_)
        ; Type = e_construction(_, _)
        ; Type = e_closure(_, _)
        ),
        Expr = Expr0
    ).

:- pred case_make_vars_unique(expr_case::in, expr_case::out,
    set(var)::in, set(var)::out, varmap::in, varmap::out) is det.

case_make_vars_unique(e_case(Pat0, Expr0), e_case(Pat, Expr), !SeenVars,
        !Varmap) :-
    ( Pat0 = p_variable(Var0),
        ( if member(Var0, !.SeenVars) then
            VarToRenameSet = make_singleton_set(Var0),
            some [!Renaming] (
                !:Renaming = map.init,
                rename_var(VarToRenameSet, Var0, Var, !Renaming, !Varmap),
                rename_expr(VarToRenameSet, Expr0, Expr1, !.Renaming, _,
                    !Varmap)
            )
        else
            Var = Var0,
            Expr1 = Expr0
        ),
        insert(Var, !SeenVars),
        Pat = p_variable(Var)
    ; Pat0 = p_ctor(Ctor, Vars0),
        VarsToRename = !.SeenVars `intersect` set(Vars0),
        ( if not is_empty(VarsToRename) then
            some [!Renaming] (
                !:Renaming = map.init,
                map_foldl2(rename_var(VarsToRename), Vars0, Vars, !Renaming,
                    !Varmap),
                rename_expr(VarsToRename, Expr0, Expr1, !.Renaming, _,
                    !Varmap)
            )
        else
            Vars = Vars0,
            Expr1 = Expr0
        ),
        !:SeenVars = !.SeenVars `union` set(Vars),
        Pat = p_ctor(Ctor, Vars)
    ;
        ( Pat0 = p_num(_)
        ; Pat0 = p_wildcard
        ),
        Pat = Pat0,
        Expr1 = Expr0
    ),
    expr_make_vars_unique(Expr1, Expr, !SeenVars, !Varmap).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
