%-----------------------------------------------------------------------%
% Plasma code representation
% vim: ts=4 sw=4 et
%
% Copyright (C) 2015-2020 Plasma Team
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
    ;       e_lets(list(expr_let), expr)
    ;       e_call(callee, list(var), maybe_resources)
    ;       e_var(var)
    ;       e_constant(const_type)
            % A constructon of one of the possible constructors.  After
            % successful type checking this set contains exactly one item.
    ;       e_construction(set(ctor_id), list(var))
    ;       e_closure(func_id, list(var))
    ;       e_match(var, list(expr_case)).

% All expressions must be matched with a variable or otherwise the root of a
% function.  The typechecker uses this property to attach types to each
% variable and therefore all expressions will have types.  Therefore we
% cannot allow an expression to bind no variables (except the empty tuple
% expression which has no type).  Similarly we cannot cast arity.  Instead
% some variables are bound but never used.
:- type expr_let
    --->    e_let(list(var), expr).

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

:- type code_origin
    --->    o_user_body(context)
    ;       o_user_decl(context)
    ;       o_user_return(context)
    ;       o_builtin
    ;       o_introduced.

:- func code_info_init(code_origin) = code_info.

:- type bang_marker
    --->    has_bang_marker
    ;       no_bang_marker.

:- func code_info_context(code_info) = context.

:- func code_info_origin(code_info) = code_origin.

:- pred code_info_set_origin(code_origin::in,
    code_info::in, code_info::out) is det.

:- func code_info_bang_marker(code_info) = bang_marker.

:- pred code_info_set_bang_marker(bang_marker::in,
    code_info::in, code_info::out) is det.

:- pred code_info_arity(code_info::in, arity::out) is semidet.

    % Throws an exception if the arity has not been set.
    %
:- func code_info_arity_det(code_info) = arity.

:- pred code_info_set_arity(arity::in, code_info::in, code_info::out) is det.

:- func code_info_types(code_info) = list(type_).

:- func code_info_maybe_types(code_info) = maybe(list(type_)).

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

:- pred make_renaming(set(var)::in, map(var, var)::out,
    varmap::in, varmap::out) is det.

:- pred rename_expr(map(var, var)::in, expr::in, expr::out) is det.

:- pred rename_pattern(map(var, var)::in, expr_pattern::in, expr_pattern::out)
    is det.

:- pred expr_make_vars_unique(expr::in, expr::out,
    set(var)::in, set(var)::out, varmap::in, varmap::out) is det.

:- pred expr_has_branch(expr::in) is semidet.

%-----------------------------------------------------------------------%

:- implementation.

:- import_module string.
:- import_module require.

%-----------------------------------------------------------------------%

:- type code_info
    --->    code_info(
                ci_origin           :: code_origin,

                ci_bang_marker      :: bang_marker,

                % How many results does this expression return?
                ci_arity            :: maybe(arity),

                % The type of each result
                ci_types            :: maybe(list(type_))
            ).

code_info_init(Origin) = code_info(Origin, no_bang_marker, no, no).

code_info_context(Info) = Context :-
    Origin = Info ^ ci_origin,
    ( if origin_context(Origin, ContextP) then
        Context = ContextP
    else
        Context = nil_context
    ).

code_info_origin(Info) = Info ^ ci_origin.

code_info_set_origin(Origin, !Info) :-
    !Info ^ ci_origin := Origin.

code_info_bang_marker(Info) = Info ^ ci_bang_marker.

code_info_set_bang_marker(BangMarker, !Info) :-
    !Info ^ ci_bang_marker := BangMarker.

code_info_arity(Info, Arity) :-
    yes(Arity) = Info ^ ci_arity.

code_info_arity_det(Info) = Arity :-
    ( if code_info_arity(Info, ArityP) then
        Arity = ArityP
    else
        unexpected($file, $pred, "Arity has not been set, " ++
            "typechecking must execute before expression arity is known")
    ).

code_info_set_arity(Arity, !Info) :-
    !Info ^ ci_arity := yes(Arity).

code_info_types(Info) = Types :-
    MaybeTypes = Info ^ ci_types,
    ( MaybeTypes = yes(Types)
    ; MaybeTypes = no,
        unexpected($file, $pred, "Types unknown")
    ).

code_info_maybe_types(Info) = Info ^ ci_types.

code_info_set_types(Types, !Info) :-
    !Info ^ ci_types := yes(Types).

%-----------------------------------------------------------------------%

code_info_join(CIA, CIB) = CI :-
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
    Origin = origin_join(CIA ^ ci_origin, CIB ^ ci_origin),
    CI = code_info(Origin, Bang, Arity, Types).

:- func origin_join(code_origin, code_origin) = code_origin.

origin_join(O@o_user_body(_), _) = O.
origin_join(O1@o_user_decl(_), O2) = O :-
    ( if O2 = o_user_body(_) then
        O = O2
    else if origin_context(O2, C) then
        O = o_user_body(C)
    else
        O = O1
    ).
origin_join(O1@o_user_return(_), O2) = O :-
    ( if
        ( O2 = o_user_body(_)
        ; O2 = o_user_decl(_)
        )
    then
        O = O2
    else if origin_context(O2, C) then
        O = o_user_return(C)
    else
        O = O1
    ).
origin_join(o_builtin, O2) = O :-
    ( if O2 = o_introduced then
        O = o_builtin
    else
        O = O2
    ).
origin_join(o_introduced, O) = O.

:- pred origin_context(code_origin::in, context::out) is semidet.

origin_context(Origin, Context) :-
    require_complete_switch [Origin]
    ( Origin = o_user_body(Context)
    ; Origin = o_user_decl(Context)
    ; Origin = o_user_return(Context)
    ;
        ( Origin = o_builtin
        ; Origin = o_introduced
        ),
        fail
    ).

%-----------------------------------------------------------------------%

expr_get_callees(Expr) = Callees :-
    ExprType = Expr ^ e_type,
    ( ExprType = e_tuple(Exprs),
        Callees = union_list(map(expr_get_callees, Exprs))
    ; ExprType = e_lets(Lets, InExpr),
        Callees = union_list(
                map(func(e_let(_, E)) = expr_get_callees(E), Lets))
            `union` expr_get_callees(InExpr)
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
        Expr = expr(e_lets([e_let([], Expr0)], LastExpr),
            code_info_join(Expr0 ^ e_info, LastExpr ^ e_info))
    ; ExprType = e_tuple(Exprs),
        ( Exprs = [_ | _],
            Expr = expr(e_lets([e_let([], Expr0)], LastExpr),
                code_info_join(Expr0 ^ e_info, LastExpr ^ e_info))
        ; Exprs = [],
            Expr = LastExpr
        )
    ; ExprType = e_match(Var, Cases0),
        map(insert_result_case(LastExpr), Cases0, Cases),
        Expr = expr(e_match(Var, Cases),
            code_info_join(Expr0 ^ e_info, LastExpr ^ e_info))

    ; ExprType = e_lets(Lets, InExpr0),
        insert_result_expr(LastExpr, InExpr0, InExpr),
        Expr = expr(e_lets(Lets, InExpr),
            code_info_join(Expr0 ^ e_info, InExpr ^ e_info))
    ).

:- pred insert_result_case(expr::in, expr_case::in, expr_case::out) is det.

insert_result_case(LastExpr, e_case(Pat, Expr0), e_case(Pat, Expr)) :-
    insert_result_expr(LastExpr, Expr0, Expr).

%-----------------------------------------------------------------------%

make_renaming(Vars, Renaming, !Varset) :-
    foldl2(make_renaming_var, Vars, map.init, Renaming, !Varset).

:- pred make_renaming_var(var::in, map(var, var)::in, map(var, var)::out,
    varmap::in, varmap::out) is det.

make_renaming_var(Var0, !Renaming, !Varmap) :-
    add_fresh_var(get_var_name_no_suffix(!.Varmap, Var0), Var, !Varmap),
    det_insert(Var0, Var, !Renaming).

rename_expr(Renaming, expr(ExprType0, Info), expr(ExprType, Info)) :-
    ( ExprType0 = e_tuple(Exprs0),
        map(rename_expr(Renaming), Exprs0, Exprs),
        ExprType = e_tuple(Exprs)
    ; ExprType0 = e_lets(Lets0, InExpr0),
        map(rename_let(Renaming), Lets0, Lets),
        rename_expr(Renaming, InExpr0, InExpr),
        ExprType = e_lets(Lets, InExpr)
    ; ExprType0 = e_call(Callee0, Args0, MaybeResources),
        map(rename_var(Renaming), Args0, Args),
        ( Callee0 = c_plain(_),
            Callee = Callee0
        ; Callee0 = c_ho(CalleeVar0),
            rename_var(Renaming, CalleeVar0, CalleeVar),
            Callee = c_ho(CalleeVar)
        ),
        ExprType = e_call(Callee, Args, MaybeResources)
    ; ExprType0 = e_var(Var0),
        rename_var(Renaming, Var0, Var),
        ExprType = e_var(Var)
    ; ExprType0 = e_constant(_),
        ExprType = ExprType0
    ; ExprType0 = e_construction(Constrs, Args0),
        map(rename_var(Renaming), Args0, Args),
        ExprType = e_construction(Constrs, Args)
    ; ExprType0 = e_closure(FuncId, Args0),
        map(rename_var(Renaming), Args0, Args),
        ExprType = e_closure(FuncId, Args)
    ; ExprType0 = e_match(Var0, Cases0),
        rename_var(Renaming, Var0, Var),
        map(rename_case(Renaming), Cases0, Cases),
        ExprType = e_match(Var, Cases)
    ).

:- pred rename_let(map(var, var)::in, expr_let::in, expr_let::out) is det.

rename_let(Renaming, e_let(Vars0, Expr0), e_let(Vars, Expr)) :-
    map(rename_var(Renaming), Vars0, Vars),
    rename_expr(Renaming, Expr0, Expr).

:- pred rename_case(map(var, var)::in, expr_case::in, expr_case::out) is det.

rename_case(Renaming, e_case(Pat0, Expr0), e_case(Pat, Expr)) :-
    rename_pattern(Renaming, Pat0, Pat),
    rename_expr(Renaming, Expr0, Expr).

:- pred rename_var(map(var, var)::in, var::in, var::out) is det.

rename_var(Renaming, Var0, Var) :-
    ( if search(Renaming, Var0, VarPrime) then
        Var = VarPrime
    else
        Var = Var0
    ).

%-----------------------------------------------------------------------%

rename_pattern(_, p_num(Num), p_num(Num)).
rename_pattern(Renaming, p_variable(Var0), p_variable(Var)) :-
    rename_var(Renaming, Var0, Var).
rename_pattern(_, p_wildcard, p_wildcard).
rename_pattern(Renaming, p_ctor(C, Args0), p_ctor(C, Args)) :-
    map(rename_var(Renaming), Args0, Args).

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
    ; Type = e_lets(Lets0, In0),
        map_foldl3(let_make_vars_unique, Lets0, Lets, map.init, Renaming,
            !SeenVars, !Varmap),
        rename_expr(Renaming, In0, In1),
        expr_make_vars_unique(In1, In, !SeenVars, !Varmap),
        Expr = expr(e_lets(Lets, In), Info)
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

:- pred let_make_vars_unique(expr_let::in, expr_let::out,
    map(var, var)::in, map(var, var)::out, set(var)::in, set(var)::out,
    varmap::in, varmap::out) is det.

let_make_vars_unique(e_let(Vars0, Expr0), e_let(Vars, Expr), !Renaming,
        !SeenVars, !Varmap) :-
    % There are two steps.

    % First do the renaming computed after visiting earlier lets.
    ( if not is_empty(!.Renaming) then
        rename_expr(!.Renaming, Expr0, Expr1)
    else
        Expr1 = Expr0
    ),

    % Then update the renaming for variables seen here.
    VarsToRename = list_to_set(Vars0) `intersect` !.SeenVars,
    ( if not is_empty(VarsToRename) then
        make_renaming(VarsToRename, Renaming, !Varmap),
        !:Renaming = merge(!.Renaming, Renaming),
        map(rename_var(Renaming), Vars0, Vars)
    else
        Vars = Vars0
    ),
    !:SeenVars = !.SeenVars `union` list_to_set(Vars),
    expr_make_vars_unique(Expr1, Expr, !SeenVars, !Varmap).

:- pred case_make_vars_unique(expr_case::in, expr_case::out,
    set(var)::in, set(var)::out, varmap::in, varmap::out) is det.

case_make_vars_unique(e_case(Pat0, Expr0), e_case(Pat, Expr), !SeenVars,
        !Varmap) :-
    ( Pat0 = p_variable(Var0),
        ( if member(Var0, !.SeenVars) then
            VarToRenameSet = make_singleton_set(Var0),
            some [!Renaming] (
                make_renaming(VarToRenameSet, Renaming, !Varmap),
                rename_var(Renaming, Var0, Var),
                rename_expr(Renaming, Expr0, Expr1)
            )
        else
            Var = Var0,
            Expr1 = Expr0
        ),
        insert(Var, !SeenVars),
        Pat = p_variable(Var)
    ; Pat0 = p_ctor(Ctor, Vars0),
        VarsToRename = !.SeenVars `intersect` list_to_set(Vars0),
        ( if not is_empty(VarsToRename) then
            make_renaming(VarsToRename, Renaming, !Varmap),
            map(rename_var(Renaming), Vars0, Vars),
            rename_expr(Renaming, Expr0, Expr1)
        else
            Vars = Vars0,
            Expr1 = Expr0
        ),
        !:SeenVars = !.SeenVars `union` list_to_set(Vars),
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

expr_has_branch(expr(Type, _)) :-
    require_complete_switch [Type]
    ( Type = e_tuple(Exprs),
        any_true(expr_has_branch, Exprs)
    ; Type = e_lets(Lets, Expr),
        (
            any_true((pred(e_let(_, E)::in) is semidet :-
                    expr_has_branch(E)
                ), Lets)
        ;
            expr_has_branch(Expr)
        )
    ;
        ( Type = e_call(_, _, _)
        ; Type = e_var(_)
        ; Type = e_constant(_)
        ; Type = e_construction(_, _)
        ; Type = e_closure(_, _)
        ),
        false
    ;
        Type = e_match(_, _)
    ).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
