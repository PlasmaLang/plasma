%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module pre.to_core.
%
% Copyright (C) 2015-2019 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
% Plasma parse tree to core representation conversion
%
%-----------------------------------------------------------------------%

:- interface.

:- import_module core.
:- import_module common_types.
:- import_module pre.pre_ds.

%-----------------------------------------------------------------------%

:- pred pre_to_core(func_id::in, pre_procedure::in, core::in, core::out)
    is det.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module maybe.
:- import_module map.
:- import_module require.
:- import_module string.

:- import_module ast.
:- import_module core.code.
:- import_module core.function.
:- import_module pre.util.
:- import_module varmap.
:- import_module util.

%-----------------------------------------------------------------------%

pre_to_core(FuncId, Proc, !Core) :-
    Proc = pre_procedure(_, Varmap, Params, _, Body, _),
    pre_to_core_func(FuncId, Params, [], Body, Varmap, !Core).

:- pred pre_to_core_func(func_id::in, list(var_or_wildcard(var))::in,
    list(var)::in, pre_statements::in, varmap::in, core::in, core::out) is det.

pre_to_core_func(FuncId, Params, Captured, Body0, !.Varmap, !Core) :-
    map_foldl(var_or_make_var, Params, ParamVars,
        !Varmap),
    foldl(pre_to_core_lambda(!.Varmap),
        get_all_lambdas_stmts(Body0), !Core),
    ParamVarsSet = set(ParamVars),
    pre_to_core_stmts(ParamVarsSet, Body0, Body1, !Varmap),
    expr_make_vars_unique(Body1, Body, set.init, _, !Varmap),
    core_get_function_det(!.Core, FuncId, Function0),
    func_set_body(!.Varmap, ParamVars, Captured, Body, Function0, Function),
    core_set_function(FuncId, Function, !Core).

:- pred pre_to_core_stmts(set(var)::in, pre_statements::in, expr::out,
    varmap::in, varmap::out) is det.

pre_to_core_stmts(_, [], empty_tuple, !Varmap).
pre_to_core_stmts(DeclVars0, Stmts0@[_ | _], Expr, !Varmap) :-
    det_split_last(Stmts0, Stmts, LastStmt),
    ( Stmts = [],
        pre_to_core_stmt(LastStmt, LastExpr, Vars, DeclVars0, _, !Varmap),
        terminate_let(Vars, [], LastExpr, Expr)
    ; Stmts = [_ | _],
        map_foldl2(
            (pred(S::in, e_let(V, E)::out, Dv0::in, Dv::out, Vm0::in, Vm::out)
                    is det :-
                pre_to_core_stmt(S, E, V, Dv0, Dv, Vm0, Vm)
            ), Stmts, Lets, DeclVars0, DeclVars, !Varmap),
        pre_to_core_stmt(LastStmt, LastExpr, Vars, DeclVars, _, !Varmap),
        terminate_let(Vars, Lets, LastExpr, Expr)
    ).

:- pred terminate_let(list(var)::in, list(expr_let)::in,
    expr::in, expr::out) is det.

terminate_let([], [], Expr, Expr).
terminate_let([], Lets@[_ | _], LastExpr, Expr) :-
    Expr = expr(e_lets(Lets, LastExpr), LastExpr ^ e_info).
terminate_let(Vars@[_ | _], Lets0, LastExpr, Expr) :-
    Lets = Lets0 ++ [e_let(Vars, LastExpr)],
    Expr = expr(e_lets(Lets, empty_tuple), LastExpr ^ e_info).

    % pre_to_core_stmt(Statement, !Stmts, Expr, !DeclVars, !Varmap).
    %
    % Build Expr from Statement and maybe some of !Stmts.
    %
:- pred pre_to_core_stmt(pre_statement::in, expr::out, list(var)::out,
    set(var)::in, set(var)::out, varmap::in, varmap::out) is det.

pre_to_core_stmt(Stmt, Expr, DefnVars, !DeclVars, !Varmap) :-
    Stmt = pre_statement(StmtType, Info),
    Context = Info ^ si_context,
    ( StmtType = s_call(Call),
        pre_to_core_call(Context, Call, Expr, !Varmap),
        DefnVars = []
    ; StmtType = s_decl_vars(NewDeclVars),
        !:DeclVars = !.DeclVars `union` set(NewDeclVars),
        Expr = empty_tuple,
        DefnVars = []
    ; StmtType = s_assign(Vars0, PreExpr),
        map_foldl(var_or_make_var, Vars0, Vars, !Varmap),
        pre_to_core_expr(Context, PreExpr, Expr, !Varmap),
        DefnVars = Vars
    ; StmtType = s_return(Vars),
        CodeInfo = code_info_init(o_user_return(Context)),
        Expr = expr(
            e_tuple(map((func(V) = expr(e_var(V), CodeInfo)), Vars)),
            CodeInfo),
        DefnVars = []
    ; StmtType = s_match(Var, Cases0),
        % For the initial version we require that all cases fall through, or
        % tha all will execute a return statement.
        Reachable = Info ^ si_reachable,
        ( Reachable = stmt_always_fallsthrough
        ; Reachable = stmt_always_returns
        ; Reachable = stmt_may_return,
            util.sorry($file, $pred,
                "Cannot handle some branches returning and others " ++
                "falling-through")
        ),

        % This statement will become a let expression, binding the
        % variables produced on all branches that are declared outside
        % of the statement.
        ProdVarsSet = Info ^ si_def_vars `intersect` !.DeclVars,
        % Within each case we have to rename these variables. then we
        % can create an expression at the end that returns their values.
        map_foldl(pre_to_core_case_rename(!.DeclVars, ProdVarsSet),
            Cases0, Cases, !Varmap),

        MatchInfo = code_info_init(o_user_body(Context)),
        DefnVars = to_sorted_list(ProdVarsSet),

        Expr = expr(e_match(Var, Cases), MatchInfo)
    ).

:- pred pre_to_core_case_rename(set(var)::in, set(var)::in,
    pre_case::in, expr_case::out, varmap::in, varmap::out) is det.

pre_to_core_case_rename(!.DeclVars, VarsSet,
        pre_case(Pattern0, Stmts), e_case(Pattern, Expr), !Varmap) :-
    pre_to_core_pattern(Pattern0, Pattern1, !DeclVars, !Varmap),
    pre_to_core_stmts(!.DeclVars, Stmts, Expr0, !Varmap),
    ( if not is_empty(VarsSet) then
        make_renaming(VarsSet, Renaming, !Varmap),
        rename_pattern(Renaming, Pattern1, Pattern),
        Info = code_info_init(o_introduced),
        ReturnExpr = expr(e_tuple(map(func(V) = expr(e_var(V), Info),
                to_sorted_list(VarsSet))),
            Info),
        insert_result_expr(ReturnExpr, Expr0, Expr1),
        rename_expr(Renaming, Expr1, Expr)
    else
        Pattern = Pattern1,
        Expr = Expr0
    ).

:- pred pre_to_core_pattern(pre_pattern::in, expr_pattern::out,
    set(var)::in, set(var)::out, varmap::in, varmap::out) is det.

pre_to_core_pattern(p_number(Num), p_num(Num), !DeclVars, !Varmap).
pre_to_core_pattern(p_var(Var), p_variable(Var), !DeclVars, !Varmap) :-
    set.insert(Var, !DeclVars).
pre_to_core_pattern(p_wildcard, p_wildcard, !DeclVars, !Varmap).
pre_to_core_pattern(p_constr(Constr, Args0), p_ctor(Constr, Args),
        !DeclVars, !Varmap) :-
    map_foldl(make_pattern_arg_var, Args0, Args, !Varmap),
    !:DeclVars = !.DeclVars `union` set(Args).

:- pred make_pattern_arg_var(pre_pattern::in, var::out,
    varmap::in, varmap::out) is det.

make_pattern_arg_var(p_number(_), _, !Varmap) :-
    util.sorry($file, $pred,
        "Nested pattern matching (number within other pattern)").
make_pattern_arg_var(p_constr(_, _), _, !Varmap) :-
    util.sorry($file, $pred,
        "Nested pattern matching (constructor within other pattern)").
make_pattern_arg_var(p_var(Var), Var, !Varmap).
make_pattern_arg_var(p_wildcard, Var, !Varmap) :-
    add_anon_var(Var, !Varmap).

:- pred pre_to_core_expr(context::in, pre_expr::in, expr::out,
    varmap::in, varmap::out) is det.

pre_to_core_expr(Context, e_call(Call), Expr, !Varmap) :-
    pre_to_core_call(Context, Call, Expr, !Varmap).
pre_to_core_expr(Context, e_var(Var),
        expr(e_var(Var), code_info_init(o_user_body(Context))), !Varmap).
pre_to_core_expr(Context, e_construction(CtorId, Args0), Expr, !Varmap) :-
    make_arg_exprs(Context, Args0, Args, LetExpr, !Varmap),
    Expr = expr(e_lets([e_let(Args, LetExpr)],
            expr(e_construction(CtorId, Args),
                code_info_init(o_user_body(Context)))),
        code_info_init(o_user_body(Context))).
pre_to_core_expr(Context, e_lambda(Lambda), Expr, !Varmap) :-
    pre_lambda(FuncId, _, MaybeCaptured, _, _) = Lambda,
    ( MaybeCaptured = yes(Captured),
        ( if empty(Captured) then
            % This isn't a closure so we can generate a function reference
            % instead.
            ExprType = e_constant(c_func(FuncId))
        else
            CapturedList = to_sorted_list(Captured),
            ExprType = e_closure(FuncId, CapturedList)
        )
    ; MaybeCaptured = no,
        unexpected($file, $pred, "e_lambda with no captured set")
    ),
    Expr = expr(ExprType, code_info_init(o_user_body(Context))).
pre_to_core_expr(Context, e_constant(Const), expr(e_constant(Const),
        code_info_init(o_user_body(Context))), !Varmap).

:- pred pre_to_core_call(context::in, pre_call::in, expr::out,
    varmap::in, varmap::out) is det.

pre_to_core_call(Context, Call, Expr, !Varmap) :-
    CodeInfo0 = code_info_init(o_user_body(Context)),
    ( Call = pre_call(_, Args0, WithBang)
    ; Call = pre_ho_call(_, Args0, WithBang)
    ),
    ( WithBang = without_bang,
        CodeInfo = CodeInfo0
    ; WithBang = with_bang,
        code_info_set_bang_marker(has_bang_marker, CodeInfo0, CodeInfo)
    ),
    make_arg_exprs(Context, Args0, Args, ArgsLetExpr, !Varmap),
    ( Call = pre_call(Callee, _, _),
        % We could fill in resources here but we do that after type-checking
        % anyway and re-check it then.
        CallExpr = expr(e_call(c_plain(Callee), Args, unknown_resources),
            CodeInfo)
    ; Call = pre_ho_call(CalleeExpr0, _, _),
        add_anon_var(CalleeVar, !Varmap),
        pre_to_core_expr(Context, CalleeExpr0, CalleeExpr, !Varmap),
        CallExpr = expr(e_lets([e_let([CalleeVar], CalleeExpr)],
                expr(e_call(c_ho(CalleeVar), Args, unknown_resources),
                    CodeInfo)),
            code_info_init(o_user_body(Context)))
    ),
    ( Args = [],
        Expr = CallExpr
    ; Args = [_ | _],
        Expr = expr(e_lets([e_let(Args, ArgsLetExpr)], CallExpr),
            code_info_init(o_user_body(Context)))
    ).

:- pred make_arg_exprs(context::in, list(pre_expr)::in, list(var)::out,
    expr::out, varmap::in, varmap::out) is det.

make_arg_exprs(Context, Args0, Args, LetExpr, !Varmap) :-
    map_foldl(pre_to_core_expr(Context), Args0, ArgExprs, !Varmap),
    LetExpr = expr(e_tuple(ArgExprs), code_info_init(o_introduced)),
    make_arg_vars(length(Args0), Args, !Varmap).

%-----------------------------------------------------------------------%

:- pred pre_to_core_lambda(varmap, pre_lambda, core, core).
:- mode pre_to_core_lambda(in, in, in, out) is det.

pre_to_core_lambda(Varmap, pre_lambda(FuncId, Params, MaybeCaptured, _, Body),
        !Core) :-
    ( MaybeCaptured = yes(Captured),
        CapturedList = set.to_sorted_list(Captured),
        pre_to_core_func(FuncId, Params, CapturedList, Body, Varmap, !Core)
    ; MaybeCaptured = no,
        unexpected($file, $pred, "Unfilled capture set")
    ).

%-----------------------------------------------------------------------%

:- pred make_arg_vars(int::in, list(var)::out, varmap::in, varmap::out)
    is det.

make_arg_vars(Num, Vars, !Varmap) :-
    ( if Num = 0 then
        Vars = []
    else
        make_arg_vars(Num - 1, Vars0, !Varmap),
        add_anon_var(Var, !Varmap),
        Vars = [Var | Vars0]
    ).

:- func empty_tuple = expr.

empty_tuple =
    expr(e_tuple([]), code_info_init(o_introduced)).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
