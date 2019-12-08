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

pre_to_core_stmts(_, [], empty_tuple(nil_context), !Varmap).
pre_to_core_stmts(DeclVars0, [Stmt | Stmts0], Expr, !Varmap) :-
    pre_to_core_stmt(Stmt, Stmts0, Stmts, StmtExpr, DeclVars0, DeclVars,
        !Varmap),
    ( Stmts = [],
        Expr = StmtExpr
    ; Stmts = [_ | _],
        pre_to_core_stmts(DeclVars, Stmts, StmtsExpr, !Varmap),
        expect(set.empty(Stmt ^ s_info ^ si_def_vars), $file, $pred,
            "These statements can't define variables"),
        Expr = expr(e_lets([e_let([], StmtExpr)], StmtsExpr),
            code_info_join(StmtExpr ^ e_info, StmtsExpr ^ e_info))
    ).

    % pre_to_core_stmt(Statement, !Stmts, Expr, !DeclVars, !Varmap).
    %
    % Build Expr from Statement and maybe some of !Stmts.
    %
:- pred pre_to_core_stmt(pre_statement::in, pre_statements::in,
    pre_statements::out, expr::out, set(var)::in, set(var)::out,
    varmap::in, varmap::out) is det.

pre_to_core_stmt(Stmt, !Stmts, Expr, !DeclVars, !Varmap) :-
    Stmt = pre_statement(StmtType, Info),
    Context = Info ^ si_context,
    CodeInfo = code_info_init(Context),
    ( StmtType = s_call(Call),
        pre_to_core_call(Context, Call, Expr, !Varmap)
    ; StmtType = s_decl_vars(NewDeclVars),
        !:DeclVars = !.DeclVars `union` set(NewDeclVars),
        Expr = empty_tuple(Context)
    ; StmtType = s_assign(Vars0, PreExpr),
        map_foldl(var_or_make_var, Vars0, Vars, !Varmap),
        pre_to_core_expr(Context, PreExpr, LetExpr, !Varmap),
        pre_to_core_stmts(!.DeclVars, !.Stmts, InExpr, !Varmap),
        !:Stmts = [],
        Expr = expr(e_lets([e_let(Vars, LetExpr)], InExpr), CodeInfo)
    ; StmtType = s_return(Vars),
        Expr = expr(
            e_tuple(map((func(V) = expr(e_var(V), CodeInfo)), Vars)),
            CodeInfo),
        ( !.Stmts = [_ | _],
            compile_error($file, $pred, Context, "Code after return statement")
        ; !.Stmts = []
        )
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

        ( !.Stmts = [],
            map_foldl(pre_to_core_case(!.DeclVars), Cases0, Cases, !Varmap),
            Expr = expr(e_match(Var, Cases), CodeInfo)
        ; !.Stmts = [_ | _],
            % This statement will become a let expression, binding the
            % variables produced on all branches that are declared outside
            % of the statement.
            ProdVarsSet = Info ^ si_def_vars `intersect` !.DeclVars,
            % Within each case we have to rename these variables. then we
            % can create an expression at the end that returns their values.
            map_foldl(pre_to_core_case_rename(Context, !.DeclVars, ProdVarsSet),
                Cases0, Cases, !Varmap),

            MatchInfo = code_info_init(Context),
            LetInfo = code_info_init(Context),
            ProdVars = to_sorted_list(ProdVarsSet),

            pre_to_core_stmts(!.DeclVars, !.Stmts, InExpr, !Varmap),
            !:Stmts = [],

            Expr = expr(e_lets([e_let(ProdVars,
                    expr(e_match(Var, Cases), MatchInfo))],
                InExpr), LetInfo)
        )
    ).

:- pred pre_to_core_case(set(var)::in, pre_case::in, expr_case::out,
    varmap::in, varmap::out) is det.

pre_to_core_case(!.DeclVars, pre_case(Pattern0, Stmts),
        e_case(Pattern, Expr), !Varmap) :-
    pre_to_core_pattern(Pattern0, Pattern, !DeclVars, !Varmap),
    pre_to_core_stmts(!.DeclVars, Stmts, Expr, !Varmap).

:- pred pre_to_core_case_rename(context::in, set(var)::in, set(var)::in,
    pre_case::in, expr_case::out, varmap::in, varmap::out) is det.

pre_to_core_case_rename(Context, !.DeclVars, VarsSet,
        pre_case(Pattern0, Stmts), e_case(Pattern, Expr), !Varmap) :-
    pre_to_core_pattern(Pattern0, Pattern1, !DeclVars, !Varmap),
    make_renaming(VarsSet, Renaming, !Varmap),
    rename_pattern(Renaming, Pattern1, Pattern),
    Info = code_info_init(Context),
    pre_to_core_stmts(!.DeclVars, Stmts, Expr0, !Varmap),
    ReturnExpr = expr(e_tuple(map(func(V) = expr(e_var(V), Info),
            to_sorted_list(VarsSet))),
        Info),
    insert_result_expr(ReturnExpr, Expr0, Expr1),
    rename_expr(Renaming, Expr1, Expr).

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
        expr(e_var(Var), code_info_init(Context)), !Varmap).
pre_to_core_expr(Context, e_construction(CtorId, Args0), Expr, !Varmap) :-
    make_arg_exprs(Context, Args0, Args, LetExpr, !Varmap),
    Expr = expr(e_lets([e_let(Args, LetExpr)],
            expr(e_construction(CtorId, Args), code_info_init(Context))),
        code_info_init(Context)).
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
    Expr = expr(ExprType, code_info_init(Context)).
pre_to_core_expr(Context, e_constant(Const), expr(e_constant(Const),
        code_info_init(Context)), !Varmap).

:- pred pre_to_core_call(context::in, pre_call::in, expr::out,
    varmap::in, varmap::out) is det.

pre_to_core_call(Context, Call, Expr, !Varmap) :-
    CodeInfo0 = code_info_init(Context),
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
            code_info_init(Context))
    ),
    Expr = expr(e_lets([e_let(Args, ArgsLetExpr)], CallExpr),
        code_info_init(Context)).

:- pred make_arg_exprs(context::in, list(pre_expr)::in, list(var)::out,
    expr::out, varmap::in, varmap::out) is det.

make_arg_exprs(Context, Args0, Args, LetExpr, !Varmap) :-
    map_foldl(pre_to_core_expr(Context), Args0, ArgExprs, !Varmap),
    LetExpr = expr(e_tuple(ArgExprs), code_info_init(Context)),
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

:- func empty_tuple(context) = expr.

empty_tuple(Context) = expr(e_tuple([]), code_info_init(Context)).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
