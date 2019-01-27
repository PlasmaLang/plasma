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
:- import_module varmap.
:- import_module util.

%-----------------------------------------------------------------------%

pre_to_core(FuncId, Proc, !Core) :-
    some [!Varmap] (
        Proc = pre_procedure(_, !:Varmap, ParamVarsOrWildcards, _, Body0, _),
        map_foldl(var_or_make_var, ParamVarsOrWildcards, ParamVars,
            !Varmap),
        pre_to_core_stmts(Body0, no, Body, !Varmap),
        core_get_function_det(!.Core, FuncId, Function0),
        func_set_body(!.Varmap, ParamVars, Body, Function0, Function),
        core_set_function(FuncId, Function, !Core)
    ).

:- pred pre_to_core_stmts(pre_statements::in, maybe(expr)::in, expr::out,
    varmap::in, varmap::out) is det.

pre_to_core_stmts([], yes(Expr), Expr, !Varmap).
pre_to_core_stmts([], no, empty_tuple(nil_context), !Varmap).
pre_to_core_stmts([Stmt | Stmts], MaybeContinuation, Expr, !Varmap) :-
    ( Stmts = [],
        MaybeStmtsExpr = MaybeContinuation
    ; Stmts = [_ | _],
        pre_to_core_stmts(Stmts, MaybeContinuation, StmtsExpr, !Varmap),
        MaybeStmtsExpr = yes(StmtsExpr)
    ),
    pre_to_core_stmt(Stmt, MaybeStmtsExpr, Expr, !Varmap).

    % pre_to_core_stmt(Statement, MaybeContinuation, Expr, !Varmap).
    %
    % Build Expr from Statement, MaybeContinuation is the code to execute after
    % Statement, if NULL then return after executing Statement.
    %
:- pred pre_to_core_stmt(pre_statement::in, maybe(expr)::in, expr::out,
    varmap::in, varmap::out) is det.

pre_to_core_stmt(Stmt, MaybeContinue, Expr, !Varmap) :-
    Stmt = pre_statement(StmtType, Info),
    Context = Info ^ si_context,
    CodeInfo = code_info_init(Context),
    ( StmtType = s_call(Call),
        pre_to_core_call(Context, Call, CallExpr, !Varmap),
        ( MaybeContinue = yes(Continue),
            Expr = expr(e_let([], CallExpr, Continue), CodeInfo)
        ; MaybeContinue = no,
            Expr = CallExpr
        )
    ; StmtType = s_assign(Vars0, PreExpr),
        map_foldl(var_or_make_var, Vars0, Vars, !Varmap),
        pre_to_core_expr(Context, PreExpr, LetExpr, !Varmap),
        ( MaybeContinue = yes(Continue),
            Expr = expr(e_let(Vars, LetExpr, Continue), CodeInfo)
        ; MaybeContinue = no,
            Expr = expr(e_let(Vars, LetExpr, empty_tuple(Context)), CodeInfo)
        )
    ; StmtType = s_return(Vars),
        Expr = expr(
            e_tuple(map((func(V) = expr(e_var(V), CodeInfo)), Vars)),
            CodeInfo),
        ( MaybeContinue = yes(_),
            compile_error($file, $pred, Context, "Code after return statement")
        ; MaybeContinue = no
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

        ( MaybeContinue = no,
            map_foldl(pre_to_core_case, Cases0, Cases, !Varmap),
            Expr = expr(e_match(Var, Cases), CodeInfo)
        ; MaybeContinue = yes(Continue),
            % This goal will become a let expression, binding the variables
            % produced on all branches that are non-local.
            ProdVarsSet = Info ^ si_def_vars `intersect` Info ^ si_non_locals,
            % Within each case we have to rename these variables. then we
            % can create an expression at the end that returns their values.
            map_foldl(pre_to_core_case_rename(Context, ProdVarsSet),
                Cases0, Cases, !Varmap),

            MatchInfo = code_info_init(Context),
            LetInfo = code_info_init(Context),
            ProdVars = to_sorted_list(ProdVarsSet),
            Expr = expr(e_let(ProdVars, expr(e_match(Var, Cases), MatchInfo),
                Continue), LetInfo)
        )
    ).

:- pred pre_to_core_case(pre_case::in, expr_case::out, varmap::in, varmap::out)
    is det.

pre_to_core_case(pre_case(Pattern0, Stmts), e_case(Pattern, Expr), !Varmap) :-
    pre_to_core_pattern(Pattern0, Pattern, !Varmap),
    pre_to_core_stmts(Stmts, no, Expr, !Varmap).

:- pred pre_to_core_case_rename(context::in, set(var)::in,
    pre_case::in, expr_case::out, varmap::in, varmap::out) is det.

pre_to_core_case_rename(Context, VarsSet, pre_case(Pattern0, Stmts),
        e_case(Pattern, Expr), !Varmap) :-
    pre_to_core_pattern(Pattern0, Pattern1, !Varmap),
    some [!Renaming] (
        !:Renaming = map.init,
        rename_pattern(VarsSet, Pattern1, Pattern, !Renaming, !Varmap),
        Info = code_info_init(Context),
        ReturnExpr = expr(e_tuple(map(func(V) = expr(e_var(V), Info),
                to_sorted_list(VarsSet))),
            Info),
        pre_to_core_stmts(Stmts, yes(ReturnExpr), Expr0, !Varmap),
        rename_expr(VarsSet, Expr0, Expr, !.Renaming, _, !Varmap)
    ).

:- pred pre_to_core_pattern(pre_pattern::in, expr_pattern::out,
    varmap::in, varmap::out) is det.

pre_to_core_pattern(p_number(Num), p_num(Num), !Varmap).
pre_to_core_pattern(p_var(Var), p_variable(Var), !Varmap).
pre_to_core_pattern(p_wildcard, p_wildcard, !Varmap).
pre_to_core_pattern(p_constr(Constr, Args0), p_ctor(Constr, Args), !Varmap) :-
    map_foldl(make_pattern_arg_var, Args0, Args, !Varmap).

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
    Expr = expr(e_let(Args, LetExpr,
            expr(e_construction(CtorId, Args), code_info_init(Context))),
        code_info_init(Context)).
pre_to_core_expr(_, e_lambda(_), _, !Varmap) :-
    util.sorry($file, $pred, "WIP").
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
        CallExpr = expr(e_let([CalleeVar], CalleeExpr,
                expr(e_call(c_ho(CalleeVar), Args, unknown_resources),
                    CodeInfo)),
            code_info_init(Context))
    ),
    Expr = expr(e_let(Args, ArgsLetExpr, CallExpr),
        code_info_init(Context)).

:- pred make_arg_exprs(context::in, list(pre_expr)::in, list(var)::out,
    expr::out, varmap::in, varmap::out) is det.

make_arg_exprs(Context, Args0, Args, LetExpr, !Varmap) :-
    map_foldl(pre_to_core_expr(Context), Args0, ArgExprs, !Varmap),
    LetExpr = expr(e_tuple(ArgExprs), code_info_init(Context)),
    make_arg_vars(length(Args0), Args, !Varmap).

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
