%-----------------------------------------------------------------------%
% Plasma AST symbol resolution
% vim: ts=4 sw=4 et
%
% Copyright (C) 2016, 2019 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
% This module fixes variable usage in branching code.  It:
%   * fixes var-def sets
%   * Determines some reachability information (WRT return statements).
%   * checks that used variables are always well defined (eg
%     along all execution paths)
%   * names-appart branch-local variables (from other
%     branches).
%
%-----------------------------------------------------------------------%
:- module pre.branches.
%-----------------------------------------------------------------------%

:- interface.

:- import_module compile_error.
:- import_module pre.pre_ds.
:- import_module result.

%-----------------------------------------------------------------------%

:- func fix_branches(pre_procedure) =
    result(pre_procedure, compile_error).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
:- implementation.

:- import_module map.
:- import_module require.
:- import_module set.

:- import_module common_types.
:- import_module util.
:- import_module varmap.

%-----------------------------------------------------------------------%

fix_branches(!.Proc) = Result :-
    Stmts0 = !.Proc ^ p_body,
    Varmap0 = !.Proc ^ p_varmap,
    Arity = !.Proc ^ p_arity,
    Context = !.Proc ^ p_context,
    map_foldl(fix_branches_stmt, Stmts0, Stmts1, Varmap0, Varmap),
    ResultStmts = fix_return_stmt(return_info(Context, Arity), Stmts1),
    ( ResultStmts = ok(Stmts),
        !Proc ^ p_body := Stmts,
        !Proc ^ p_varmap := Varmap,
        Result = ok(!.Proc)
    ; ResultStmts = errors(Errors),
        Result = errors(Errors)
    ).

%-----------------------------------------------------------------------%

:- pred fix_branches_stmt(pre_statement::in, pre_statement::out,
    varmap::in, varmap::out) is det.

fix_branches_stmt(pre_statement(Type0, Info0), pre_statement(Type, Info),
        !Varmap) :-
    % Only defined vars that are also non-local can be defined vars.
    (
        ( Type0 = s_call(_)
        ; Type0 = s_assign(_, _)
        ; Type0 = s_return(_)
        ),
        Type = Type0,
        Info = Info0
    ;
        Type0 = s_match(Var, Cases0),

        DefVars = Info0 ^ si_def_vars,
        NonLocals = Info0 ^ si_non_locals,
        UsedDefVars = DefVars `intersect` NonLocals,
        map2_foldl2(fix_branches_case(UsedDefVars), Cases0, Cases,
            CasesReachable, set.init, _, !Varmap),
        Reachable = reachable_branches(CasesReachable),

        Type = s_match(Var, Cases),

        % Fixup variable sets.  These sets are more strict but they also
        % allow us to avoid doing any renaming here, since renaming only
        % occurs for local variables.
        UseVars0 = Info0 ^ si_use_vars,
        UseVars = UseVars0 `intersect` NonLocals,

        Info = (Info0 ^ si_use_vars := UseVars)
                      ^ si_reachable := Reachable
    ).

:- type binds_vars
    --->    binds_vars(set(var))
    ;       not_reached.

:- pred fix_branches_case(set(var)::in,
    pre_case::in, pre_case::out, stmt_reachable::out,
    set(var)::in, set(var)::out, varmap::in, varmap::out) is det.

fix_branches_case(SwitchDefVars, pre_case(Pat, Stmts0),
        pre_case(Pat, Stmts), Reachable, !CasesVars, !Varmap) :-
    map_foldl(fix_branches_stmt, Stmts0, Stmts, !Varmap),

    PatVars = pattern_all_vars(Pat),

    StmtsDefVars = union_list(map((func(S) = S ^ s_info ^ si_def_vars),
        Stmts)),
    Reachable = reachable_sequence(
        map((func(S) = S ^ s_info ^ si_reachable), Stmts)),
    (
        Reachable = stmt_always_returns
    ;
        ( Reachable = stmt_always_fallsthrough
        ; Reachable = stmt_may_return
        ),
        DefVars = StmtsDefVars `union` PatVars,
        ( if not superset(DefVars, SwitchDefVars) then
            ( Stmts0 = [HeadStmt | _],
                Context = HeadStmt ^ s_info ^ si_context
            ; Stmts0 = [],
                unexpected($file, $pred, "Empty case")
            ),
            compile_error($file, $pred, Context,
                "Case does not define all required variables")
        else
            true
        )
    ),

    AllVars = union_list(map(stmt_all_vars, Stmts)),
    !:CasesVars = !.CasesVars `union` AllVars.

:- func reachable_branches(list(stmt_reachable)) = stmt_reachable.

reachable_branches([]) = stmt_always_fallsthrough.
reachable_branches([R | Rs]) =
    foldl(reachable_branches_2, Rs, R).

:- func reachable_branches_2(stmt_reachable, stmt_reachable) =
    stmt_reachable.

reachable_branches_2(stmt_may_return, _) = stmt_may_return.
reachable_branches_2(stmt_always_fallsthrough, stmt_always_fallsthrough) =
    stmt_always_fallsthrough.
reachable_branches_2(stmt_always_fallsthrough, stmt_always_returns) =
    stmt_may_return.
reachable_branches_2(stmt_always_fallsthrough, stmt_may_return) =
    stmt_may_return.
reachable_branches_2(stmt_always_returns, stmt_always_returns) =
    stmt_always_returns.
reachable_branches_2(stmt_always_returns, stmt_always_fallsthrough) =
    stmt_may_return.
reachable_branches_2(stmt_always_returns, stmt_may_return) =
    stmt_may_return.

:- func reachable_sequence(list(stmt_reachable)) = stmt_reachable.

reachable_sequence(Branches) =
    foldl(reachable_sequence_2, Branches, stmt_always_fallsthrough).

:- func reachable_sequence_2(stmt_reachable, stmt_reachable) = stmt_reachable.

reachable_sequence_2(stmt_always_fallsthrough, R) = R.
reachable_sequence_2(stmt_always_returns, _) = stmt_always_returns.
reachable_sequence_2(stmt_may_return, stmt_always_fallsthrough) =
    stmt_may_return.
reachable_sequence_2(stmt_may_return, stmt_always_returns) =
    stmt_always_returns.
reachable_sequence_2(stmt_may_return, stmt_may_return) =
    stmt_may_return.

%-----------------------------------------------------------------------%

:- type return_info
    --->    return_info(
                ri_context      :: context,
                ri_arity        :: arity
            ).

:- func fix_return_stmt(return_info, pre_statements) =
    result(pre_statements, compile_error).

fix_return_stmt(Info, Stmts0) =
    result_map(reverse, fix_return_stmt_rev(Info, reverse(Stmts0))).

:- func fix_return_stmt_rev(return_info, pre_statements) =
    result(pre_statements, compile_error).

fix_return_stmt_rev(Info, []) = check_arity_and_return(Context, Arity) :-
    return_info(Context, Arity) = Info.
fix_return_stmt_rev(Info, [Stmt0 | Stmts0]) = Result :-
    Reachable = Stmt0 ^ s_info ^ si_reachable,
    ( Reachable = stmt_always_returns,
        Result = ok([Stmt0 | Stmts0])
    ; Reachable = stmt_always_fallsthrough,
        Context = Stmt0 ^ s_info ^ si_context,
        Arity = Info ^ ri_arity,
        Result0 = check_arity_and_return(Context, Arity),
        Result = result_map((func(R) = R ++ [Stmt0 | Stmts0]), Result0)
    ; Reachable = stmt_may_return,
        Type = Stmt0 ^ s_type,
        ( Type = s_match(Var, Cases0),
            CasesResult = result_list_to_result(
                map(fix_return_stmt_case(Info), Cases0)),
            Result = result_map(
                (func(Cases) = [Stmt | Stmts0] :-
                    Stmt = Stmt0 ^ s_type := s_match(Var, Cases)
                ),
                CasesResult)
        ;
            ( Type = s_call(_)
            ; Type = s_assign(_, _)
            ; Type = s_return(_)
            ),
            unexpected($file, $pred, "Impercise reachablity")
        )
    ).

:- func fix_return_stmt_case(return_info, pre_case) =
    result(pre_case, compile_error).

fix_return_stmt_case(Info, pre_case(Pat, Stmts0)) = Result :-
    ResultStmts = fix_return_stmt(Info, Stmts0),
    Result = result_map(
        (func(Stmts) =
            pre_case(Pat, Stmts)
        ), ResultStmts).

:- func check_arity_and_return(context, arity) =
    result(pre_statements, compile_error).

check_arity_and_return(Context, Arity) = Result :-
    ( if Arity = arity(0) then
        Result = ok([new_return_statement])
    else
        Result = return_error(Context, ce_no_return_statement(Arity))
    ).

:- func new_return_statement = pre_statement.

new_return_statement = pre_statement(s_return([]), Info) :-
    Info = stmt_info(nil_context, init, init, init, stmt_always_returns).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
