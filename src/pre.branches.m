%-----------------------------------------------------------------------%
% Plasma AST symbol resolution
% vim: ts=4 sw=4 et
%
% Copyright (C) 2016 Plasma Team
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

:- import_module pre.pre_ds.

%-----------------------------------------------------------------------%

:- pred fix_branches(pre_procedure::in, pre_procedure::out) is det.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
:- implementation.

:- import_module map.
:- import_module require.
:- import_module set.

:- import_module util.
:- import_module varmap.

%-----------------------------------------------------------------------%

fix_branches(!Proc) :-
    Stmts0 = !.Proc ^ p_body,
    Varmap0 = !.Proc ^ p_varmap,
    map_foldl(fix_branches_stmt, Stmts0, Stmts, Varmap0, Varmap),
    !Proc ^ p_body := Stmts,
    !Proc ^ p_varmap := Varmap.

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

        DefVars0 = Info0 ^ si_def_vars,
        NonLocals = Info0 ^ si_non_locals,
        UsedDefVars = DefVars0 `intersect` NonLocals,
        map3_foldl2(fix_branches_case(UsedDefVars, NonLocals), Cases0, Cases,
            CasesDefVars, CasesReachable, set.init, _, !Varmap),
        DefVars = binds_vars_intersect(CasesDefVars) `intersect` NonLocals,
        Reachable = reachable_branches(CasesReachable),
        expect(subset(DefVars, DefVars0), $file, $pred,
            "The set of defined variables of a switch cannot grow"),

        Type = s_match(Var, Cases),

        % Fixup variable sets.  These sets are more strict but they also
        % allow us to avoid doing any renaming here, since renaming only
        % occurs for local variables.
        UseVars0 = Info0 ^ si_use_vars,
        UseVars = UseVars0 `intersect` NonLocals,

        Info = ((Info0 ^ si_def_vars := DefVars)
                       ^ si_use_vars := UseVars)
                       ^ si_reachable := Reachable
    ).

:- type binds_vars
    --->    binds_vars(set(var))
    ;       not_reached.

:- pred fix_branches_case(set(var)::in, set(var)::in,
    pre_case::in, pre_case::out, binds_vars::out, stmt_reachable::out,
    set(var)::in, set(var)::out, varmap::in, varmap::out) is det.

fix_branches_case(SwitchDefVars, SwitchNonLocals, pre_case(Pat0, Stmts0),
        pre_case(Pat, Stmts), BindsVars, Reachable, !CasesVars, !Varmap) :-
    map_foldl(fix_branches_stmt, Stmts0, Stmts1, !Varmap),

    ( Pat = p_var(Var),
        PatVars = make_singleton_set(Var)
    ;
        ( Pat = p_number(_)
        ; Pat = p_wildcard
        ; Pat = p_constr(_)
        ),
        PatVars = set.init
    ),

    StmtsDefVars = union_list(map((func(S) = S ^ s_info ^ si_def_vars),
        Stmts1)),
    Reachable = reachable_sequence(
        map((func(S) = S ^ s_info ^ si_reachable), Stmts1)),
    (
        Reachable = stmt_always_returns,
        BindsVars = not_reached
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
        ),
        BindsVars = binds_vars(DefVars)
    ),

    % Rename variables that occur in more than one branch but are local
    % to this case.
    AllVars = union_list(map(stmt_all_vars, Stmts1)),
    Local = AllVars `difference` SwitchNonLocals,
    RenameSet = !.CasesVars `intersect` Local,
    some [!Renaming] (
        !:Renaming = map.init,
        pat_rename(RenameSet, Pat0, Pat, !Renaming, !Varmap),
        map_foldl2(stmt_rename(RenameSet), Stmts1, Stmts, !Renaming, !Varmap),
        _ = !.Renaming
    ),

    % We need to fix the var sets of the switch and any parent statements
    % after renaming?  The simplest solution is to remove any local
    % variables from the use and def sets of the switch or any other
    % compound statment.  We don this in fix_branches_stmt/1.

    !:CasesVars = !.CasesVars `union` AllVars.

:- func binds_vars_intersect(list(binds_vars)) = set(var).

binds_vars_intersect([]) = set.init.
binds_vars_intersect([B | Bs]) = Vars :-
    ( B = binds_vars(Vars0),
        Vars = foldl(binds_vars_intersect_2, Bs, Vars0)
    ; B = not_reached,
        Vars = binds_vars_intersect(Bs)
    ).

:- func binds_vars_intersect_2(binds_vars, set(var)) = set(var).

binds_vars_intersect_2(binds_vars(BranchVars), Vars) =
    intersect(BranchVars, Vars).
binds_vars_intersect_2(not_reached, Vars) = Vars.

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

