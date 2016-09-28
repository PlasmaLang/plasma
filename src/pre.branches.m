%-----------------------------------------------------------------------%
% Plasma AST symbol resolution
% vim: ts=4 sw=4 et
%
% Copyright (C) 2016 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
% This module fixes variable usage in branching code.  It:
%   * fixes var-def sets
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
        map2_foldl2(fix_branches_case(UsedDefVars, NonLocals), Cases0, Cases,
            CasesDefVars, set.init, _, !Varmap),
        DefVars = intersect_list(CasesDefVars) `intersect` NonLocals,
        expect(subset(DefVars, DefVars0), $file, $pred,
            "The set of defined variables of a switch cannot grow"),

        Type = s_match(Var, Cases),

        % Fixup variable sets.  These sets are more strict but they also
        % allow us to avoid doing any renaming here, since renaming only
        % occurs for local variables.
        UseVars0 = Info0 ^ si_use_vars,
        UseVars = UseVars0 `intersect` NonLocals,
        Info1 = Info0 ^ si_def_vars := DefVars,
        Info = Info1 ^ si_use_vars := UseVars
    ).

:- pred fix_branches_case(set(var)::in, set(var)::in,
    pre_case::in, pre_case::out, set(var)::out, set(var)::in, set(var)::out,
    varmap::in, varmap::out) is det.

fix_branches_case(SwitchDefVars, SwitchNonLocals, pre_case(Pat0, Stmts0),
        pre_case(Pat, Stmts), DefVars, !CasesVars, !Varmap) :-
    map_foldl(fix_branches_stmt, Stmts0, Stmts1, !Varmap),

    ( Pat = p_var(Var),
        PatVars = make_singleton_set(Var)
    ;
        ( Pat = p_number(_)
        ; Pat = p_wildcard
        ),
        PatVars = set.init
    ),

    StmtsDefVars = union_list(map((func(S) = S ^ s_info ^ si_def_vars),
        Stmts1)),
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

