%-----------------------------------------------------------------------%
% Plasma pre-core pretty printer
% vim: ts=4 sw=4 et
%
% Copyright (C) 2016-2017, 2019 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
% This module defines a pretty printer for the pre-core representation.
%
%-----------------------------------------------------------------------%
:- module pre.pretty.
%-----------------------------------------------------------------------%

:- interface.

:- import_module cord.
:- import_module map.

% Used to lookup function names, we could decouple this better.
:- import_module core.

:- import_module common_types.
:- import_module pre.pre_ds.

:- func pre_pretty(core, map(func_id, pre_procedure)) = cord(string).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
:- implementation.

:- import_module int.
:- import_module pair.
:- import_module require.
:- import_module string.

:- import_module pretty_utils.
:- import_module util.

%-----------------------------------------------------------------------%

pre_pretty(Core, Map) = join(nl, map(proc_pretty(Core), to_assoc_list(Map))).

:- type pretty_info
    --->    pretty_info(
                pi_varmap       :: varmap,
                pi_core         :: core
            ).

:- func proc_pretty(core, pair(func_id, pre_procedure)) = cord(string).

proc_pretty(Core, FuncId - Proc) =
        procish_pretty(Info, 0, FuncId, ParamVars, Body) :-
    ParamVars = Proc ^ p_param_vars,
    Body = Proc ^ p_body,
    Varmap = Proc ^ p_varmap,
    Info = pretty_info(Varmap, Core).

:- func procish_pretty(pretty_info, int, func_id, list(var_or_wildcard(var)),
    pre_statements) = cord(string).

procish_pretty(Info, Indent, FuncId, ParamVars, Body) =
        id_pretty(core_lookup_function_name(Core), FuncId) ++
        open_paren ++
        join(comma ++ spc, map(var_or_wild_pretty(Varmap), ParamVars)) ++
        close_paren ++ spc ++ open_curly ++
        stmts_pretty(Info, Indent + unit, Body) ++
        line(Indent) ++ close_curly ++ line(Indent) :-
    pretty_info(Varmap, Core) = Info.

:- func stmts_pretty(pretty_info, int, pre_statements) = cord(string).

stmts_pretty(Info, Indent, Stmts) =
    join(nl, map(stmt_pretty(Info, Indent), Stmts)).

:- func stmt_pretty(pretty_info, int, pre_statement) = cord(string).

stmt_pretty(Info, Indent, pre_statement(Type, StmtInfo)) =
        PrettyInfo1 ++ PrettyStmt ++ PrettyInfo2 :-
    Varmap = Info ^ pi_varmap,

    StmtInfo = stmt_info(Context, UseVars, DefVars, Nonlocals, StmtReturns),
    PrettyInfo1 = context_pretty(Indent, Context) ++
        comment_line(Indent) ++ singleton("Use vars: ") ++
            vars_pretty(Varmap, UseVars) ++
        comment_line(Indent) ++ singleton("Nonlocals: ") ++
            vars_pretty(Varmap, Nonlocals),
    PrettyInfo2 =
        comment_line(Indent) ++ singleton("Def vars: ") ++
            vars_pretty(Varmap, DefVars) ++
        comment_line(Indent) ++ singleton("Reachable: ") ++
            singleton(string(StmtReturns)),

    ( Type = s_call(Call),
        PrettyStmt = line(Indent) ++ call_pretty(Info, Indent, Call)
    ; Type = s_assign(Vars, Expr),
        PrettyStmt = line(Indent) ++
            pretty_seperated(comma_spc, var_or_wild_pretty(Varmap), Vars) ++
            singleton(" = ") ++ expr_pretty(Info, Indent, Expr)
    ; Type = s_return(Var),
        PrettyStmt = line(Indent) ++ return ++ spc ++
            pretty_seperated(comma_spc, var_pretty(Varmap), Var)
    ; Type = s_match(Var, Cases),
        PrettyStmt = line(Indent) ++ match ++
                open_paren ++ var_pretty(Varmap, Var) ++ close_paren ++
            line(Indent) ++ open_curly ++
                cord_list_to_cord(map(case_pretty(Info, Indent + unit),
                    Cases)) ++
            line(Indent) ++ close_curly
    ).

:- func case_pretty(pretty_info, int, pre_case) = cord(string).

case_pretty(Info, Indent, pre_case(Pattern, Stmts)) =
    line(Indent) ++ case ++ spc ++
            pattern_pretty(Info, Pattern) ++ spc ++ r_arrow ++
            spc ++ open_curly ++
        stmts_pretty(Info, Indent + unit, Stmts) ++
        line(Indent) ++ close_curly.

:- func pattern_pretty(pretty_info, pre_pattern) = cord(string).

pattern_pretty(_, p_number(Num)) = singleton(string(Num)).
pattern_pretty(Info, p_var(Var)) = var_pretty(Info ^ pi_varmap, Var).
pattern_pretty(_, p_wildcard) = singleton("_").
pattern_pretty(Info, p_constr(CtorId, Args)) = IdPretty ++ ArgsPretty :-
    IdPretty = id_pretty(core_lookup_constructor_name(Info ^ pi_core),
        CtorId),
    ArgsPretty = pretty_optional_args(pattern_pretty(Info), Args).

:- func expr_pretty(pretty_info, int, pre_expr) = cord(string).

expr_pretty(Info, Indent, e_call(Call)) = call_pretty(Info, Indent, Call).
expr_pretty(Info, _, e_var(Var)) = var_pretty(Info ^ pi_varmap, Var).
expr_pretty(Info, Indent, e_construction(CtorId, Args)) =
        IdPretty ++ ArgsPretty :-
    IdPretty = id_pretty(core_lookup_constructor_name(Info ^ pi_core),
        CtorId),
    ArgsPretty = pretty_optional_args(expr_pretty(Info, Indent), Args).
expr_pretty(Info, Indent, e_lambda(FuncId, Params, _, Body)) =
    procish_pretty(Info, Indent, FuncId, Params, Body).
expr_pretty(Info, _, e_constant(Const)) =
    const_pretty(core_lookup_function_name(Info ^ pi_core),
        core_lookup_constructor_name(Info ^ pi_core), Const).

:- func call_pretty(pretty_info, int, pre_call) = cord(string).

call_pretty(Info, Indent, Call) =
        CalleePretty ++ BangPretty ++ open_paren ++
        join(comma ++ spc, map(expr_pretty(Info, Indent), Args)) ++
        close_paren :-
    ( Call = pre_call(FuncId, Args, WithBang),
        Lookup = core_lookup_function_name(Info ^ pi_core),
        CalleePretty = id_pretty(Lookup, FuncId)
    ; Call = pre_ho_call(Callee, Args, WithBang),
        CalleePretty = expr_pretty(Info, Indent, Callee)
    ),
    ( WithBang = with_bang,
        BangPretty = bang
    ; WithBang = without_bang,
        BangPretty = init
    ).

%-----------------------------------------------------------------------%

:- func case = cord(string).
case = singleton("case").

:- func match = cord(string).
match = singleton("match").

:- func return = cord(string).
return = singleton("return").

:- func r_arrow = cord(string).
r_arrow = singleton("->").

:- func unit = int.
unit = 2.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
