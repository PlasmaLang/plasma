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
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module string.

:- import_module pretty_utils.
:- import_module util.

%-----------------------------------------------------------------------%

pre_pretty(Core, Map) = pretty(default_options, 0, Pretty) :-
    Pretty = [p_list(list_join([p_nl_hard],
        map(proc_pretty(Core), to_assoc_list(Map))))].

:- type pretty_info
    --->    pretty_info(
                pi_varmap       :: varmap,
                pi_core         :: core
            ).

:- func proc_pretty(core, pair(func_id, pre_procedure)) = pretty.

proc_pretty(Core, FuncId - Proc) =
        procish_pretty(Info, FuncId, ParamVars, yes(init), Body) :-
    ParamVars = Proc ^ p_param_vars,
    Body = Proc ^ p_body,
    Varmap = Proc ^ p_varmap,
    Info = pretty_info(Varmap, Core).

:- func procish_pretty(pretty_info, func_id, list(var_or_wildcard(var)),
    maybe(set(var)), pre_statements) = pretty.

procish_pretty(Info, FuncId, ParamVars, MaybeCaptured, Body) =
        p_group_curly(
            [id_pretty(core_lookup_function_name(Core), FuncId), p_str("(")] ++
                pretty_seperated([p_str(", "), p_nl_soft],
                                 map(var_or_wild_pretty(Varmap), ParamVars)) ++
                [p_str(")")] ++ CapturedPretty,
            singleton("{"),
            stmts_pretty(Info, Body),
            singleton("}")) :-
    pretty_info(Varmap, Core) = Info,
    ( if
        MaybeCaptured = yes(Captured),
        not is_empty(Captured)
    then
        CapturedPretty = [p_nl_hard, p_str("// Captured: ")] ++
            pretty_seperated([p_str(", ")],
                map(var_pretty(Varmap), to_sorted_list(Captured)))
    else
        CapturedPretty = []
    ).

:- func stmts_pretty(pretty_info, pre_statements) = list(pretty).

stmts_pretty(Info, Stmts) =
    condense(list_join([[p_nl_double]], map(stmt_pretty(Info), Stmts))).

:- func stmt_pretty(pretty_info, pre_statement) = list(pretty).

stmt_pretty(Info, pre_statement(Type, StmtInfo)) =
        PrettyInfo1 ++ [p_nl_hard, PrettyStmt] ++
        PrettyInfo2 :-
    Varmap = Info ^ pi_varmap,

    StmtInfo = stmt_info(Context, UseVars, DefVars, StmtReturns),
    PrettyInfo1 = [p_comment(singleton("// "),
        [p_str(context_string(Context)), p_nl_hard,
         p_str("Use vars: "), vars_pretty(Varmap, UseVars)])],
    PrettyInfo2 = [p_comment(singleton("// "),
        [p_str("Def vars: "), vars_pretty(Varmap, DefVars), p_nl_hard,
         p_str("Reachable: "), p_str(string(StmtReturns))])],

    ( Type = s_call(Call),
        PrettyStmt = call_pretty(Info, Call)
    ; Type = s_decl_vars(Vars),
        PrettyStmt = p_expr([p_str("var ")] ++
            pretty_seperated([p_str(", "), p_nl_soft],
                map(var_pretty(Varmap), Vars)))
    ; Type = s_assign(Vars, Expr),
        PrettyStmt = p_expr(pretty_seperated([p_str(", "), p_nl_soft],
                map(var_or_wild_pretty(Varmap), Vars)) ++
            [p_spc, p_nl_soft, p_str("= "), expr_pretty(Info, Expr)])
    ; Type = s_return(Var),
        PrettyStmt = p_expr([p_str("return ")] ++
            pretty_seperated([p_str(", "), p_nl_soft],
                map(var_pretty(Varmap), Var)))
    ; Type = s_match(Var, Cases),
        PrettyStmt = p_group_curly(
            [p_str("match ("), var_pretty(Varmap, Var), p_str(")")],
            singleton("{"),
            list_join([p_nl_hard], map(case_pretty(Info), Cases)),
            singleton("}"))
    ).

:- func case_pretty(pretty_info, pre_case) = pretty.

case_pretty(Info, pre_case(Pattern, Stmts)) = p_group_curly(
    [p_str("case "), pattern_pretty(Info, Pattern), p_str(" ->")],
    singleton("{"),
    stmts_pretty(Info, Stmts),
    singleton("}")).

:- func pattern_pretty(pretty_info, pre_pattern) = pretty.

pattern_pretty(_, p_number(Num)) = p_str(string(Num)).
pattern_pretty(Info, p_var(Var)) = var_pretty(Info ^ pi_varmap, Var).
pattern_pretty(_, p_wildcard) = p_str("_").
pattern_pretty(Info, p_constr(CtorId, Args)) =
        pretty_optional_args(IdPretty, ArgsPretty) :-
    IdPretty = id_pretty(core_lookup_constructor_name(Info ^ pi_core),
        CtorId),
    ArgsPretty = map(pattern_pretty(Info), Args).

:- func expr_pretty(pretty_info, pre_expr) = pretty.

expr_pretty(Info, e_call(Call)) = call_pretty(Info, Call).
expr_pretty(Info, e_var(Var)) = var_pretty(Info ^ pi_varmap, Var).
expr_pretty(Info, e_construction(CtorId, Args)) =
        pretty_optional_args(IdPretty, ArgsPretty) :-
    IdPretty = id_pretty(core_lookup_constructor_name(Info ^ pi_core),
        CtorId),
    ArgsPretty = map(expr_pretty(Info), Args).
expr_pretty(Info,
        e_lambda(pre_lambda(FuncId, Params, MaybeCaptured, _, Body))) =
    procish_pretty(Info, FuncId, Params, MaybeCaptured, Body).
expr_pretty(Info, e_constant(Const)) =
    const_pretty(core_lookup_function_name(Info ^ pi_core),
        core_lookup_constructor_name(Info ^ pi_core), Const).

:- func call_pretty(pretty_info, pre_call) = pretty.

call_pretty(Info, Call) = Pretty :-
    ( Call = pre_call(FuncId, Args, WithBang),
        Lookup = core_lookup_function_name(Info ^ pi_core),
        CalleePretty = id_pretty(Lookup, FuncId)
    ; Call = pre_ho_call(Callee, Args, WithBang),
        CalleePretty = expr_pretty(Info, Callee)
    ),
    ( WithBang = with_bang,
        BangPretty = [p_str("!")]
    ; WithBang = without_bang,
        BangPretty = []
    ),
    Pretty = pretty_callish(p_expr([CalleePretty] ++ BangPretty),
            map(expr_pretty(Info), Args)).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
