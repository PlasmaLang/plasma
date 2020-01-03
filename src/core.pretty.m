%-----------------------------------------------------------------------%
% Plasma code pretty printer
% vim: ts=4 sw=4 et
%
% Copyright (C) 2016-2020 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
%-----------------------------------------------------------------------%
:- module core.pretty.
%-----------------------------------------------------------------------%

:- interface.

:- import_module cord.
:- import_module string.

:- func core_pretty(core) = cord(string).

:- func type_pretty(core, type_) = cord(string).

:- func func_call_pretty(core, function, varmap, list(var)) = cord(string).

    % Print the argument parts of a function type.  You can either put
    % "func" in front of this or the name of the variable at a call site.
    %
:- func type_pretty_func(core, list(type_), list(type_), set(resource_id),
    set(resource_id)) = cord(string).

:- func resource_pretty(core, resource_id) = cord(string).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

:- import_module pair.

:- import_module pretty_utils.
:- import_module pretty_utils2.
:- import_module string_utils.
:- import_module util.
:- import_module varmap.

%-----------------------------------------------------------------------%

core_pretty(Core) = ModuleDecl ++ cord_list_to_cord(Funcs) :-
    ModuleDecl = singleton(format("module %s\n",
        [s(q_name_to_string(module_name(Core)))])),
    Funcs = map(func_pretty(Core), core_all_functions(Core)).

:- func func_pretty(core, func_id) = cord(string).

func_pretty(Core, FuncId) = FuncIdPretty ++ FuncDecl ++ FuncDefn ++ nl :-
    core_get_function_det(Core, FuncId, Func),
    FuncId = func_id(FuncIdInt),
    FuncIdPretty = comment_line(0) ++
        singleton(format("func: %d", [i(FuncIdInt)])),
    FuncDecl = line(0) ++ func_decl_pretty(Core, Func),
    ( if func_get_body(Func, _, _, _, _) then
        FuncDefn = spc ++ func_body_pretty(Core, 0, Func)
    else
        FuncDefn = singleton(";")
    ).

:- func func_decl_pretty(core, function) = cord(string).

func_decl_pretty(Core, Func) =
        func_decl_or_call_pretty(Core, Func, ParamsPretty) :-
    func_get_type_signature(Func, ParamTypes, _, _),
    ( if func_get_body(Func, Varmap, ParamNames, _Captured, _Expr) then
        ParamsPretty = params_pretty(Core, Varmap, ParamNames, ParamTypes)
    else
        ParamsPretty = map(type_pretty(Core), ParamTypes)
    ).

func_call_pretty(Core, Func, Varmap, Args) =
        func_decl_or_call_pretty(Core, Func, ParamsPretty) :-
    func_get_type_signature(Func, ParamTypes, _, _),
    ParamsPretty = params_pretty(Core, Varmap, Args, ParamTypes).

:- func func_decl_or_call_pretty(core, function, list(cord(string))) =
    cord(string).

func_decl_or_call_pretty(Core, Func, ParamsPretty0) =
        from_list(["func ", q_name_to_string(FuncName), "("]) ++
            ParamsPretty ++ singleton(")") ++ ReturnsPretty ++
            UsesPretty :-
    FuncName = func_get_name(Func),
    func_get_type_signature(Func, _, Returns, _),
    ParamsPretty = join(singleton(", "), ParamsPretty0),
    ( Returns = [],
        ReturnsPretty = empty
    ; Returns = [_ | _],
        ReturnsPretty = singleton(" -> ") ++
            join(singleton(", "),
                map(type_pretty(Core), Returns))
    ),
    UsesPretty = empty. % XXX

:- func params_pretty(core, varmap, list(var), list(type_)) =
    list(cord(string)).

params_pretty(Core, Varmap, Names, Types) =
    map_corresponding(param_pretty(Core, Varmap), Names, Types).

:- func param_pretty(core, varmap, var, type_) = cord(string).

param_pretty(Core, Varmap, Var, Type) =
    var_pretty(Varmap, Var) ++ singleton(" : ") ++ type_pretty(Core, Type).

:- func func_body_pretty(core, int, function) = cord(string).

func_body_pretty(Core, Indent, Func) = Pretty :-
    ( if func_get_body(Func, VarmapPrime, _, CapturedPrime, ExprPrime) then
        Varmap = VarmapPrime,
        Captured = CapturedPrime,
        Expr = ExprPrime
    else
        unexpected($file, $pred, "Abstract function")
    ),

    expr_pretty(Core, Varmap, Expr, ExprPretty0, 0, _, map.init, InfoMap),
    ExprPretty = line(Indent+1) ++ pretty(Indent+1, ExprPretty0),

    ( Captured = [],
        CapturedPretty = empty
    ; Captured = [_ | _],
        CapturedPretty = nl ++ line(Indent + unit) ++
            singleton("// Captured: ") ++
            join(singleton(", "), map(var_pretty(Varmap), Captured))
    ),

    ( if func_get_vartypes(Func, VarTypes) then
        VarTypesPretty = nl ++ line(Indent + unit) ++
            singleton("// Types of variables: ") ++
            line(Indent + unit) ++ singleton("//   ") ++
            join(line(Indent + unit) ++ singleton("//   "),
                map(var_type_map_pretty(Core, Varmap),
                    to_assoc_list(VarTypes)))
    else
        VarTypesPretty = empty
    ),

    foldl(expr_type_map_pretty(Core, Indent + unit), InfoMap,
        [], ExprTypesPretty0),
    ( ExprTypesPretty0 = [],
        ExprTypesPretty = empty
    ; ExprTypesPretty0 = [_ | _],
        ExprTypesPretty = nl ++ line(Indent + unit) ++
            singleton("// Types of expressions: ") ++
            cord_list_to_cord(reverse(ExprTypesPretty0))
    ),

    Pretty = open_curly ++
        context_pretty(Indent+1, code_info_context(Expr ^ e_info)) ++
            ExprPretty ++
            CapturedPretty ++ VarTypesPretty ++ ExprTypesPretty ++
        line(Indent) ++ close_curly.

%-----------------------------------------------------------------------%

:- func var_type_map_pretty(core, varmap, pair(var, type_)) = cord(string).

var_type_map_pretty(Core, Varmap, Var - Type) =
        VarPretty ++ singleton(": ") ++ TypePretty :-
    VarPretty = var_pretty(Varmap, Var),
    TypePretty = type_pretty(Core, Type).

:- pred expr_type_map_pretty(core::in, int::in, int::in, code_info::in,
    list(cord(string))::in, list(cord(string))::out) is det.

expr_type_map_pretty(Core, Indent, ExprNum, CodeInfo, !List) :-
    MaybeTypes = code_info_maybe_types(CodeInfo),
    ( MaybeTypes = yes(Types),
        ( Types = [],
            PrettyTypes0 = singleton("(no types)")
        ; Types = [_ | _],
            PrettyTypes0 = join(comma ++ spc, map(type_pretty(Core), Types))
        ),
        PrettyTypes = comment_line(Indent) ++
            singleton(format("  #%d ", [i(ExprNum)])) ++
            PrettyTypes0,
        !:List = [PrettyTypes | !.List]
    ; MaybeTypes = no
    ).

%-----------------------------------------------------------------------%

% Note that expression nubers start at 0 and are allocated to parents before
% children.  This allows us to avoid printing the number of the first child
% of any expression, which makes pretty printed output less cluttered, as
% these numbers would otherwise appear consecutively in many expressions.
% This must be the same throughout the compiler so that anything
% using expression numbers makes sense when looking at pretty printed
% reports.

:- type print_next_expr_num
    --->    print_next_expr_num
    ;       skip_next_expr_num.

:- pred expr_pretty(core::in, varmap::in, expr::in, list(pretty)::out,
    int::in, int::out, map(int, code_info)::in, map(int, code_info)::out)
    is det.

expr_pretty(Core, Varmap, Expr, Pretty, !ExprNum, !InfoMap) :-
    Expr = expr(ExprType, CodeInfo),

    MyExprNum = !.ExprNum,
    !:ExprNum = !.ExprNum + 1,

    det_insert(MyExprNum, CodeInfo, !InfoMap),

    % XXX
%     ( PrintNextExprNum = print_next_expr_num,
%         PrettyInfoStr = format("#%d ", [i(MyExprNum)]),
%         PrettyInfo = singleton(PrettyInfoStr),
%         Indent = IndentWithoutExprNum + length(PrettyInfoStr)
%     ; PrintNextExprNum = skip_next_expr_num,
%         PrettyInfo = empty,
%         Indent = IndentWithoutExprNum
%     ),

    ( ExprType = e_tuple(Exprs),
        ( Exprs = [],
            PrettyExpr = [p_group([p_cord(open_paren ++ spc ++
                close_paren)])]
        ; Exprs = [TExpr | TExprs],
            expr_pretty(Core, Varmap, TExpr, TExprPretty, !ExprNum, !InfoMap),
            map_foldl2(expr_pretty(Core, Varmap), TExprs, TExprsPretty,
                !ExprNum, !InfoMap),
            PrettyExpr = p_parens(
                [p_cord(open_paren)], [],
                [p_cord(close_paren)], [],
                [p_cord(comma), p_spc, p_nl_soft],
                TExprPretty ++ map(func(G) = p_group(G), TExprsPretty))
        )
    ; ExprType = e_lets(Lets, In),
        map_foldl2(let_pretty(Core, Varmap), Lets, LetsPretty0,
            !ExprNum, !InfoMap),
        LetsPretty = condense(list_join([[p_nl_hard]], LetsPretty0)),
        expr_pretty(Core, Varmap, In, InPretty, !ExprNum, !InfoMap),
        PrettyExpr = [p_group([p_cord(let ++ spc), p_tabstop] ++
            LetsPretty ++ [p_nl_hard] ++
            [p_group(InPretty)])]
    ; ExprType = e_call(Callee, Args, _),
        ( Callee = c_plain(FuncId),
            CalleePretty = p_cord(id_pretty(core_lookup_function_name(Core),
                FuncId))
        ; Callee = c_ho(CalleeVar),
            CalleePretty = p_cord(var_pretty(Varmap, CalleeVar))
        ),
        ArgsPretty = map(func(V) = p_cord(var_pretty(Varmap, V)), Args),
        PrettyExpr = p_parens(
            [CalleePretty, p_str("(")], [],
            [p_str(")")], [],
            [p_str(","), p_nl_soft],
            ArgsPretty)
    ; ExprType = e_var(Var),
        PrettyExpr = [p_cord(var_pretty(Varmap, Var))]
    ; ExprType = e_constant(Const),
        PrettyExpr = [p_cord(const_pretty(core_lookup_function_name(Core),
            core_lookup_constructor_name(Core), Const))]
    ; ExprType = e_construction(CtorId, Args),
        PrettyName = p_cord(id_pretty(core_lookup_constructor_name(Core),
            CtorId)),
        ( Args = [],
            PrettyExpr = [PrettyName]
        ; Args = [_ | _],
            PrettyExpr = p_parens(
                [PrettyName, p_str("(")], [],
                [p_str(")")], [],
                [p_str(","), p_nl_soft],
                map(func(V) = p_cord(var_pretty(Varmap, V)), Args))
        )
    ; ExprType = e_closure(FuncId, Args),
        PrettyFunc = id_pretty(core_lookup_function_name(Core), FuncId),
        PrettyExpr = p_parens(
            [p_str("closure(")], [],
            [p_str(")")], [],
            [p_str(","), p_nl_soft],
            [p_cord(PrettyFunc) |
                map(func(V) = p_cord(var_pretty(Varmap, V)), Args)])
    ; ExprType = e_match(Var, Cases),
        VarPretty = p_cord(var_pretty(Varmap, Var)),
        map_foldl2(case_pretty(Core, Varmap), Cases, CasesPretty,
            !ExprNum, !InfoMap),
        PrettyExpr = p_parens(
            [p_str("match ("), VarPretty, p_str(") {")], [p_nl_hard],
            [p_nl_hard, p_str("}")], [],
            [p_nl_hard],
            CasesPretty)
    ),

    Pretty = PrettyExpr.

:- pred let_pretty(core::in, varmap::in, expr_let::in, list(pretty)::out,
    int::in, int::out, map(int, code_info)::in, map(int, code_info)::out)
    is det.

let_pretty(Core, Varmap, e_let(Vars, Let), Pretty,
        !ExprNum, !InfoMap) :-
    expr_pretty(Core, Varmap, Let, LetPretty, !ExprNum, !InfoMap),
    ( Vars = [],
        Pretty = [p_str("="), p_spc] ++ [p_group(LetPretty)]
    ; Vars = [_ | _],
        VarsPretty = list_join([p_str(","), p_nl_soft],
            map(func(V) = p_cord(var_pretty(Varmap, V)), Vars)),
        Pretty = [p_group(VarsPretty)] ++ [p_spc, p_str("="), p_nl_soft] ++
            [p_group(LetPretty)]
    ).

:- pred case_pretty(core::in, varmap::in,
    expr_case::in, pretty::out, int::in, int::out,
    map(int, code_info)::in, map(int, code_info)::out) is det.

case_pretty(Core, Varmap, e_case(Pattern, Expr), Pretty, !ExprNum,
        !InfoMap) :-
    PatternPretty = p_cord(pattern_pretty(Core, Varmap, Pattern)),
    expr_pretty(Core, Varmap, Expr, ExprPretty, !ExprNum, !InfoMap),
    Pretty = p_group([p_str("case "), PatternPretty, p_str(" ->"),
        p_nl_soft] ++ ExprPretty).

:- func pattern_pretty(core, varmap, expr_pattern) = cord(string).

pattern_pretty(_,    _,      p_num(Num)) = singleton(string(Num)).
pattern_pretty(_,    Varmap, p_variable(Var)) = var_pretty(Varmap, Var).
pattern_pretty(_,    _,      p_wildcard) = singleton("_").
pattern_pretty(Core, Varmap, p_ctor(CtorId, Args)) =
        NamePretty ++ ArgsPretty :-
    NamePretty = id_pretty(core_lookup_constructor_name(Core), CtorId),
    ArgsPretty = pretty_optional_args(var_pretty(Varmap), Args).

%-----------------------------------------------------------------------%

type_pretty(_, builtin_type(Builtin)) = singleton(Name) :-
    builtin_type_name(Builtin, Name).
type_pretty(_, type_variable(Var)) = singleton(Var).
type_pretty(Core, type_ref(TypeId, Args)) = NamePretty ++ ArgsPretty :-
    NamePretty = id_pretty(core_lookup_type_name(Core), TypeId),
    ArgsPretty = pretty_optional_args(type_pretty(Core), Args).
type_pretty(Core, func_type(Args, Returns, Uses, Observes)) =
        singleton("func") ++ type_pretty_func(Core, Args, Returns, Uses,
            Observes).

type_pretty_func(Core, Args, Returns, Uses, Observes) =
        singleton("(") ++ ArgsPretty ++ singleton(")") ++ UsesPretty ++
        ObservesPretty ++ ReturnsPretty :-
    ArgsPretty = pretty_seperated(comma_spc, type_pretty(Core), Args),
    UsesPretty = maybe_pretty_args_maybe_prefix(singleton(" uses "),
        resource_pretty(Core), set.to_sorted_list(Uses)),
    ObservesPretty = maybe_pretty_args_maybe_prefix(singleton(" uses "),
        resource_pretty(Core), set.to_sorted_list(Observes)),
    ReturnsPretty = maybe_pretty_args_maybe_prefix(singleton(" -> "),
        type_pretty(Core), Returns).

%-----------------------------------------------------------------------%

resource_pretty(Core, ResId) =
    singleton(resource_to_string(core_get_resource(Core, ResId))).

%-----------------------------------------------------------------------%

:- func let = cord(string).
let = singleton("let").

:- func unit = int.
unit = 2.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
