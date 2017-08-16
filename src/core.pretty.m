%-----------------------------------------------------------------------%
% Plasma code pretty printer
% vim: ts=4 sw=4 et
%
% Copyright (C) 2016-2017 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
%-----------------------------------------------------------------------%
:- module core.pretty.
%-----------------------------------------------------------------------%

:- interface.

:- import_module cord.
:- import_module string.

:- func core_pretty(core) = cord(string).

:- func func_call_pretty(core, function, varmap, list(var)) = cord(string).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

:- import_module pair.

:- import_module pretty_utils.
:- import_module string_utils.
:- import_module util.
:- import_module varmap.

%-----------------------------------------------------------------------%

core_pretty(Core) = ModuleDecl ++ cord_list_to_cord(Funcs) :-
    ModuleDecl = singleton(format("module %s\n\n",
        [s(q_name_to_string(module_name(Core)))])),
    Funcs = map(func_pretty(Core), core_all_functions(Core)).

:- func func_pretty(core, func_id) = cord(string).

func_pretty(Core, FuncId) = FuncDecl ++ FuncDefn ++ nl :-
    core_get_function_det(Core, FuncId, Func),
    FuncDecl = func_decl_pretty(Core, Func),
    ( if func_get_body(Func, _, _, _) then
        FuncDefn = spc ++ func_body_pretty(Core, 0, Func)
    else
        FuncDefn = singleton(";\n")
    ).

:- func func_decl_pretty(core, function) = cord(string).

func_decl_pretty(Core, Func) =
        func_decl_or_call_pretty(Core, Func, ParamsPretty) :-
    func_get_signature(Func, ParamTypes, _, _),
    ( if func_get_body(Func, Varmap, ParamNames, _Expr) then
        ParamsPretty = params_pretty(Core, Varmap, ParamNames, ParamTypes)
    else
        ParamsPretty = map(type_pretty(Core), ParamTypes)
    ).

func_call_pretty(Core, Func, Varmap, Args) =
        func_decl_or_call_pretty(Core, Func, ParamsPretty) :-
    func_get_signature(Func, ParamTypes, _, _),
    ParamsPretty = params_pretty(Core, Varmap, Args, ParamTypes).

:- func func_decl_or_call_pretty(core, function, list(cord(string))) =
    cord(string).

func_decl_or_call_pretty(Core, Func, ParamsPretty0) =
        from_list(["func ", q_name_to_string(FuncName), "("]) ++
            ParamsPretty ++ singleton(")") ++ ReturnsPretty ++
            UsesPretty :-
    FuncName = func_get_name(Func),
    func_get_signature(Func, _, Returns, _),
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
    ( if func_get_body(Func, VarmapPrime, _, ExprPrime) then
        Varmap = VarmapPrime,
        Expr = ExprPrime
    else
        unexpected($file, $pred, "Abstract function")
    ),

    expr_pretty(Core, Varmap, Indent+unit, print_next_expr_num, Expr,
        ExprPretty, 0, _, map.init, InfoMap),

    ( if func_get_vartypes(Func, VarTypes) then
        VarTypesPretty = nl ++ line(Indent + unit) ++
            singleton("// Types of variables: ") ++ line(Indent + unit) ++
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
        context_pretty(Indent, code_info_get_context(Expr ^ e_info)) ++
            line(Indent) ++ ExprPretty ++ VarTypesPretty ++ ExprTypesPretty ++
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
    MaybeTypes = code_info_get_maybe_types(CodeInfo),
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

:- pred expr_pretty(core::in, varmap::in, int::in, print_next_expr_num::in,
    expr::in, cord(string)::out, int::in, int::out,
    map(int, code_info)::in, map(int, code_info)::out) is det.

expr_pretty(Core, Varmap, IndentWithoutExprNum, PrintNextExprNum, Expr,
        Pretty, !ExprNum, !InfoMap) :-
    Expr = expr(ExprType, CodeInfo),

    MyExprNum = !.ExprNum,
    !:ExprNum = !.ExprNum + 1,

    det_insert(MyExprNum, CodeInfo, !InfoMap),

    ( PrintNextExprNum = print_next_expr_num,
        PrettyInfoStr = format("#%d ", [i(MyExprNum)]),
        PrettyInfo = singleton(PrettyInfoStr),
        Indent = IndentWithoutExprNum + length(PrettyInfoStr)
    ; PrintNextExprNum = skip_next_expr_num,
        PrettyInfo = empty,
        Indent = IndentWithoutExprNum
    ),

    ( ExprType = e_tuple(Exprs),
        ( Exprs = [],
            PrettyExpr = open_paren ++ spc ++ close_paren
        ; Exprs = [TExpr | TExprs],
            expr_pretty(Core, Varmap, Indent+unit, skip_next_expr_num, TExpr,
                TExprPretty, !ExprNum, !InfoMap),
            map_foldl2(
                expr_pretty(Core, Varmap, Indent+unit, print_next_expr_num),
                TExprs, TExprsPretty, !ExprNum, !InfoMap),
            PrettyExpr = open_paren ++ spc ++ join(line(Indent) ++ comma ++ spc,
                [TExprPretty | TExprsPretty]) ++ line(Indent) ++ close_paren
        )
    ; ExprType = e_let(Vars, ExprA, ExprB),
        VarsPretty = join(singleton(", "), map(var_pretty(Varmap), Vars)),
        expr_pretty(Core, Varmap, Indent+unit, skip_next_expr_num, ExprA,
            ExprAPretty, !ExprNum, !InfoMap),
        expr_pretty(Core, Varmap, Indent+unit, print_next_expr_num, ExprB,
            ExprBPretty, !ExprNum, !InfoMap),
        PrettyExpr = let ++ spc ++ VarsPretty ++ spc ++ equals ++
            line(Indent+unit) ++ ExprAPretty ++
            line(Indent) ++ in ++
            line(Indent+unit) ++ ExprBPretty
    ; ExprType = e_call(Callee, Args),
        CalleePretty = id_pretty(core_lookup_function_name(Core),
            Callee),
        ArgsPretty = map(var_pretty(Varmap), Args),
        PrettyExpr = CalleePretty ++ singleton("(") ++
            join(singleton(", "), ArgsPretty) ++ singleton(")")
    ; ExprType = e_var(Var),
        PrettyExpr = var_pretty(Varmap, Var)
    ; ExprType = e_constant(Const),
        PrettyExpr = const_pretty(core_lookup_function_name(Core),
            core_lookup_constructor_name(Core), Const)
    ; ExprType = e_construction(CtorId, Args),
        PrettyName = id_pretty(core_lookup_constructor_name(Core), CtorId),
        PrettyArgs = pretty_optional_args(var_pretty(Varmap), Args),
        PrettyExpr = PrettyName ++ PrettyArgs
    ; ExprType = e_match(Var, Cases),
        map_foldl2(case_pretty(Core, Varmap, Indent + unit),
            Cases, CasesPretty, !ExprNum, !InfoMap),
        PrettyExpr = singleton("match (") ++ var_pretty(Varmap, Var) ++
                singleton(") {") ++
            cord_list_to_cord(CasesPretty) ++
            line(Indent) ++ singleton("}")
    ),

    Pretty = PrettyInfo ++ PrettyExpr.

:- pred case_pretty(core::in, varmap::in, int::in,
    expr_case::in, cord(string)::out, int::in, int::out,
    map(int, code_info)::in, map(int, code_info)::out) is det.

case_pretty(Core, Varmap, Indent, e_case(Pattern, Expr), Pretty, !ExprNum,
        !InfoMap) :-
    PatternPretty = pattern_pretty(Core, Varmap, Pattern),
    expr_pretty(Core, Varmap, Indent+unit, print_next_expr_num, Expr,
        ExprPretty, !ExprNum, !InfoMap),
    Pretty = line(Indent) ++ singleton("case ") ++ PatternPretty ++
        singleton(" -> ") ++ ExprPretty.

:- func pattern_pretty(core, varmap, expr_pattern) = cord(string).

pattern_pretty(_,    _,      p_num(Num)) = singleton(string(Num)).
pattern_pretty(_,    Varmap, p_variable(Var)) = var_pretty(Varmap, Var).
pattern_pretty(_,    _,      p_wildcard) = singleton("_").
pattern_pretty(Core, Varmap, p_ctor(CtorId, Args)) =
        NamePretty ++ ArgsPretty :-
    NamePretty = id_pretty(core_lookup_constructor_name(Core), CtorId),
    ArgsPretty = pretty_optional_args(var_pretty(Varmap), Args).

%-----------------------------------------------------------------------%

:- func type_pretty(core, type_) = cord(string).

type_pretty(_, builtin_type(Builtin)) = singleton(Name) :-
    builtin_type_name(Builtin, Name).
type_pretty(_, type_variable(Var)) = singleton(Var).
type_pretty(Core, type_ref(TypeId, Args)) = NamePretty ++ ArgsPretty :-
    NamePretty = id_pretty(core_lookup_type_name(Core), TypeId),
    ArgsPretty = pretty_optional_args(type_pretty(Core), Args).

%-----------------------------------------------------------------------%

:- func let = cord(string).
let = singleton("let").

:- func in = cord(string).
in = singleton("in").

:- func unit = int.
unit = 2.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
