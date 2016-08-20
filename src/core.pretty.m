%-----------------------------------------------------------------------%
% Plasma code pretty printer
% vim: ts=4 sw=4 et
%
% Copyright (C) 2016 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
%-----------------------------------------------------------------------%
:- module core.pretty.
%-----------------------------------------------------------------------%

:- interface.

:- import_module cord.
:- import_module string.

:- func core_pretty(core) = cord(string).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

:- import_module pretty_utils.
:- import_module string_utils.
:- import_module varmap.

%-----------------------------------------------------------------------%

core_pretty(Core) = ModuleDecl ++ cord_list_to_cord(Funcs) :-
    ModuleDecl = singleton(format("module %s\n\n",
        [s(q_name_to_string(module_name(Core)))])),
    Funcs = map(func_pretty(Core), core_all_functions(Core)).

:- func func_pretty(core, func_id) = cord(string).

func_pretty(Core, FuncId) = FuncDecl ++ FuncDefn ++ nl :-
    core_get_function_det(Core, FuncId, Func),

    FuncDecl = from_list(["func ", q_name_to_string(FuncName), "("]) ++
        ParamsPretty ++ singleton(")") ++ ReturnsPretty ++
        UsingPretty,
    core_lookup_function_name(Core, FuncId, FuncName),
    func_get_signature(Func, ParamTypes, Returns, _),
    ParamsPretty = join(singleton(", "), ParamsPretty0),
    ( Returns = [],
        ReturnsPretty = empty
    ; Returns = [_ | _],
        ReturnsPretty = singleton(" -> ") ++
            join(singleton(", "),
                map(type_pretty, Returns))
    ),
    UsingPretty = empty, % XXX

    ( if func_get_body(Func, Varmap, ParamNames, Expr) then
        ParamsPretty0 =
            map_corresponding(param_pretty(Varmap), ParamNames, ParamTypes),
        expr_sequence_pretty(Core, Varmap, 0, [Expr], DefnExprSeq, 0, _),
        FuncDefn = spc ++ DefnExprSeq
    else
        ParamsPretty0 = map(type_pretty, ParamTypes),
        FuncDefn = singleton(";\n")
    ).

:- func param_pretty(varmap, var, type_) = cord(string).

param_pretty(Varmap, Var, Type) = var_pretty(Varmap, Var) ++ singleton(" : ") ++
    type_pretty(Type).

%-----------------------------------------------------------------------%

% Note that expression nubers start at 0 and are allocated to parents before
% children.  This allows us to avoid printing the number of the first child
% of any expression, which makes pretty printed output less cluttered, as
% these numbers would otherwise appear consecutively in many expressions.
% This must be the same throughout the compiler so that anything
% using expression numbers makes sense when looking at pretty printed
% reports.

:- pred expr_sequence_pretty(core::in, varmap::in, int::in, list(expr)::in,
    cord(string)::out, int::in, int::out) is det.

expr_sequence_pretty(Core, Varmap, Indent, Exprs, Pretty, !ExprNum) :-
    expr_sequence_pretty_loop(Core, Varmap, Indent+2, Exprs, ExprsPretty,
        !ExprNum),
    Pretty = open_curly ++
        ExprsPretty ++
        line(Indent) ++ close_curly.

:- pred expr_sequence_pretty_loop(core::in, varmap::in, int::in,
    list(expr)::in, cord(string)::out, int::in, int::out) is det.

expr_sequence_pretty_loop(_, _, _, [], empty, !ExprNum).
expr_sequence_pretty_loop(Core, Varmap, Indent, [Expr | Exprs], Pretty,
        !ExprNum) :-
    expr_pretty(Core, Varmap, Indent, print_next_expr_num, Expr, ExprPretty,
        !ExprNum, map.init, InfoMap),

    foldl(type_map_pretty(Indent), InfoMap, [], TypesPretty0),
    ( TypesPretty0 = [],
        TypesPretty = empty
    ; TypesPretty0 = [_ | _],
        TypesPretty = line(Indent) ++ singleton("// Types: ") ++
            cord_list_to_cord(reverse(TypesPretty0))
    ),

    expr_sequence_pretty_loop(Core, Varmap, Indent, Exprs, ExprsPretty0,
        !ExprNum),
    ( if ExprsPretty0 = empty then
        ExprsPretty = ExprsPretty0
    else
        ExprsPretty = nl ++ ExprsPretty0
    ),
    Pretty = context_pretty(Indent, code_info_get_context(Expr ^ e_info)) ++
        line(Indent) ++ ExprPretty ++ TypesPretty ++ ExprsPretty.

:- pred type_map_pretty(int::in, int::in, code_info::in,
    list(cord(string))::in, list(cord(string))::out) is det.

type_map_pretty(Indent, ExprNum, CodeInfo, !List) :-
    MaybeTypes = code_info_get_maybe_types(CodeInfo),
    ( MaybeTypes = yes(Types),
        ( Types = [],
            PrettyTypes0 = singleton("(no types)")
        ; Types = [_ | _],
            PrettyTypes0 = join(comma ++ spc, map(type_pretty, Types))
        ),
        PrettyTypes = comment_line(Indent) ++
            singleton(format("  #%d ", [i(ExprNum)])) ++
            PrettyTypes0,
        !:List = [PrettyTypes | !.List]
    ; MaybeTypes = no
    ).

:- type print_next_expr_num
    --->    print_next_expr_num
    ;       skip_next_expr_num.

:- pred expr_pretty(core::in, varmap::in, int::in, print_next_expr_num::in,
    expr::in, cord(string)::out, int::in, int::out,
    map(int, code_info)::in, map(int, code_info)::out) is det.

expr_pretty(Core, Varmap, IndentWithoutExprNum, PrintNextExprNum, Expr, Pretty, !ExprNum,
        !InfoMap) :-
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
        CalleePretty = func_name_pretty(core_lookup_function_name(Core),
            Callee),
        ArgsPretty = map(var_pretty(Varmap), Args),
        PrettyExpr = CalleePretty ++ singleton("(") ++
            join(singleton(", "), ArgsPretty) ++ singleton(")")
    ; ExprType = e_var(Var),
        PrettyExpr = var_pretty(Varmap, Var)
    ; ExprType = e_const(Const),
        PrettyExpr = const_pretty(core_lookup_function_name(Core), Const)
    ),

    Pretty = PrettyInfo ++ PrettyExpr.

%-----------------------------------------------------------------------%

:- func type_pretty(type_) = cord(string).

type_pretty(builtin_type(Builtin)) = singleton(Name) :-
    builtin_type_name(Builtin, Name).
type_pretty(type_variable(Var)) = singleton(Var).
type_pretty(type_(Name, Args)) =
    from_list([q_name_to_string(Name), "("]) ++
        join(singleton(", "), map(type_pretty, Args)) ++
        singleton(")").

%-----------------------------------------------------------------------%

:- func let = cord(string).
let = singleton("let").

:- func in = cord(string).
in = singleton("in").

:- func unit = int.
unit = 2.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
