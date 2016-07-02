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
        Expr = expr(ExprType, _),
        ( if ExprType = e_sequence(ExprsPrime) then
            Exprs = ExprsPrime
        else
            Exprs = [Expr]
        ),
        expr_sequence_pretty(Core, Varmap, 0, Exprs, FuncDefn, 0, _)
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

expr_pretty(Core, Varmap, Indent, PrintNextExprNum, Expr, Pretty, !ExprNum,
        !InfoMap) :-
    Expr = expr(ExprType, CodeInfo),

    MyExprNum = !.ExprNum,
    !:ExprNum = !.ExprNum + 1,

    det_insert(MyExprNum, CodeInfo, !InfoMap),

    ( PrintNextExprNum = print_next_expr_num,
        PrettyInfo = singleton(format("#%d ", [i(MyExprNum)]))
    ; PrintNextExprNum = skip_next_expr_num,
        PrettyInfo = empty
    ),

    ( ExprType = e_sequence(Exprs),
        expr_sequence_pretty(Core, Varmap, Indent, Exprs, PrettyExpr,
            !ExprNum)
    ; ExprType = e_tuple(Exprs),
        ( Exprs = [],
            PrettyExpr = open_paren ++ spc ++ close_paren
        ; Exprs = [TExpr | TExprs],
            expr_pretty(Core, Varmap, Indent+1, skip_next_expr_num, TExpr,
                TExprPretty, !ExprNum, !InfoMap),
            map_foldl2(
                expr_pretty(Core, Varmap, Indent+1, print_next_expr_num),
                TExprs, TExprsPretty, !ExprNum, !InfoMap),
            PrettyExpr = open_paren ++ join(comma ++ nl,
                [TExprPretty | TExprsPretty]) ++ close_paren
        )
    ; ExprType = e_let(Vars, ExprA, ExprB),
        VarsPretty = cord_list_to_cord(map(var_pretty(Varmap), Vars)),
        expr_pretty(Core, Varmap, Indent, skip_next_expr_num, ExprA,
            ExprAPretty, !ExprNum, !InfoMap),
        expr_pretty(Core, Varmap, Indent+2, print_next_expr_num, ExprB,
            ExprBPretty, !ExprNum, !InfoMap),
        PrettyExpr = let ++ spc ++ VarsPretty ++ spc ++
            equals ++ spc ++ ExprAPretty ++ spc ++ in ++
            line(Indent+2) ++ ExprBPretty
    ; ExprType = e_call(Callee, Args),
        expr_pretty(Core, Varmap, Indent, skip_next_expr_num, Callee,
            CalleePretty, !ExprNum, !InfoMap),
        map_foldl2(expr_pretty(Core, Varmap, Indent+2, print_next_expr_num),
                Args, ArgsPretty, !ExprNum, !InfoMap),
        PrettyExpr = CalleePretty ++ singleton("(") ++
            join(singleton(", "), ArgsPretty) ++ singleton(")")
    ; ExprType = e_var(Var),
        PrettyExpr = var_pretty(Varmap, Var)
    ; ExprType = e_const(Const),
        ( Const = c_number(Int),
            PrettyExpr = singleton(string(Int))
        ; Const = c_string(String),
            PrettyExpr = singleton(escape_string(String))
        )
    ; ExprType = e_func(FuncId),
        PrettyExpr = func_name_pretty(Core, FuncId)
    ),

    Pretty = PrettyInfo ++ PrettyExpr.

:- func func_name_pretty(core, func_id) = cord(string).

func_name_pretty(Core, FuncId) = singleton(String) :-
    core_lookup_function_name(Core, FuncId, Name),
    String = q_name_to_string(Name).

%-----------------------------------------------------------------------%

:- func context_pretty(int, context) = cord(string).

context_pretty(Indent, Context) =
    comment_line(Indent) ++ singleton(context_string(Context)).

:- func var_pretty(varmap, var) = cord(string).

var_pretty(Varmap, Var) = singleton(get_var_name(Varmap, Var)).

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

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
