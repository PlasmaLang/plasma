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

% Note that expression nubers start at 0 and are allocated to children
% first.  This must be the same throughout the compiler so that anything
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
    expr_pretty(Core, Varmap, Indent, Expr, ExprPretty, !ExprNum),
    expr_sequence_pretty_loop(Core, Varmap, Indent, Exprs, ExprsPretty0,
        !ExprNum),
    ( if ExprsPretty0 = empty then
        ExprsPretty = ExprsPretty0
    else
        ExprsPretty = nl ++ ExprsPretty0
    ),
    Pretty = context_pretty(Indent, code_info_get_context(Expr ^ e_info)) ++
        line(Indent) ++ ExprPretty ++ ExprsPretty.

:- pred expr_pretty(core::in, varmap::in, int::in, expr::in,
    cord(string)::out, int::in, int::out) is det.

expr_pretty(Core, Varmap, Indent, Expr, Pretty, !ExprNum) :-
    Expr = expr(ExprType, _CodeInfo),

    % Types aren't currently printed.
%    MaybeTypes = code_info_get_maybe_types(CodeInfo),
%    ( MaybeTypes = yes(Types),
%        ( Types = [],
%            PrettyTypes0 = singleton("(no types)")
%        ; Types = [_ | _],
%            PrettyTypes0 = join(comma ++ spc, map(type_pretty, Types))
%        ),
%        PrettyTypes = comment_line(Indent) ++ singleton("Types: ") ++
%            PrettyTypes0
%    ; MaybeTypes = no,
%        PrettyTypes = empty
%    ),

    PrettyInfo = singleton(format("#%d ", [i(MyExprNum)])),

    ( ExprType = e_sequence(Exprs),
        expr_sequence_pretty(Core, Varmap, Indent, Exprs, PrettyExpr,
            !ExprNum)
    ; ExprType = e_tuple(Exprs),
        map_foldl(expr_pretty(Core, Varmap, Indent+1), Exprs, ExprsPretty,
            !ExprNum),
        PrettyExpr = open_paren ++ join(comma ++ nl, ExprsPretty) ++
            close_paren
    ; ExprType = e_let(Vars, ExprA, ExprB),
        VarsPretty = cord_list_to_cord(map(var_pretty(Varmap), Vars)),
        expr_pretty(Core, Varmap, Indent, ExprA, ExprAPretty, !ExprNum),
        expr_pretty(Core, Varmap, Indent+2, ExprB, ExprBPretty, !ExprNum),
        PrettyExpr = let ++ spc ++ VarsPretty ++ spc ++
            equals ++ spc ++ ExprAPretty ++ spc ++ in ++
            line(Indent+2) ++ ExprBPretty
    ; ExprType = e_call(Callee, Args),
        expr_pretty(Core, Varmap, Indent, Callee, CalleePretty, !ExprNum),
        map_foldl(expr_pretty(Core, Varmap, Indent+2), Args, ArgsPretty,
            !ExprNum),
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
    MyExprNum = !.ExprNum,
    !:ExprNum = !.ExprNum + 1,

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
