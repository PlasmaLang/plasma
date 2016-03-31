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
:- import_module varmap.

%-----------------------------------------------------------------------%

core_pretty(Core) = ModuleDecl ++ cord_list_to_cord(Funcs) :-
    ModuleDecl = singleton(format("module %s\n\n",
        [s(symbol_to_string(module_name(Core)))])),
    Funcs = map(func_pretty(Core), core_all_functions(Core)).

:- func func_pretty(core, func_id) = cord(string).

func_pretty(Core, FuncId) = FuncDecl ++ FuncDefn ++ nl :-
    core_get_function_det(Core, FuncId, Func),

    FuncDecl = from_list(["func ", symbol_to_string(FuncName), "("]) ++
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
        ( if Expr = expr(e_sequence(_), _) then
            expr_pretty(Core, Varmap, 0, Expr, FuncDefn, 0, _)
        else
            expr_pretty(Core, Varmap, 1, Expr, BodyPretty, 0, _),
            FuncDefn = singleton("\n{\n") ++ BodyPretty ++
                singleton("\n}\n")
        )
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

:- pred expr_pretty(core::in, varmap::in, int::in, expr::in,
    cord(string)::out, int::in, int::out) is det.

expr_pretty(Core, Varmap, Indent, Expr, Pretty, !ExprNum) :-
    Expr = expr(ExprType, CodeInfo),

    MaybeTypes = code_info_get_maybe_types(CodeInfo),
    ( MaybeTypes = yes(Types),
        ( Types = [],
            PrettyTypes0 = singleton("(no types)")
        ; Types = [_ | _],
            PrettyTypes0 = join(comma ++ spc, map(type_pretty, Types))
        ),
        PrettyTypes = comment_line(Indent) ++ singleton("Types: ") ++
            PrettyTypes0
    ; MaybeTypes = no,
        PrettyTypes = empty
    ),

    PrettyInfo = comment_line(Indent) ++
        singleton(format("Expr # %d", [i(MyExprNum)])) ++
        comment_line(Indent) ++
        singleton(context_string(code_info_get_context(CodeInfo))) ++
        PrettyTypes,

    ( ExprType = e_sequence(Exprs),
        map_foldl(expr_pretty(Core, Varmap, Indent+1), Exprs, ExprsPretty,
            !ExprNum),
        PrettyExpr = line(Indent) ++ open_curly ++
            join(nl, ExprsPretty) ++
            line(Indent) ++ close_curly
    ; ExprType = e_call(FuncId, Args),
        map_foldl(expr_pretty(Core, Varmap, Indent+1), Args, ArgsPretty,
            !ExprNum),
        PrettyExpr = line(Indent) ++ func_name_pretty(Core, FuncId) ++
            singleton("(") ++
            join(nl, ArgsPretty) ++
            line(Indent) ++ singleton(")")
    ; ExprType = e_var(Var),
        PrettyExpr = line(Indent) ++ var_pretty(Varmap, Var)
    ; ExprType = e_const(Const),
        ( Const = c_number(Int),
            PrettyExpr = line(Indent) ++ singleton(string(Int))
        ; Const = c_string(String),
            PrettyExpr = line(Indent) ++
                singleton(format("\"%s\"", [s(String)]))
        )
    ; ExprType = e_func(FuncId),
        PrettyExpr = line(Indent) ++ func_name_pretty(Core, FuncId)
    ),
    MyExprNum = !.ExprNum,
    !:ExprNum = !.ExprNum + 1,

    Pretty = PrettyInfo ++ PrettyExpr.

:- func func_name_pretty(core, func_id) = cord(string).

func_name_pretty(Core, FuncId) = singleton(String) :-
    core_lookup_function_name(Core, FuncId, Symbol),
    String = symbol_to_string(Symbol).

%-----------------------------------------------------------------------%

:- func var_pretty(varmap, var) = cord(string).

var_pretty(Varmap, Var) = singleton(get_var_name(Varmap, Var)).

:- func type_pretty(type_) = cord(string).

type_pretty(builtin_type(Builtin)) = builtin_type_pretty(Builtin).
type_pretty(type_variable(Var)) = singleton(Var).
type_pretty(type_(Symbol, Args)) =
    from_list([symbol_to_string(Symbol), "("]) ++
        join(singleton(", "), map(type_pretty, Args)) ++
        singleton(")").

:- func builtin_type_pretty(builtin_type) = cord(string).

builtin_type_pretty(int) = singleton("Int").
builtin_type_pretty(string) = singleton("String").

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
