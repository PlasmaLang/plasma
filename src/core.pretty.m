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

func_pretty(Core, FuncId) = FuncIdPretty ++ nl ++ FuncPretty ++ nl :-
    core_get_function_det(Core, FuncId, Func),
    FuncId = func_id(FuncIdInt),
    FuncIdPretty = comment_line(0) ++
        singleton(format("func: %d", [i(FuncIdInt)])),
    FuncDecl = func_decl_pretty(Core, Func),
    ( if func_get_body(Func, _, _, _, _) then
        FuncPretty0 = [p_group_curly(FuncDecl, open_curly,
            func_body_pretty(Core, Func), close_curly)]
    else
        FuncPretty0 = FuncDecl ++ [p_cord(singleton(";"))]
    ),
    Opts = options(max_line, default_indent),
    FuncPretty = pretty(Opts, 0, FuncPretty0).

:- func func_decl_pretty(core, function) = list(pretty).

func_decl_pretty(Core, Func) =
        func_decl_or_call_pretty(Core, Func, ParamsPretty) :-
    func_get_type_signature(Func, ParamTypes, _, _),
    ( if func_get_body(Func, Varmap, ParamNames, _Captured, _Expr) then
        ParamsPretty = params_pretty(Core, Varmap, ParamNames, ParamTypes)
    else
        ParamsPretty = map(func(T) = p_cord(type_pretty(Core, T)),
            ParamTypes)
    ).

func_call_pretty(Core, Func, Varmap, Args) =
    % This is likely going to look ugly depending on the caller.
    pretty(Opts, 0, func_call_pretty_new(Core, Func, Varmap, Args)) :-
    Opts = options(max_line, 0).

:- func func_call_pretty_new(core, function, varmap, list(var)) = list(pretty).

func_call_pretty_new(Core, Func, Varmap, Args) =
        func_decl_or_call_pretty(Core, Func, ParamsPretty) :-
    func_get_type_signature(Func, ParamTypes, _, _),
    ParamsPretty = params_pretty(Core, Varmap, Args, ParamTypes).

:- func func_decl_or_call_pretty(core, function, list(pretty)) =
    list(pretty).

func_decl_or_call_pretty(Core, Func, ParamsPretty) =
        [p_cord(singleton("func")), p_spc,
            p_cord(singleton(q_name_to_string(FuncName)))] ++
        pretty_args(ParamsPretty) ++ ReturnsPretty ++ UsesPretty :-
    FuncName = func_get_name(Func),
    func_get_type_signature(Func, _, Returns, _),
    ( Returns = [],
        ReturnsPretty = []
    ; Returns = [_ | _],
        ReturnsPretty = [p_nl_soft, p_cord(singleton("-> "))] ++
            pretty_seperated([p_cord(comma), p_nl_soft],
                map(func(R) = p_cord(type_pretty(Core, R)), Returns))
    ),
    UsesPretty = []. % XXX

:- func params_pretty(core, varmap, list(var), list(type_)) =
    list(pretty).

params_pretty(Core, Varmap, Names, Types) =
    map_corresponding(param_pretty(Core, Varmap), Names, Types).

:- func param_pretty(core, varmap, var, type_) = pretty.

param_pretty(Core, Varmap, Var, Type) =
    p_group([p_cord(var_pretty(Varmap, Var)), p_cord(singleton(" :")),
        p_nl_soft, p_cord(type_pretty(Core, Type))]).

:- func func_body_pretty(core, function) = list(pretty).

func_body_pretty(Core, Func) = Pretty :-
    ( if func_get_body(Func, VarmapPrime, _, CapturedPrime, ExprPrime) then
        Varmap = VarmapPrime,
        Captured = CapturedPrime,
        Expr = ExprPrime
    else
        unexpected($file, $pred, "Abstract function")
    ),

    expr_pretty(Core, Varmap, Expr, ExprPretty, 0, _, map.init, _InfoMap),

    ( Captured = [],
        CapturedPretty = []
    ; Captured = [_ | _],
        CapturedPretty = [p_nl_double,
            p_cord(singleton("// Captured: ") ++
            join(singleton(", "), map(var_pretty(Varmap), Captured)))]
    ),

    ( if func_get_vartypes(Func, VarTypes) then
        VarTypesPretty = [p_nl_double,
            p_cord(singleton("// Types of variables: "))] ++
            pretty_seperated([p_nl_soft], map(var_type_map_pretty(Core, Varmap),
                to_assoc_list(VarTypes)))
    else
        VarTypesPretty = []
    ),

    % _InfoMap could be printed, but we should also print expression numbers
    % if that's the case.

    Pretty = [
            p_cord(singleton("// " ++
                context_string(code_info_context(Expr ^ e_info)))),
            p_nl_hard] ++
        [p_group(ExprPretty)] ++
        CapturedPretty ++ VarTypesPretty.

%-----------------------------------------------------------------------%

:- func var_type_map_pretty(core, varmap, pair(var, type_)) = pretty.

var_type_map_pretty(Core, Varmap, Var - Type) =
        p_cord(VarPretty ++ singleton(": ") ++ TypePretty) :-
    VarPretty = var_pretty(Varmap, Var),
    TypePretty = type_pretty(Core, Type).

%-----------------------------------------------------------------------%

% Expression numbers are currently unused, and no meta information is
% currently printed about expressions.  As we need it we should consider how
% best to do this.  Or we should print information directly within the
% pretty-printed expression.

% Note that expression nubers start at 0 and are allocated to parents before
% children.  This allows us to avoid printing the number of the first child
% of any expression, which makes pretty printed output less cluttered, as
% these numbers would otherwise appear consecutively in many expressions.
% This must be the same throughout the compiler so that anything
% using expression numbers makes sense when looking at pretty printed
% reports.

:- pred expr_pretty(core::in, varmap::in, expr::in, list(pretty)::out,
    int::in, int::out, map(int, code_info)::in, map(int, code_info)::out)
    is det.

expr_pretty(Core, Varmap, Expr, Pretty, !ExprNum, !InfoMap) :-
    Expr = expr(ExprType, CodeInfo),

    MyExprNum = !.ExprNum,
    !:ExprNum = !.ExprNum + 1,

    det_insert(MyExprNum, CodeInfo, !InfoMap),

    ( ExprType = e_tuple(Exprs),
        map_foldl2(expr_pretty(Core, Varmap), Exprs, ExprsPretty,
            !ExprNum, !InfoMap),
        Pretty = pretty_args(map(func(G) = p_group(G), ExprsPretty))
    ; ExprType = e_lets(Lets, In),
        map_foldl2(let_pretty(Core, Varmap), Lets, LetsPretty0,
            !ExprNum, !InfoMap),
        LetsPretty = list_join([p_nl_hard],
            map(func(L) = p_group(L), LetsPretty0)),
        expr_pretty(Core, Varmap, In, InPretty, !ExprNum, !InfoMap),
        Pretty = [p_group([p_cord(let ++ spc), p_tabstop] ++
            LetsPretty ++ [p_nl_hard] ++
            [p_group(InPretty)])]
    ; ExprType = e_call(Callee, Args, _),
        ( Callee = c_plain(FuncId),
            CalleePretty = p_cord(id_pretty(core_lookup_function_name(Core),
                FuncId))
        ; Callee = c_ho(CalleeVar),
            CalleePretty = p_cord(var_pretty(Varmap, CalleeVar))
        ),
        ArgsPretty = pretty_args(map(func(V) = p_cord(var_pretty(Varmap, V)),
            Args)),
        Pretty = [CalleePretty] ++ ArgsPretty
    ; ExprType = e_var(Var),
        Pretty = [p_cord(var_pretty(Varmap, Var))]
    ; ExprType = e_constant(Const),
        Pretty = [p_cord(const_pretty(core_lookup_function_name(Core),
            core_lookup_constructor_name(Core), Const))]
    ; ExprType = e_construction(CtorId, Args),
        PrettyName = p_cord(id_pretty(core_lookup_constructor_name(Core),
            CtorId)),
        Pretty = [PrettyName] ++ pretty_optional_args(
                map(func(V) = p_cord(var_pretty(Varmap, V)), Args))
    ; ExprType = e_closure(FuncId, Args),
        PrettyFunc = id_pretty(core_lookup_function_name(Core), FuncId),
        Pretty = [p_str("closure")] ++
            pretty_args([p_cord(PrettyFunc) |
                map(func(V) = p_cord(var_pretty(Varmap, V)), Args)])
    ; ExprType = e_match(Var, Cases),
        VarPretty = p_cord(var_pretty(Varmap, Var)),
        map_foldl2(case_pretty(Core, Varmap), Cases, CasesPretty,
            !ExprNum, !InfoMap),
        Pretty = [p_group_curly(
            [p_str("match ("), VarPretty, p_str(")")],
            singleton("{"),
            list_join([p_nl_hard], CasesPretty),
            singleton("}"))]
    ).

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
        Pretty = [p_group(VarsPretty)] ++ [p_nl_soft, p_str("="), p_spc] ++
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

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
