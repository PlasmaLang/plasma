%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module pre.ast_to_core.
%
% Copyright (C) 2015-2016 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
% Plasma parse tree to core representation conversion
%
%-----------------------------------------------------------------------%

:- interface.

:- import_module ast.
:- import_module compile_error.
:- import_module core.
:- import_module result.

%-----------------------------------------------------------------------%

:- pred ast_to_core(ast::in, result(core, compile_error)::out) is det.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module cord.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module require.
:- import_module set.
:- import_module string.

:- import_module pre.env.
:- import_module pre.from_ast.
:- import_module pre.nonlocals.
:- import_module builtins.
:- import_module context.
:- import_module common_types.
:- import_module core.code.
:- import_module core.types.
:- import_module q_name.
:- import_module result.
:- import_module varmap.

%-----------------------------------------------------------------------%

ast_to_core(ast(ModuleName, Entries), Result) :-
    Exports = gather_exports(Entries),
    some [!Core, !Errors] (
        !:Core = core.init(q_name(ModuleName)),
        !:Errors = init,

        setup_builtins(BuiltinMap, !Core),
        map.foldl(env_add_func, BuiltinMap, env.init, Env0),
        env_import_star(builtin_module_name, Env0, Env1),

        foldl3(gather_funcs, Entries, !Core, Env1, Env, !Errors),
        ( if is_empty(!.Errors) then
            foldl2(build_function(Exports, Env), Entries, !Core, !Errors),
            ( if is_empty(!.Errors) then
                Result = ok(!.Core)
            else
                Result = errors(!.Errors)
            )
        else
            Result = errors(!.Errors)
        )
    ).

%-----------------------------------------------------------------------%

:- type exports
    --->    exports(set(string))
    ;       export_all.

:- func gather_exports(list(ast_entry)) = exports.

gather_exports(Entries) = Exports :-
    ( if member(ast_export(export_all), Entries) then
        Exports = export_all
    else
        filter_map(
            (pred(Entry::in, Export::out) is semidet :-
                Entry = ast_export(export_some(List)),
                Export = set(List)
            ), Entries, Sets),
        Exports = exports(union_list(Sets))
    ).

%-----------------------------------------------------------------------%

:- pred gather_funcs(ast_entry::in, core::in, core::out, env::in, env::out,
    errors(compile_error)::in, errors(compile_error)::out) is det.

gather_funcs(ast_export(_), !Core, !Env, !Errors).
gather_funcs(ast_import(_, _), !Core, !Env, !Errors).
gather_funcs(ast_type(_, _, _, _), !Core, !Env, !Errors).
gather_funcs(ast_function(Name, _, _, _, _, Context), !Core, !Env,
        !Errors) :-
    QName = q_name_snoc(module_name(!.Core), Name),
    ( if
        core_register_function(QName, FuncId, !Core)
    then
        % Add the function to the environment with it's local name, since
        % we're in the scope of the module already.
        env_add_func(q_name(Name), FuncId, !Env)
    else
        add_error(Context, ce_function_already_defined(Name), !Errors)
    ).

%-----------------------------------------------------------------------%

:- pred build_function(exports::in, env::in, ast_entry::in,
    core::in, core::out,
    errors(compile_error)::in, errors(compile_error)::out) is det.

build_function(_, _, ast_export(_), !Core, !Errors).
build_function(_, _, ast_import(_, _), !Core, !Errors).
build_function(_, _, ast_type(_, _, _, _), !Core, !Errors).
build_function(Exports, Env0, ast_function(Name, Params, Return, Using0,
        Body0, Context), !Core, !Errors) :-
    ModuleName = module_name(!.Core),
    det_core_lookup_function(!.Core, q_name_snoc(ModuleName, Name), FuncId),

    % Build basic information about the function.
    Sharing = sharing(Exports, Name),
    ParamTypesResult = result_list_to_result(map(build_param_type, Params)),
    ReturnTypeResult = build_type(Return),
    foldl2(build_using, Using0, set.init, Using, set.init, Observing),
    IntersectUsingObserving = intersect(Using, Observing),
    ( if
        ParamTypesResult = ok(ParamTypes),
        ReturnTypeResult = ok(ReturnType),
        is_empty(IntersectUsingObserving)
    then
        Function0 = func_init(Context, Sharing, ParamTypes, [ReturnType],
            Using, Observing),

        % Build body.
        ParamNames = map((func(ast_param(N, _)) = N), Params),
        some [!Varmap] (
            !:Varmap = varmap.init,
            % XXX: parameters must be named appart.
            map_foldl2(env_add_var, ParamNames, ParamVars, Env0, Env,
                !Varmap),
            build_body(Env, Context, ParamVars, Body0, Body, !Varmap),
            func_set_body(!.Varmap, ParamVars, Body, Function0, Function)
        ),
        core_set_function(FuncId, Function, !Core)
    else
        ( if ParamTypesResult = errors(ParamTypesErrors) then
            !:Errors = ParamTypesErrors ++ !.Errors
        else
            true
        ),
        ( if ReturnTypeResult = errors(ReturnTypeErrors) then
            !:Errors = ReturnTypeErrors ++ !.Errors
        else
            true
        ),
        ( if not is_empty(IntersectUsingObserving) then
            add_error(Context,
                ce_using_observing_not_distinct(IntersectUsingObserving),
                !Errors)
        else
            true
        )
    ).

:- func sharing(exports, string) = sharing.

sharing(export_all, _) = s_public.
sharing(exports(Exports), Name) =
    ( if member(Name, Exports) then
        s_public
    else
        s_private
    ).

:- func build_param_type(ast_param) = result(type_, compile_error).

build_param_type(ast_param(_, Type)) = build_type(Type).

:- func build_type(ast_type_expr) = result(type_, compile_error).

build_type(ast_type(Qualifiers, Name, Args0, Context)) = Result :-
    ( if
        Qualifiers = [],
        builtin_type_name(Type, Name)
    then
        ( Args0 = [],
            Result = ok(builtin_type(Type))
        ; Args0 = [_ | _],
            Result = return_error(Context, ce_builtin_type_with_args(Name))
        )
    else
        ArgsResult = result_list_to_result(map(build_type, Args0)),
        ( ArgsResult = ok(Args),
            Result = ok(type_(q_name(Qualifiers, Name), Args))
        ; ArgsResult = errors(Error),
            Result = errors(Error)
        )
    ).
build_type(ast_type_var(Name, _Context)) = Result :-
    Result = ok(type_variable(Name)).

:- pred build_using(ast_using::in,
    set(resource)::in, set(resource)::out,
    set(resource)::in, set(resource)::out) is det.

build_using(ast_using(Type, ResourceName), !Using, !Observing) :-
    ( if ResourceName = "IO" then
        Resource = r_io,
        ( Type = ut_using,
            !:Using = set.insert(!.Using, Resource)
        ; Type = ut_observing,
            !:Observing = set.insert(!.Observing, Resource)
        )
    else
        sorry($file, $pred, "Only IO resource is supported")
    ).

%-----------------------------------------------------------------------%

:- pred build_body(env::in, context::in, list(var)::in,
    list(ast_statement)::in, expr::out, varmap::in, varmap::out) is det.

build_body(Env, Context, ParamVars, !.Statements, Expr, !Varmap) :-
    % Steps 1-4 transform the statements to get them into a form
    % that's easy to create the core representation from (steps 5 & 6).

    % 1. Resolve symbols, build a varmap, build var use sets and
    %    over-conservative var-def sets.
    resolve_symbols_stmts(!Statements, _, _, Env, _, !Varmap),

    % 2. Determine nonlocals
    compute_nonlocals_stmts(set(ParamVars), !Statements),

    % 3. TODO: Name appart vars on different branches, except where they are
    %    nonlocals, fixup var-def sets.

    % 4. Remove return statements.
    remove_returns(!Statements, Returns, map.init, _, !Varmap),
    CI = code_info_init(Context),
    ( Returns = returns(ReturnVars),
        ( if ReturnVars = [ReturnVar] then
            ReturnExpr = expr(e_var(ReturnVar), CI)
        else
            ReturnExpr = expr(e_tuple(
                map((func(V) = expr(e_var(V), CI)),
                    ReturnVars)),
                CI)
        )
    ; Returns = no_returns,
        ReturnExpr = expr(e_tuple([]), CI)
    ),

    % 5. TODO: Turn branch statements into branch expressions (this can
    %    probably be done as part of the next step.
    % 6. Transform the whole structure into an expression tree.
    build_statements(ReturnExpr, !.Statements, Expr, !Varmap).

%-----------------------------------------------------------------------%

    % XXX: Instead generate assignments and return a set of variables.
    %
:- type maybe_returns
    --->    returns(
                list(var)
            )
    ;       no_returns.

    % remove_returns(!Stmts, MaybeReturns, !ReturnVars, !Varmap)
    %
    % Remove return statements from !Stmts replacing them with assignments
    % to new variables returned in MaybeReturns.  The new variables are
    % generated using !ReturnVars (which helps us use the same variables on
    % different branches) and !Varmap.
    %
:- pred remove_returns(list(ast_statement(stmt_info_varsets))::in,
    list(ast_statement(stmt_info_varsets))::out, maybe_returns::out,
    map(int, var)::in, map(int, var)::out, varmap::in, varmap::out) is det.

remove_returns([], [], no_returns, !ReturnVars, !Varmap).
remove_returns([Stmt0 | Stmts0], Stmts, Returns, !ReturnVars, !Varmap) :-
    Stmt0 = ast_statement(StmtType0, Info),
    (
        ( StmtType0 = s_call(_)
        ; StmtType0 = s_asign_statement(_, _, _)
        ; StmtType0 = s_array_set_statement(_, _, _)
        ),
        remove_returns(Stmts0, Stmts1, Returns, !ReturnVars, !Varmap),
        Stmts = [Stmt0 | Stmts1]
    ;
        StmtType0 = s_return_statement(Exprs),
        ( Stmts0 = [],
            NumReturns = length(Exprs),
            get_or_make_return_vars(NumReturns, Vars, VarNames, !ReturnVars,
                !Varmap),
            StmtType = s_asign_statement(VarNames, yes(Vars), Exprs),
            Stmt = ast_statement(StmtType, Info),
            Stmts = [Stmt],
            Returns = returns(Vars)
        ; Stmts0 = [_ | _],
            unexpected($file, $pred,
                "compile error: dead code after return")
        )
    ;
        StmtType0 = s_match_statement(MatchExpr, Cases0),
        map2_foldl2(remove_returns_case, Cases0, Cases1, MaybeReturnss,
            !ReturnVars, !Varmap),
        ( if
            all [R] (
                member(R, MaybeReturnss),
                R = no_returns
            )
            % Also implying that MaybeReturnss could be empty.
        then
            % If there is no return expresson on any branch then don't
            % transform the code.
            remove_returns(Stmts0, Stmts1, Returns, !ReturnVars, !Varmap),
            Stmts = [Stmt0 | Stmts1]
        else if
            % If all branches contain a return statement then there most be
            % no code after this match.
            all [R] (
                member(R, MaybeReturnss),
                R = returns(_)
            )
        then
            ( if remove_adjacent_dups(MaybeReturnss) = [returns(Vars)] then
                Returns = returns(Vars)
            else
                unexpected($file, $pred,
                    "Return statements with mismatched arities")
            ),
            ( Stmts0 = [],
                StmtType = s_match_statement(MatchExpr, Cases1),
                Stmt = ast_statement(StmtType, Info),
                Stmts = [Stmt]
            ; Stmts0 = [_ | _],
                unexpected($file, $pred,
                    "compile error: dead code after return")
            )
        else
            unexpected($file, $pred, "TODO")
            % XXX: In the common case push the code after the switch into the
            % branches that don't have return statements, then process it like
            % case 2.
            % XXX: I don't like this idea as it duplicates code, by creating
            % a closure we can avoid this, and maybe inline it later to
            % avoid overhead (at the cost of duplication).
        )
    ).

:- pred remove_returns_case(ast_match_case(stmt_info_varsets)::in,
    ast_match_case(stmt_info_varsets)::out,
    maybe_returns::out, map(int, var)::in, map(int, var)::out,
    varmap::in, varmap::out) is det.

remove_returns_case(ast_match_case(Pat, Stmts0),
        ast_match_case(Pat, Stmts), Returns, !ReturnVars, !Varset) :-
    remove_returns(Stmts0, Stmts, Returns, !ReturnVars, !Varset).

:- pred get_or_make_return_vars(int::in, list(var)::out, list(string)::out,
    map(int, var)::in, map(int, var)::out, varmap::in, varmap::out) is det.

get_or_make_return_vars(Num, Vars, Names, !ReturnVars, !Varmap) :-
    ( if Num > 0 then
        get_or_make_return_vars(Num - 1, Vars0, Names0, !ReturnVars,
            !Varmap),
        ( if search(!.ReturnVars, Num, VarPrime) then
            Var = VarPrime
        else
            add_anon_var(Var, !Varmap)
        ),
        Vars = [Var | Vars0],
        Name = get_var_name(!.Varmap, Var),
        Names = [Name | Names0]
    else
        Vars = [],
        Names = []
    ).

%-----------------------------------------------------------------------%

:- pred build_statements(expr::in,
    list(ast_statement(stmt_info_varsets))::in, expr::out,
    varmap::in, varmap::out) is det.

build_statements(Expr, [], Expr, !Varmap).
build_statements(ResultExpr, [Stmt | Stmts], Expr, !Varmap) :-
    Stmt = ast_statement(StmtType, Info),
    Context = Info ^ siv_context,
    ( StmtType = s_call(Call),
        build_call(Context, Call, expr(CallType, CallInfo), !Varmap),
        CallExpr = expr(CallType, CallInfo),
        build_statements(ResultExpr, Stmts, StmtsExpr, !Varmap),
        Expr = expr(e_let([], CallExpr, StmtsExpr), code_info_init(Context))
    ; StmtType = s_asign_statement(_, MaybeVars, ASTExprs),
        map_foldl(build_expr(Context), ASTExprs, Exprs, !Varmap),
        ( if Exprs = [TupleP] then
            Tuple = TupleP
        else
            Tuple = expr(e_tuple(Exprs), code_info_init(Context))
        ),
        ( MaybeVars = yes(Vars)
        ; MaybeVars = no,
            unexpected($file, $pred, "Unresolved variables in assignment")
        ),
        Expr = expr(e_let(Vars, Tuple, StmtsExpr), code_info_init(Context)),
        build_statements(ResultExpr, Stmts, StmtsExpr, !Varmap)
    ; StmtType = s_array_set_statement(_, _, _),
        sorry($file, $pred, "Array assignment")
    ; StmtType = s_return_statement(_),
        unexpected($file, $pred, "Return statement")
    ; StmtType = s_match_statement(_, _),
        sorry($file, $pred, "match")
    ).

:- pred build_expr(context::in, ast_expression::in, expr::out,
    varmap::in, varmap::out) is det.

build_expr(Context, e_call(Call), Expr, !Varmap) :-
    build_call(Context, Call, Expr, !Varmap).
build_expr(_, e_symbol(Name), _, !Varmap) :-
    unexpected($file, $pred,
        format("Unresolved symbol %s", [s(q_name_to_string(Name))])).
build_expr(Context, e_var(Var),
        expr(e_var(Var), code_info_init(Context)), !Varmap).
build_expr(Context, e_func(FuncId),
        expr(e_func(FuncId), code_info_init(Context)), !Varmap).
build_expr(Context, e_const(Const),
        expr(e_const(Value), code_info_init(Context)), !Varmap) :-
    ( Const = c_string(String),
        Value = c_string(String)
    ; Const = c_number(Number),
        Value = c_number(Number)
    ; Const = c_list_nil,
        sorry($file, $pred, "list")
    ).
build_expr(_, e_u_op(_, _), _, !Varmap) :-
    unexpected($file, $pred, "Unresolved unary operator").
build_expr(_, e_b_op(_, _, _), _, !Varmap) :-
    unexpected($file, $pred, "Unresolved binary operator").
build_expr(_, e_array(_), _, !Varmap) :-
    sorry($file, $pred, "Array").

:- pred build_call(context::in, ast_call::in, expr::out,
    varmap::in, varmap::out) is det.

build_call(Context, Call, Expr, !Varmap) :-
    CodeInfo0 = code_info_init(Context),
    ( Call = ast_call(Callee0, Args0),
        CodeInfo = CodeInfo0
    ; Call = ast_bang_call(Callee0, Args0),
        code_info_set_using_marker(has_using_marker, CodeInfo0, CodeInfo)
    ),
    build_expr(Context, Callee0, Callee, !Varmap),
    map_foldl(build_expr(Context), Args0, ArgExprs, !Varmap),
    make_arg_vars(length(Args0), Args, !Varmap),
    Expr = expr(e_let(Args,
            expr(e_tuple(ArgExprs), code_info_init(Context)),
            expr(e_call(Callee, Args), CodeInfo)),
        code_info_init(Context)).

:- pred make_arg_vars(int::in, list(var)::out, varmap::in, varmap::out)
    is det.

make_arg_vars(Num, Vars, !Varmap) :-
    ( if Num = 0 then
        Vars = []
    else
        make_arg_vars(Num - 1, Vars0, !Varmap),
        add_anon_var(Var, !Varmap),
        Vars = [Var | Vars0]
    ).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
