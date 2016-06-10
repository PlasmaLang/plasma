%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module ast_to_core.
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

:- pred ast_to_core(plasma_ast::in, result(core, compile_error)::out) is det.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module cord.
:- import_module list.
:- import_module require.
:- import_module set.
:- import_module string.

:- import_module ast.env.
:- import_module ast.resolve.
:- import_module builtins.
:- import_module context.
:- import_module common_types.
:- import_module core.code.
:- import_module core.types.
:- import_module result.
:- import_module symtab.
:- import_module varmap.

%-----------------------------------------------------------------------%

ast_to_core(plasma_ast(ModuleName, Entries), Result) :-
    Exports = gather_exports(Entries),
    some [!Core, !Errors] (
        !:Core = core.init(symbol(ModuleName)),
        !:Errors = init,
        setup_builtins(!Core),
        foldl2(gather_funcs, Entries, !Core, !Errors),
        ( if is_empty(!.Errors) then
            foldl2(build_function(Exports), Entries, !Core, !Errors),
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

:- func gather_exports(list(past_entry)) = exports.

gather_exports(Entries) = Exports :-
    ( if member(past_export(export_all), Entries) then
        Exports = export_all
    else
        filter_map(
            (pred(Entry::in, Export::out) is semidet :-
                Entry = past_export(export_some(List)),
                Export = set(List)
            ), Entries, Sets),
        Exports = exports(union_list(Sets))
    ).

%-----------------------------------------------------------------------%

:- pred gather_funcs(past_entry::in, core::in, core::out,
    errors(compile_error)::in, errors(compile_error)::out) is det.

gather_funcs(past_export(_), !Core, !Errors).
gather_funcs(past_import(_, _), !Core, !Errors).
gather_funcs(past_type(_, _, _, _), !Core, !Errors).
gather_funcs(past_function(Name, _, _, _, _, Context),
        !Core, !Errors) :-
    ModuleName = module_name(!.Core),
    ( if
        core_register_function(symbol_append(ModuleName, Name), _, !Core)
    then
        true
    else
        add_error(Context, ce_function_already_defined(Name), !Errors)
    ).

%-----------------------------------------------------------------------%

:- pred build_function(exports::in, past_entry::in,
    core::in, core::out,
    errors(compile_error)::in, errors(compile_error)::out) is det.

build_function(_, past_export(_), !Core, !Errors).
build_function(_, past_import(_, _), !Core, !Errors).
build_function(_, past_type(_, _, _, _), !Core, !Errors).
build_function(Exports, past_function(Name, Params, Return, Using0,
        Body0, Context), !Core, !Errors) :-
    ModuleName = module_name(!.Core),
    det_core_lookup_function(!.Core, symbol_append(ModuleName, Name), FuncId),

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
        ParamNames = map((func(past_param(N, _)) = N), Params),
        Varmap0 = varmap.init,
        map_foldl(varmap.add_or_get_var, ParamNames, ParamVars, Varmap0,
            Varmap1),
        % XXX: parameters must be named appart.
        build_body(!.Core, Context, Body0, Body, Varmap1, Varmap),
        func_set_body(Varmap, ParamVars, Body, Function0, Function),
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

:- func build_param_type(past_param) = result(type_, compile_error).

build_param_type(past_param(_, Type)) = build_type(Type).

:- func build_type(past_type_expr) = result(type_, compile_error).

build_type(past_type(Qualifiers, Name, Args0, Context)) = Result :-
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
            Result = ok(type_(symbol(Qualifiers, Name), Args))
        ; ArgsResult = errors(Error),
            Result = errors(Error)
        )
    ).
build_type(past_type_var(Name, _Context)) = Result :-
    Result = ok(type_variable(Name)).

:- pred build_using(past_using::in,
    set(resource)::in, set(resource)::out,
    set(resource)::in, set(resource)::out) is det.

build_using(past_using(Type, ResourceName), !Using, !Observing) :-
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

% Steps 1-4 transform the statements to get them into a form
% that's easy to create the core representation from (step 6).
%
% 1. Resolve symbols, build a varmap and build var sets.
% 2. Name appart vars on different branches, except where they are
%    nonlocals (not needed yet).
% 3. Remove return statements.
% 4. Turn branch statements into branch expressions (Maybe this can be
%    done as part of the next step.
% 5. Transform the whole structure into an expression tree.
:- pred build_body(core::in, context::in, list(past_statement)::in,
    expr::out, varmap::in, varmap::out) is det.

build_body(Core, _Context, !.Statements, Expr, !Varmap) :-
    map_foldl2(resolve_symbols_stmt(Core), !Statements, env.init, _, !Varmap),
    remove_returns(!Statements, ReturnASTExprs, Context),
    map(build_expr(Core, !.Varmap, Context), ReturnASTExprs, ReturnExprs),
    ( if ReturnExprs = [ReturnExprP] then
        ReturnExpr = ReturnExprP
    else
        ReturnExpr = expr(e_sequence(ReturnExprs), code_info_init(Context))
    ),
    build_statements(Core, !.Varmap, ReturnExpr, !.Statements, Expr).

%-----------------------------------------------------------------------%

:- pred remove_returns(list(past_statement)::in, list(past_statement)::out,
    list(past_expression)::out, context::out) is det.

remove_returns([], [], [], nil_context).
remove_returns([Stmt0 | Stmts0], Stmts, Exprs, Context) :-
    ( if
        Stmt0 = ps_return_statement(Exprs0, ContextP)
    then
        ( Stmts0 = [],
            Stmts = [],
            Exprs = Exprs0,
            Context = ContextP
        ; Stmts0 = [_ | _],
            unexpected($file, $pred,
                "compile error: dead code after return")
        )
    else
        remove_returns(Stmts0, Stmts1, Exprs, Context),
        Stmts = [Stmt0 | Stmts1]
    ).

%-----------------------------------------------------------------------%

:- pred build_statements(core::in, varmap::in, expr::in,
    list(past_statement)::in, expr::out) is det.

build_statements(_, _, Expr, [], Expr).
build_statements(Core, Varmap, ResultExpr, [Stmt | Stmts], Expr) :-
    ( Stmt = ps_bang_call(Call, Context),
        build_call(Core, Varmap, Context, Call, expr(CallType, CallInfo0)),
        code_info_set_using_marker(has_using_marker, CallInfo0, CallInfo),
        CallExpr = expr(CallType, CallInfo),
        build_statements(Core, Varmap, ResultExpr, Stmts, StmtsExpr),
        Expr = expr_append(CallExpr, StmtsExpr)
    ; Stmt = ps_bang_asign_call(_Vars, _Call, _Context),
        sorry($file, $pred, "bang assign call")
%        build_call(Core, Varmap, Context, Call, expr(CallType, CallInfo0),
%            !Varmap),
%        code_info_set_using_marker(has_using_marker, CallInfo0, CallInfo),
%        CallExpr = expr(CallType, CallInfo),
%        Type = e_let(Vars, CallExpr),
%        Info = code_info_init(Context).
    ; Stmt = ps_asign_statement(ASTVars, ASTExprs, Context),
        map(build_expr(Core, Varmap, Context), ASTExprs, Exprs),
        ( if Exprs = [TupleP] then
            Tuple = TupleP
        else
            Tuple = expr(e_tuple(Exprs), code_info_init(Context))
        ),
        map(lookup_var(Varmap), ASTVars, Vars),
        Expr = expr(e_let(Vars, Tuple, StmtsExpr), code_info_init(Context)),
        build_statements(Core, Varmap, ResultExpr, Stmts, StmtsExpr)
    ; Stmt = ps_return_statement(_, _),
        unexpected($file, $pred, "Return statement")
    ; Stmt = ps_array_set_statement(_, _, _, _),
        sorry($file, $pred, "Array assignment")
    ).

:- pred build_expr(core::in, varmap::in, context::in, past_expression::in,
    expr::out) is det.

build_expr(Core, Varmap, Context, pe_call(Call), Expr) :-
    build_call(Core, Varmap, Context, Call, Expr).
build_expr(_, _, _, pe_symbol(_), _) :-
    unexpected($file, $pred, "Unresolved symbol").
build_expr(_, Varmap, Context, pe_var(VarName),
        expr(e_var(Var), code_info_init(Context))) :-
    % TODO: Store vars in the AST as "var".
    varmap.lookup_var(Varmap, VarName, Var).
build_expr(_, _, _, pe_func(_), _) :-
    sorry($file, $pred, "Higher order value").
build_expr(_, _, Context, pe_const(Const),
        expr(e_const(Value), code_info_init(Context))) :-
    ( Const = pc_string(String),
        Value = c_string(String)
    ; Const = pc_number(Number),
        Value = c_number(Number)
    ; Const = pc_list_nil,
        sorry($file, $pred, "list")
    ).
build_expr(_, _, _, pe_u_op(_, _), _) :-
    sorry($file, $pred, "Unary operators").
build_expr(_, _, _, pe_b_op(_, _, _), _) :-
    sorry($file, $pred, "Binary operators").
build_expr(_, _, _, pe_array(_), _) :-
    sorry($file, $pred, "Array").

:- pred build_call(core::in, varmap::in, context::in, past_call::in,
    expr::out) is det.

build_call(Core, Varmap, Context, past_call(Callee0, Args0), Expr) :-
    % TODO: Resolve this symbol earlier, possibly make Callee0 an
    % expression.
    ( if
        core_search_function(Core, Callee0, Callees),
        set.singleton_set(Callee, Callees)
    then
        map(build_expr(Core, Varmap, Context), Args0, Args),
        Expr = expr(e_call(Callee, Args), code_info_init(Context))
    else
        unexpected($file, $pred,
            format("Func not found: %s", [s(symbol_to_string(Callee0))]))
    ).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
