%-----------------------------------------------------------------------%
% Plasma parse tree to core conversion
% vim: ts=4 sw=4 et
%
% Copyright (C) 2015-2016 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
% This program compiles plasma modules.
%
%-----------------------------------------------------------------------%
:- module ast_to_core.
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
gather_funcs(past_import(_), !Core, !Errors).
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
build_function(_, past_import(_), !Core, !Errors).
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

:- func build_type(past_type) = result(type_, compile_error).

build_type(past_type(Name, Args0, Context)) = Result :-
    det_index(Name, 0, FirstChar),
    ( if builtin_type_name(Type, Name) then
        ( Args0 = [],
            Result = ok(builtin_type(Type))
        ; Args0 = [_ | _],
            Result = return_error(Context, ce_builtin_type_with_args(Name))
        )
    else if is_lower(FirstChar) then
        ( Args0 = [],
            Result = ok(type_variable(Name))
        ; Args0 = [_ | _],
            Result = return_error(Context, ce_type_var_with_args(Name))
        )
    else
        ArgsResult = result_list_to_result(map(build_type, Args0)),
        ( ArgsResult = ok(Args),
            Result = ok(type_(symbol(Name), Args))
        ; ArgsResult = errors(Error),
            Result = errors(Error)
        )
    ).

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

:- pred build_body(core::in, context::in, list(past_statement)::in,
    expr::out, varmap::in, varmap::out) is det.

build_body(Core, Context, Statements, Expr, !Varmap) :-
    map_foldl(build_statement(Core), Statements, Exprs, !Varmap),
    Expr = expr(e_sequence(Exprs), code_info_init(Context)).

:- pred build_statement(core::in, past_statement::in, expr::out,
    varmap::in, varmap::out) is det.

build_statement(Core, ps_bang_statement(PStmt), expr(Type, Info), !Varmap) :-
    build_statement(Core, PStmt, expr(Type, Info0), !Varmap),
    code_info_set_using_marker(has_using_marker, Info0, Info).
build_statement(Core, ps_expr_statement(Expr0, Context), Expr, !Varmap) :-
    build_expr(Core, Context, Expr0, Expr, !Varmap).

:- pred build_expr(core::in, context::in, past_expression::in, expr::out,
    varmap::in, varmap::out) is det.

build_expr(Core, Context, pe_call(Callee0, Args0), Expr, !Varmap) :-
    build_expr(Core, Context, Callee0, Callee1, !Varmap),
    ( if Callee1 = expr(e_func(CalleePrime), _) then
        Callee = CalleePrime
    else
        unexpected($file, $pred, "Higher order call")
    ),
    map_foldl(build_expr(Core, Context), Args0, Args, !Varmap),
    Expr = expr(e_call(Callee, Args), code_info_init(Context)).
build_expr(Core, Context, pe_symbol(Symbol),
        expr(ExprType, code_info_init(Context)), !Varmap) :-
    ( if
        symbol_parts(Symbol, [], Name),
        search_var(!.Varmap, Name, Var)
    then
        ExprType = e_var(Var)
    else
        ( if
            core_search_function(Core, Symbol, Funcs),
            singleton_set(Func, Funcs)
        then
            ExprType = e_func(Func)
        else
            unexpected($file, $pred,
                format("Symbol '%s' not found or ambigious",
                    [s(symbol_to_string(Symbol))]))
        )
    ).

build_expr(_, Context, pe_const(Const),
        expr(e_const(Value), code_info_init(Context)), !Varmap) :-
    ( Const = pc_string(String),
        Value = c_string(String)
    ; Const = pc_number(Number),
        Value = c_number(Number)
    ).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
