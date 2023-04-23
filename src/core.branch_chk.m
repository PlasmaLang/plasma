%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module core.branch_chk.
%
% Copyright (C) Plasma Team
% Distributed under the terms of the MIT see ../LICENSE.code
%
% Plasma branch checking.
%
%-----------------------------------------------------------------------%

:- interface.

:- import_module io.

:- import_module compile_error.
:- import_module util.log.
:- import_module util.result.

:- pred branch_check(log_config::in, errors(compile_error)::out,
    core::in, core::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
:- implementation.

:- import_module cord.
:- import_module require.

:- import_module context.
:- import_module core.util.
:- import_module util.mercury.

%-----------------------------------------------------------------------%

branch_check(Verbose, Errors, !Core, !IO) :-
    process_noerror_funcs(Verbose, branchcheck_func, Errors, !Core, !IO).

:- pred branchcheck_func(core::in, func_id::in, function::in,
    result_partial(function, compile_error)::out) is det.

branchcheck_func(Core, _FuncId, Func, Result) :-
    ( if
        func_get_body(Func, _, _, _, Expr),
        func_get_vartypes(Func, Vartypes)
    then
        Errors = branchcheck_expr(Core, Vartypes, Expr),
        ( if not has_fatal_errors(Errors) then
            Result = ok(Func, Errors)
        else
            Result = errors(Errors)
        )
    else
        unexpected($file, $pred, "Function body or types not present")
    ).

:- func branchcheck_expr(core, map(var, type_), expr) = errors(compile_error).

branchcheck_expr(Core, Vartypes, expr(ExprType, CodeInfo)) = Errors :-
    ( ExprType = e_tuple(Exprs),
        Errors = cord_list_to_cord(map(branchcheck_expr(Core, Vartypes), Exprs))
    ; ExprType = e_lets(Lets, Expr),
        Errors =
            cord_list_to_cord(map(branchcheck_let(Core, Vartypes), Lets)) ++
            branchcheck_expr(Core, Vartypes, Expr)
    ;
        ( ExprType = e_call(_, _, _)
        ; ExprType = e_var(_)
        ; ExprType = e_constant(_)
        ; ExprType = e_construction(_, _)
        ; ExprType = e_closure(_, _)
        ),
        Errors = init
    ; ExprType = e_match(Var, Cases),
        map.lookup(Vartypes, Var, Type),
        Context = code_info_context(CodeInfo),
        Errors = branchcheck_match(Core, Context, Type, Cases)
    ).

:- func branchcheck_let(core, map(var, type_), expr_let) =
    errors(compile_error).

branchcheck_let(Core, Vartypes, e_let(_, Expr)) =
    branchcheck_expr(Core, Vartypes, Expr).

:- func branchcheck_match(core, context, type_, list(expr_case)) =
    errors(compile_error).

branchcheck_match(Core, Context, Type, Cases) = Errors :-
    ( Type = builtin_type(Builtin),
        % Int and string have an infinite number of values. Their pattern
        % matches must contain at least one wildcard.
        (
            ( Builtin = int
            ; Builtin = string
            ; Builtin = codepoint
            )
        ; Builtin = string_pos,
            unexpected($file, $pred, "Match on opaque builtin")
        ),
        Errors = branchcheck_inf(Context, Cases, set.init)
    ; Type = type_ref(TypeId, _),
        MaybeCtors = utype_get_ctors(core_get_type(Core, TypeId)),
        ( MaybeCtors = yes(CtorsList),
            Ctors = list_to_set(CtorsList)
        ; MaybeCtors = no,
            unexpected($file, $pred, "Pattern match on abstract type")
        ),
        Errors = branchcheck_type(Context, Ctors, Cases)
    ; Type = type_variable(_),
        unexpected($file, $pred, "Type variable in match")
    ; Type = func_type(_, _, _, _),
        Errors = error(Context, ce_match_on_function_type)
    ).

:- func branchcheck_inf(context, list(expr_case), set(int)) =
    errors(compile_error).

branchcheck_inf(Context, [], _) =
    error(Context, ce_match_does_not_cover_all_cases).
branchcheck_inf(Context, [e_case(Pat, Expr) | Cases], SeenSet0) = Errors :-
    ( Pat = p_num(Num),
        ( if insert_new(Num, SeenSet0, SeenSet) then
            Errors = branchcheck_inf(Context, Cases, SeenSet)
        else
            Errors = error(code_info_context(Expr ^ e_info),
                    ce_match_duplicate_case) ++
                branchcheck_inf(Context, Cases, SeenSet0)
        )
    ;
        ( Pat = p_variable(_)
        ; Pat = p_wildcard
        ),
        Errors = branchcheck_tail(Cases)
    ; Pat = p_ctor(_, _),
        unexpected($file, $pred, "Constructor seen on builtin type match")
    ).

:- func branchcheck_type(context, set(ctor_id), list(expr_case)) =
    errors(compile_error).

branchcheck_type(Context, TypeCtors, []) =
    ( if is_empty(TypeCtors) then
        init
    else
        error(Context, ce_match_does_not_cover_all_cases)
    ).
branchcheck_type(Context, TypeCtors, [e_case(Pat, Expr) | Cases]) = Errors :-
    ( if is_empty(TypeCtors) then
        Errors = error(code_info_context(Expr ^ e_info),
            ce_match_unreached_cases)
    else
        ( Pat = p_num(_),
            unexpected($file, $pred, "Number seen on user type match")
        ;
            ( Pat = p_variable(_)
            ; Pat = p_wildcard
            ),
            Errors = branchcheck_tail(Cases)
        ; Pat = p_ctor(Ctors, _),
            % There should be only one constructor here because typechecking
            % would have made it unambigious.
            Ctor = one_item_in_set(Ctors),
            ( if remove(Ctor, TypeCtors, RestCtors) then
                Errors = branchcheck_type(Context, RestCtors, Cases)
            else
                % The only way remove can fail when the program is type
                % correct is if there is a duplicate case.
                Errors = error(code_info_context(Expr ^ e_info),
                        ce_match_duplicate_case) ++
                    branchcheck_type(Context, TypeCtors, Cases)
            )
        )
    ).

:- func branchcheck_tail(list(expr_case)) = errors(compile_error).

branchcheck_tail([]) = init.
branchcheck_tail([e_case(_, expr(_, CodeInfo)) | _]) =
    error(code_info_context(CodeInfo), ce_match_unreached_cases).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
