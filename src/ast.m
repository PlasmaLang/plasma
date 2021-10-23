%-----------------------------------------------------------------------%
% Plasma AST
% vim: ts=4 sw=4 et
%
% Copyright (C) 2015-2021 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
% This module represents the AST for plasma programs.
%
%-----------------------------------------------------------------------%
:- module ast.
%-----------------------------------------------------------------------%

:- interface.

:- import_module list.
:- import_module maybe.

:- import_module common_types.
:- import_module context.
:- import_module q_name.
:- import_module varmap.

:- type ast == ast(ast_entry).
:- type ast(E)
    --->    ast(
                a_module_name       :: q_name,
                % Context of module declaration.
                a_context           :: context,
                a_entries           :: list(E)
            ).

% AST for include files.
:- type ast_interface == ast(ast_interface_entry).

% AST for typeres files.
:- type ast_typeres == ast(ast_typeres_entry).

:- type ast_entry
    --->    ast_import(ast_import)
    ;       ast_type(nq_name, ast_type(nq_name))
    ;       ast_resource(nq_name, ast_resource)
    ;       ast_function(nq_name, ast_function).

:- type ast_interface_entry
    --->    asti_resource(q_name, ast_resource)
    ;       asti_type(q_name, ast_type(q_name))
    ;       asti_function(q_name, ast_function_decl).

:- type ast_typeres_entry
    --->    asti_resource_abs(q_name)
    ;       asti_type_abs(q_name, arity).

:- type ast_import
    --->    ast_import(
                ai_name             :: q_name,
                ai_as               :: maybe(string),
                ai_context          :: context
            ).

:- type ast_type(Name)
    --->    ast_type(
                at_params           :: list(string),
                at_costructors      :: list(at_constructor(Name)),
                at_export           :: sharing_type,
                at_context          :: context
            )
            % An abstractly-imported type.
            % This module has no knowledge of the constructors and
            % these are always st_private.
    ;       ast_type_abstract(
                ata_arity           :: arity,
                ata_context         :: context
            ).

:- type ast_resource
    --->    ast_resource(
                ar_from             :: q_name,
                ar_sharing          :: sharing,
                ar_context          :: context
            ).

:- type ast_function_decl
    --->    ast_function_decl(
                afd_params          :: list(ast_param),
                afd_return          :: list(ast_type_expr),
                afd_uses            :: list(ast_uses),
                afd_context         :: context
            ).

:- type ast_function
    --->    ast_function(
                af_decl             :: ast_function_decl,
                af_body             :: ast_body,
                af_export           :: sharing,
                af_is_entrypoint    :: is_entrypoint
            ).

:- type ast_body
    --->    ast_body_block(
                list(ast_block_thing)
            )
    ;       ast_body_foreign.

:- type ast_block_thing(Info)
    --->    astbt_statement(ast_statement(Info))
    ;       astbt_function(nq_name, ast_nested_function).

:- type ast_block_thing == ast_block_thing(context).

:- type ast_nested_function
    --->    ast_nested_function(
                anf_decl            :: ast_function_decl,
                anf_body            :: list(ast_block_thing)
            ).

%
% Modules, imports and exports.
%
:- type export_some_or_all
    --->    export_some(list(string))
    ;       export_all.

%
% Types
%
:- type at_constructor(Name)
    --->    at_constructor(
                atc_name        :: Name,
                atc_args        :: list(at_field),
                atc_context     :: context
            ).

:- type at_field
    --->    at_field(
                atf_name        :: string,
                atf_type        :: ast_type_expr,
                atf_context     :: context
            ).

:- type ast_type_expr
    --->    ast_type(
                ate_name            :: q_name,
                ate_args            :: list(ast_type_expr),
                ate_context         :: context
            )
    ;       ast_type_func(
                atf_args            :: list(ast_type_expr),
                atf_returns         :: list(ast_type_expr),
                atf_uses            :: list(ast_uses),
                atf_context_        :: context
            )
    ;       ast_type_var(
                atv_name            :: string,
                atv_context         :: context
            ).

%
% Code signatures
%
:- type ast_param
    --->    ast_param(
                ap_name             :: var_or_wildcard(string),
                ap_type             :: ast_type_expr
            ).

:- type ast_uses
    --->    ast_uses(
                au_uses_type        :: uses_type,
                au_name             :: q_name
            ).

:- type uses_type
    --->    ut_uses
    ;       ut_observes.

%
% Code
%
:- type ast_statement(Info)
    --->    ast_statement(
                ast_stmt_type       :: ast_stmt_type(Info),
                ast_stmt_info       :: Info
            ).

:- type ast_statement == ast_statement(context).

:- type ast_stmt_type(Info)
            % A statement that looks like a call must be a call, it cannot
            % be a construction as that would have no effect.
    --->    s_call(ast_call_like)
    ;       s_assign_statement(
                as_ast_vars         :: list(ast_pattern),
                as_expr             :: list(ast_expression)
            )
    ;       s_array_set_statement(
                sas_array           :: string,
                sas_subscript       :: ast_expression,
                sas_rhs             :: ast_expression
            )
    ;       s_return_statement(list(ast_expression))
    ;       s_var_statement(
                vs_vars             :: string
            )
    ;       s_match_statement(
                sms_expr            :: ast_expression,
                sms_cases           :: list(ast_match_case(Info))
            )
    ;       s_ite(
                psi_cond            :: ast_expression,
                psi_then            :: list(ast_block_thing(Info)),
                psi_else            :: list(ast_block_thing(Info))
            ).

:- type ast_match_case(Info)
    --->    ast_match_case(
                c_pattern           :: ast_pattern,
                c_stmts             :: list(ast_block_thing(Info))
            ).

:- type ast_match_case == ast_match_case(context).

:- type ast_expression
    --->    e_call_like(
                ec_call_like        :: ast_call_like
            )
    ;       e_u_op(
                euo_op              :: ast_uop,
                euo_expr            :: ast_expression
            )
    ;       e_b_op(
                ebo_expr_left       :: ast_expression,
                ebo_op              :: ast_bop,
                ebo_expr_right      :: ast_expression
            )
    ;       e_match(
                em_expr             :: ast_expression,
                em_cases            :: list(ast_expr_match_case)
            )
    ;       e_if(
                eif_cond            :: ast_expression,
                eif_then            :: list(ast_expression),
                eif_else            :: list(ast_expression)
            )
    ;       e_symbol(
                es_name             :: q_name
            )
    ;       e_const(
                ec_value            :: ast_const
            )
    ;       e_array(
                ea_values           :: list(ast_expression)
            ).

:- type ast_uop
    --->    u_minus
    ;       u_not.

:- type ast_bop
    --->    b_add
    ;       b_sub
    ;       b_mul
    ;       b_div
    ;       b_mod
    ;       b_lt
    ;       b_gt
    ;       b_lteq
    ;       b_gteq
    ;       b_eq
    ;       b_neq
    ;       b_logical_and
    ;       b_logical_or
    ;       b_concat
    ;       b_list_cons
    ;       b_array_subscript.

:- type ast_expr_match_case
    --->    ast_emc(ast_pattern, list(ast_expression)).

:- type ast_const
    --->    c_number(int)
    ;       c_string(string)
    ;       c_list_nil.

    % A call or call-like thing (such as a construction).
    %
:- type ast_call_like
    --->    ast_call_like(
                ec_callee           :: ast_expression,
                ec_args             :: list(ast_expression)
            )
    ;       ast_bang_call(
                ebc_callee          :: ast_expression,
                ebc_args            :: list(ast_expression)
            ).

:- type ast_pattern
    --->    p_constr(q_name, list(ast_pattern))
    ;       p_number(int)
    ;       p_wildcard
    ;       p_var(string) % A declaration of a new variable
    ;       p_symbol(q_name) % The binding of a new variable or a
                             % constructor with zero args.
    ;       p_list_nil
    ;       p_list_cons(ast_pattern, ast_pattern).

%-----------------------------------------------------------------------%

:- func type_arity(ast_type(T)) = arity.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

%-----------------------------------------------------------------------%

type_arity(ast_type(Params, _, _, _)) = arity(length(Params)).
type_arity(ast_type_abstract(Arity, _)) = Arity.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
