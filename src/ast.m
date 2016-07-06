%-----------------------------------------------------------------------%
% Plasma AST
% vim: ts=4 sw=4 et
%
% Copyright (C) 2015 Plasma Team
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
:- import_module string.

:- import_module context.
:- import_module core.
:- import_module q_name.
:- import_module varmap.

:- include_module ast.env.
:- include_module ast.resolve.

:- type plasma_ast
    --->    plasma_ast(
                pa_module_name      :: string,
                pa_entries          :: list(past_entry)
            ).

:- type past_entry
    --->    past_export(
                pae_names           :: export_some_or_all
            )
    ;       past_import(
                pai_names           :: import_name,
                pai_as              :: maybe(string)
            )
    ;       past_type(
                pat_name            :: string,
                pat_params          :: list(string),
                pat_costructors     :: list(pat_constructor),
                pat_context         :: context
            )
    ;       past_function(
                paf_name            :: string,
                paf_params          :: list(past_param),
                paf_return          :: past_type_expr,
                paf_using           :: list(past_using),
                paf_body            :: list(past_statement),
                paf_context         :: context
            ).

%
% Modules, imports and exports.
%
:- type export_some_or_all
    --->    export_some(list(string))
    ;       export_all.

:- type import_name
    --->    dot(string, import_name_2).

:- type import_name_2
    --->    nil
    ;       star
    ;       dot(string, import_name_2).

%
% Types
%
:- type pat_constructor
    --->    pat_constructor(
                patc_name       :: string,
                patc_args       :: list(pat_field),
                patc_context    :: context
            ).

:- type pat_field
    --->    pat_field(
                patf_name       :: string,
                patf_type       :: past_type_expr,
                patf_context    :: context
            ).

:- type past_type_expr
    --->    past_type(
                pate_qualifiers     :: list(string),
                pate_name           :: string,
                pate_args           :: list(past_type_expr),
                pate_context        :: context
            )
    ;       past_type_var(
                patv_name           :: string,
                patv_context        :: context
            ).

%
% Code signatures
%
:- type past_param
    --->    past_param(
                pap_name            :: string,
                pap_type            :: past_type_expr
            ).

:- type past_using
    --->    past_using(
                pau_using_type      :: using_type,
                pau_name            :: string
            ).

:- type using_type
    --->    ut_using
    ;       ut_observing.

%
% Code
%
:- type past_statement(Info)
    --->    past_statement(past_stmt_type(Info), Info).

:- type past_statement == past_statement(context).

:- type past_stmt_type(Info)
    --->    ps_call(past_call)
    ;       ps_asign_statement(
                pas_ast_vars        :: list(string),
                pas_vars            :: maybe(list(var)),
                pas_exprs           :: list(past_expression)
            )
    ;       ps_array_set_statement(
                psas_array          :: string,
                psas_subscript      :: past_expression,
                psas_rhs            :: past_expression
            )
    ;       ps_return_statement(list(past_expression))
    ;       ps_match_statement(
                psms_expr           :: past_expression,
                psms_cases          :: list(past_match_case(Info))
            ).

:- type past_match_case(Info)
    --->    past_match_case(
                pc_pattern              :: past_pattern,
                pc_stmts                :: list(past_statement(Info))
            ).

:- type past_match_case == past_match_case(context).

:- type past_expression
    --->    pe_call(
                pec_call            :: past_call
            )
    ;       pe_u_op(
                peuo_op             :: past_uop,
                peuo_expr           :: past_expression
            )
    ;       pe_b_op(
                pebo_expr_left      :: past_expression,
                pebo_op             :: past_bop,
                pebo_expr_right     :: past_expression
            )
    ;       pe_symbol(
                pes_name            :: q_name
            )
    ;       pe_var(
                pev_var             :: var
            )
    ;       pe_func(
                pef_func            :: func_id
            )
    ;       pe_const(
                pec_value           :: past_const
            )
    ;       pe_array(
                pea_values          :: list(past_expression)
            ).

:- type past_uop
    --->    pu_minus
    ;       pu_comp.

:- type past_bop
    --->    pb_add
    ;       pb_sub
    ;       pb_mul
    ;       pb_div
    ;       pb_mod
    ;       pb_lshift
    ;       pb_rshift
    ;       pb_and
    ;       pb_or
    ;       pb_xor
    ;       pb_concat
    ;       pb_list_cons
    ;       pb_array_subscript.

:- type past_const
    --->    pc_number(int)
    ;       pc_string(string)
    ;       pc_list_nil.

:- type past_call
    --->    past_call(
                pec_callee          :: past_expression,
                pec_args            :: list(past_expression)
            )
    ;       past_bang_call(
                pebc_callee         :: past_expression,
                pebc_args           :: list(past_expression)
            ).

:- type past_pattern
    --->    pp_number(int)
    ;       pp_ident(string).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
