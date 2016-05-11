%-----------------------------------------------------------------------%
% Plasma AST
% vim: ts=4 sw=4 et
%
% Copyright (C) 2015 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
% This program compiles plasma modules.
%
%-----------------------------------------------------------------------%
:- module ast.
%-----------------------------------------------------------------------%

:- interface.

:- import_module list.
:- import_module maybe.
:- import_module string.

:- import_module context.
:- import_module symtab.

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

:- type past_statement
    --->    ps_bang_call(past_call, context)
    ;       ps_bang_asign_call(list(string), past_call, context)
    ;       ps_asign_statement(list(string), past_expression, context)
    ;       ps_return_statement(past_expression, context).

%
% Code
%
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
                pes_name            :: symbol
            )
    ;       pe_const(
                pec_value           :: past_const
            ).

:- type past_uop
    --->    pu_minus
    ;       pu_not.

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
    ;       pb_concat.

:- type past_const
    --->    pc_number(int)
    ;       pc_string(string).

:- type past_call
    --->    past_call(
                pec_callee          :: symbol,
                pec_args            :: list(past_expression)
            ).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

%-----------------------------------------------------------------------%


%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
