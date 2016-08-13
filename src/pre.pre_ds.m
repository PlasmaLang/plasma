%-----------------------------------------------------------------------%
% Plasma pre-core representation
% vim: ts=4 sw=4 et
%
% Copyright (C) 2016 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
% This module represents the pre-core representation.
%
%-----------------------------------------------------------------------%
:- module pre.pre_ds.
%-----------------------------------------------------------------------%

:- interface.

:- import_module list.
:- import_module set.

:- import_module common_types.
:- import_module varmap.

%-----------------------------------------------------------------------%

:- type pre_statements == list(pre_statement).

:- type pre_statement
    --->    pre_statement(
                s_type      :: pre_stmt_type,
                s_info      :: pre_stmt_info
            ).

    % TODO: Support multi-value statements and expressions.
    %
:- type pre_stmt_type
    --->    s_call(pre_call)
    ;       s_assign(var, pre_expr)
    ;       s_return(var)
    ;       s_match(var, list(pre_case)).

:- type pre_stmt_info
    --->    stmt_info(
                si_context      :: context,

                    % Does not include def vars.
                si_use_vars     :: set(var),

                si_def_vars     :: set(var),
                si_non_locals   :: set(var)
            ).

:- type pre_call
    % XXX: Maybe use only variables as call arguments?
    --->    pre_call(func_id, list(pre_expr), with_bang).

:- type with_bang
    --->    with_bang
    ;       without_bang.

:- type pre_case
    --->    pre_case(pre_pattern, pre_statements).

:- type pre_pattern
    --->    pre_pattern.

:- type pre_expr
    --->    e_call(pre_call)
    ;       e_var(var)
    ;       e_const(const_type)
            % TODO: Refactor this into const.
    ;       e_func(func_id).

