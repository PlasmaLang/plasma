%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module asm_error.
%
% Copyright (C) 2015, 2017-2018, 2020 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
% Error type for the PZ assembler.
%
%-----------------------------------------------------------------------%

:- interface.

:- import_module parse_util.
:- import_module q_name.
:- import_module util.
:- import_module util.result.

%-----------------------------------------------------------------------%

:- type asm_error
    --->    e_read_src_error(read_src_error)
    ;       e_name_already_defined(string)
    ;       e_no_such_instruction(string)
    ;       e_symbol_not_found(q_name)
    ;       e_block_not_found(string)
    ;       e_struct_not_found(string)
    ;       e_import_not_found(q_name)
    ;       e_stack_depth.

:- instance error(asm_error).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module string.

:- import_module util.pretty.

%-----------------------------------------------------------------------%

:- instance error(asm_error) where [
        func(pretty/1) is asme_pretty,
        func(error_or_warning/1) is asme_error_or_warning
    ].

:- func asme_pretty(asm_error) = list(pretty).

asme_pretty(e_read_src_error(Error)) = pretty(Error).
asme_pretty(e_name_already_defined(Name)) =
    [p_quote("\"", p_str(Name))] ++ p_spc_nl ++ p_words("is already defined").
asme_pretty(e_no_such_instruction(Name)) =
    [p_quote("\"", p_str(Name))] ++ p_spc_nl ++ p_words("is not a PZ instruction").
asme_pretty(e_symbol_not_found(Symbol)) =
    p_words("The symbol") ++ p_spc_nl ++
    [p_quote("\"", q_name_pretty(Symbol))] ++ p_spc_nl ++
    p_words("is undefined").
asme_pretty(e_block_not_found(Name)) =
    p_words("The block") ++ p_spc_nl ++
    [p_quote("\"", p_str(Name))] ++ p_spc_nl ++
    p_words("is undefined").
asme_pretty(e_struct_not_found(Name)) =
    p_words("The structure") ++ p_spc_nl ++
    [p_quote("\"", p_str(Name))] ++ p_spc_nl ++
    p_words("is undefined").
asme_pretty(e_stack_depth) =
    p_words("Stack operations have a maximum depth of 255").
asme_pretty(e_import_not_found(Symbol)) =
    p_words("The symbol") ++ p_spc_nl ++
    [p_quote("\"", q_name_pretty(Symbol))] ++ p_spc_nl ++
    p_words("cannot be found or is not an imported procedure").
:- func asme_error_or_warning(asm_error) = error_or_warning.

asme_error_or_warning(_) = error.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
