%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module asm_error.
%
% Copyright (C) 2015 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
% Error type for the PZ assembler.
%
%-----------------------------------------------------------------------%

:- interface.

:- import_module string.

:- import_module parse_util.
:- import_module result.
:- import_module symtab.

%-----------------------------------------------------------------------%

:- type asm_error
    --->    e_read_src_error(read_src_error)
    ;       e_name_already_defined(string)
    ;       e_symbol_not_found(symbol)
    ;       e_symbol_ambigious(symbol)
    ;       e_block_not_found(string)
    ;       e_stack_depth.

:- instance error(asm_error).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

:- import_module list.

%-----------------------------------------------------------------------%

:- instance error(asm_error) where [
        func(to_string/1) is asme_to_string,
        func(error_or_warning/1) is asme_error_or_warning
    ].

:- func asme_to_string(asm_error) = string.

asme_to_string(e_read_src_error(Error)) = to_string(Error).
asme_to_string(e_name_already_defined(Name)) =
    format("\"%s\" is already defined", [s(Name)]).
asme_to_string(e_symbol_not_found(Symbol)) =
    format("The symbol \"%s\" is undefined", [s(symbol_to_string(Symbol))]).
asme_to_string(e_symbol_ambigious(Symbol)) =
    format("The symbol \"%s\" is ambigious", [s(symbol_to_string(Symbol))]).
asme_to_string(e_block_not_found(Name)) =
    format("The block \"%s\" is undefined", [s(Name)]).
asme_to_string(e_stack_depth) =
    "Stack operations have a maximum depth of 255".

:- func asme_error_or_warning(asm_error) = error_or_warning.

asme_error_or_warning(_) = error.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
