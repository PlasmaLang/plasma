%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module asm_error.
%
% Copyright (C) Plasma Team
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
        pred(pretty/4) is asme_pretty,
        func(error_or_warning/1) is asme_error_or_warning
    ].

:- pred asme_pretty(string::in, asm_error::in, list(pretty)::out, list(pretty)::out) is det.

asme_pretty(SrcPath, Error, Para, Extra) :-
    ( Error = e_read_src_error(ReadSrcError),
        pretty(SrcPath, ReadSrcError, Para, Extra)
    ;
        ( Error = e_name_already_defined(Name),
            Para = [p_quote("\"", p_str(Name))] ++ p_spc_nl ++
                p_words("is already defined")
        ; Error = e_no_such_instruction(Name),
            Para = [p_quote("\"", p_str(Name))] ++ p_spc_nl ++
                p_words("is not a PZ instruction")
        ; Error = e_symbol_not_found(Symbol),
            Para = p_words("The symbol") ++ p_spc_nl ++
                [p_quote("\"", q_name_pretty(Symbol))] ++ p_spc_nl ++
                p_words("is undefined")
        ; Error = e_block_not_found(Name),
            Para = p_words("The block") ++ p_spc_nl ++
                [p_quote("\"", p_str(Name))] ++ p_spc_nl ++
                p_words("is undefined")
        ; Error = e_struct_not_found(Name),
            Para = p_words("The structure") ++ p_spc_nl ++
                [p_quote("\"", p_str(Name))] ++ p_spc_nl ++
                p_words("is undefined")
        ; Error = e_stack_depth,
            Para = p_words("Stack operations have a maximum depth of 255")
        ; Error = e_import_not_found(Symbol),
            Para = p_words("The symbol") ++ p_spc_nl ++
                [p_quote("\"", q_name_pretty(Symbol))] ++ p_spc_nl ++
                p_words("cannot be found or is not an imported procedure")
        ),
        Extra = []
    ).

:- func asme_error_or_warning(asm_error) = error_or_warning.

asme_error_or_warning(_) = error.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
