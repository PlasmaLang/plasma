%-----------------------------------------------------------------------%
% Plasma parser
% vim: ts=4 sw=4 et
%
% Copyright (C) 2015 Paul Bone
% Distributed under the terms of the GPLv2 see ../LICENSE.tools
%
% This program compiles plasma modules.
%
%-----------------------------------------------------------------------%
:- module parse.
%-----------------------------------------------------------------------%

:- interface.

:- import_module io.
:- import_module string.

:- import_module ast.
:- import_module result.
:- import_module lex_util.

%-----------------------------------------------------------------------%

:- pred parse(string::in, result(plasma_ast, read_src_error)::out,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module list.
:- import_module maybe.
:- import_module require.

:- import_module ast.
:- import_module context.
:- import_module lex.
:- import_module parsing.
:- import_module parsing.bnf.
:- import_module symtab.

%-----------------------------------------------------------------------%

parse(Filename, Result, !IO) :-
    parse_file(Filename, lexemes, ignore_tokens, plasma_bnf, Result0, !IO),
    ( Result0 = ok(PNode),
        ( PNode = module_(AST) ->
            Result = ok(AST)
        ;
            unexpected($file, $pred, "Wrong node type")
        )
    ; Result0 = errors(Errors),
        Result = errors(Errors)
    ).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- type token_type
    --->    module_
    ;       import
    ;       using
    ;       observing
    ;       ident
    ;       string
    ;       l_curly
    ;       r_curly
    ;       l_paren
    ;       r_paren
    ;       semicolon
    ;       colon
    ;       comma
    ;       period
    ;       arrow
    ;       bang
    ;       newline
    ;       comment
    ;       whitespace
    ;       eof.

:- func lexemes = list(lexeme(lex_token(token_type))).

lexemes = [
        ("module"           -> return_simple(module_)),
        ("import"           -> return_simple(import)),
        ("using"            -> return_simple(using)),
        ("observing"        -> return_simple(observing)),
        ("{"                -> return_simple(l_curly)),
        ("}"                -> return_simple(r_curly)),
        ("("                -> return_simple(l_paren)),
        (")"                -> return_simple(r_paren)),
        (";"                -> return_simple(semicolon)),
        (":"                -> return_simple(colon)),
        (","                -> return_simple(comma)),
        ("."                -> return_simple(period)),
        ("->"               -> return_simple(arrow)),
        ("!"                -> return_simple(bang)),
        (lex.identifier     -> return_string(ident)),
        % TODO: escapes
        ("\"" ++ *(anybut("\"")) ++ "\""
                            -> (func(S0) = lex_token(string, yes(S)) :-
                                    between(S0, 1, length(S0) - 1, S))),

        (("#" ++ *(anybut("\n")))
                            -> return_simple(comment)),
        ("\n"               -> return_simple(newline)),
        (any(" \t\v\f")     -> return_simple(whitespace))
    ].

:- pred ignore_tokens(lex_token(token_type)::in) is semidet.

ignore_tokens(lex_token(whitespace, _)).
ignore_tokens(lex_token(newline, _)).
ignore_tokens(lex_token(comment, _)).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- type non_terminal
    --->    module_
    ;       module_decl
    ;       export_list
    ;       export_list_continue
    ;       toplevel_items
    ;       toplevel_item
    ;       import_directive.

:- func plasma_bnf = bnf(token_type, non_terminal, pt_node).

plasma_bnf = bnf(module_, eof,
    [
        bnf_rule("module", module_,
            [bnf_rhs([nt(module_decl), nt(toplevel_items)],
                (func(Nodes) =
                    (
                        Nodes = [module_decl(Name, MaybeExports),
                            toplevel_items(Items)]
                    ->
                        module_(plasma_ast(Name, MaybeExports, Items))
                    ;
                        unexpected($file, $pred, string(Nodes))
                    )
                ))
            ]),

        % Module declration
        bnf_rule("module decl", module_decl,
            [bnf_rhs([t(module_), t(ident), nt(export_list)],
                (func(Nodes) =
                    ( Nodes = [_, ident(Name), nil] ->
                        module_decl(Name, no)
                    ; Nodes = [_, ident(Name), export_list(List)] ->
                        module_decl(Name, yes(List))
                    ;
                        unexpected($file, $pred, string(Nodes))
                    )
                ))
            ]),
        bnf_rule("export list", export_list,
            [bnf_rhs(
                [t(l_curly), t(ident), nt(export_list_continue), t(r_curly)],
                (func(Nodes) =
                    ( Nodes = [_, ident(X), export_list(Xs), _] ->
                        export_list([X | Xs])
                    ;
                        unexpected($file, $pred, string(Nodes))
                    )
                )),
            bnf_rhs([], const(nil))
            ]),
        bnf_rule("export list continue", export_list_continue,
            [bnf_rhs([t(comma), t(ident), nt(export_list_continue)],
                (func(Nodes) =
                    ( Nodes = [_, ident(X), export_list(Xs)] ->
                        export_list([X | Xs])
                    ;
                        unexpected($file, $pred, string(Nodes))
                    )
                )),
            bnf_rhs([],
                const(export_list([])))
            ]),

        % Toplevel items
        bnf_rule("toplevel items", toplevel_items,
            [bnf_rhs([],
                const(toplevel_items([]))),
            bnf_rhs([nt(toplevel_item), nt(toplevel_items)],
                (func(Nodes) =
                    ( Nodes = [toplevel_item(X), toplevel_items(Xs)] ->
                        toplevel_items([X | Xs])
                    ;
                        unexpected($file, $pred, string(Nodes))
                    )
                ))
            ]),

        bnf_rule("toplevel item", toplevel_item,
            [bnf_rhs([nt(import_directive)], identity)]),

        % Import directive.
        bnf_rule("import directive", import_directive,
            [bnf_rhs([t(import), t(ident)],
                (func(Nodes) =
                    ( Nodes = [_, ident(Name)] ->
                        toplevel_item(past_import(symbol(Name)))
                    ;
                        unexpected($file, $pred, string(Nodes))
                    )
                ))
            ])
    ]).

:- type pt_node
    --->    module_(plasma_ast)
    ;       module_decl(string, maybe(list(string)))
    ;       export_list(list(string))
    ;       toplevel_items(list(past_entry))
    ;       toplevel_item(past_entry)
    ;       ident(string)
    ;       nil.

:- instance token_to_result(token_type, pt_node) where [
        token_to_result(Type, MaybeString, _) =
            ( Type = ident, MaybeString = yes(String) ->
                ident(String)
            ;
                nil
            )
    ].

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
