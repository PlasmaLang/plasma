%-----------------------------------------------------------------------%
% Plasma parser
% vim: ts=4 sw=4 et
%
% Copyright (C) 2015 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
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
    ;       export
    ;       import
    ;       func_
    ;       using
    ;       observing
    ;       ident
    ;       number
    ;       string
    ;       l_curly
    ;       r_curly
    ;       l_paren
    ;       r_paren
    ;       semicolon
    ;       colon
    ;       comma
    ;       period
    ;       star
    ;       arrow
    ;       bang
    ;       newline
    ;       comment
    ;       whitespace
    ;       eof.

:- func lexemes = list(lexeme(lex_token(token_type))).

lexemes = [
        ("module"           -> return_simple(module_)),
        ("export"           -> return_simple(export)),
        ("import"           -> return_simple(import)),
        ("func"             -> return_simple(func_)),
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
        ("*"                -> return_simple(star)),
        ("->"               -> return_simple(arrow)),
        ("!"                -> return_simple(bang)),
        (signed_int         -> return_string(number)),
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
    ;       toplevel_items
    ;       toplevel_item
    ;       export_directive
    ;       export_arg
    ;       import_directive

    ;       func_defn
    ;       func_param_list
    ;       maybe_using

    ;       block
    ;       statements
    ;       statement

    ;       expr
    ;       expr_part1
    ;       expr_part2
    ;       call_arg_list
    ;       call_arg_list_cont

    ;       type_expr
    ;       type_
    ;       maybe_type_parameters
    ;       type_parameters

    ;       ident_list
    ;       ident_list_cont.

:- func plasma_bnf = bnf(token_type, non_terminal, pt_node).

plasma_bnf = bnf(module_, eof,
    [
        % I will show the EBNF in comments.  NonTerminals appear in
        % CamelCase and terminals appear in lower_underscore_case.
        %
        % Plasma := ModuleDecl ToplevelItem*
        %
        bnf_rule("module", module_, [
            bnf_rhs([nt(module_decl), nt(toplevel_items)],
                det_func((pred(Nodes::in, Node::out) is semidet :-
                    Nodes = [module_decl(Name),
                        toplevel_items(Items)],
                    Node = module_(plasma_ast(Name, Items))
                ))
            )
        ]),

        % ModuleDecl := module ident
        bnf_rule("module decl", module_decl, [
            bnf_rhs([t(module_), t(ident)],
                det_func((pred(Nodes::in, Node::out) is semidet :-
                    Nodes = [_, ident(Name, _)],
                    Node = module_decl(Name)
                ))
            )
        ]),

        bnf_rule("toplevel items", toplevel_items, [
            bnf_rhs([],
                const(toplevel_items([]))),
            bnf_rhs([nt(toplevel_item), nt(toplevel_items)],
                det_func((pred(Nodes::in, Node::out) is semidet :-
                    Nodes = [toplevel_item(X), toplevel_items(Xs)],
                    Node = toplevel_items([X | Xs])
                ))
            )
        ]),

        % ToplevelItem := ExportDirective
        %               | ImportDirective
        %               | FuncDefinition
        bnf_rule("toplevel item", toplevel_item, [
            bnf_rhs([nt(export_directive)], identity),
            bnf_rhs([nt(import_directive)], identity),
            bnf_rhs([nt(func_defn)], identity)
        ]),

        % ExportDirective := export IdentList
        %                  | export '*'
        bnf_rule("export directive", export_directive, [
            bnf_rhs([t(export), nt(export_arg)], identity_nth(2))]),
        bnf_rule("export directive", export_arg, [
            bnf_rhs([t(star)],
                const(toplevel_item(past_export(export_all)))),
            bnf_rhs([nt(ident_list)],
                det_func((pred(Nodes::in, Node::out) is semidet :-
                    Nodes = [ident_list(Names)],
                    Node = toplevel_item(past_export(export_some(Names)))
                ))
            )
        ]),

        % ImportDirective := import ident
        bnf_rule("import directive", import_directive, [
            bnf_rhs([t(import), nt(ident_list)],
                det_func((pred(Nodes::in, Node::out) is semidet :-
                    Nodes = [_, ident_list(Names)],
                    Node = toplevel_item(past_import(Names))
                ))
            )
        ]),

        % FuncDefinition := ident '(' ( Param ( , Param )* )? ')' ->
        %                       TypeExpr Using* Block
        % Param := ident : TypeExpr
        % Using := using IdentList
        %        | observing IdentList
        bnf_rule("function definition", func_defn, [
            bnf_rhs([t(func_), t(ident),
                    t(l_paren), nt(func_param_list), t(r_paren),
                    t(arrow), nt(type_expr), nt(maybe_using), nt(block)],
                det_func((pred(Nodes::in, Node::out) is semidet :-
                    Nodes = [context(Context), ident(Name, _), _,
                        param_list(Params), _, _, type_(RetType),
                        using(Using), block(Body)],
                    Node = toplevel_item(past_function(Name, Params,
                        RetType, Using, Body, Context))
                ))
            )
        ]),
        bnf_rule("parameter list", func_param_list, [
            bnf_rhs([], const(param_list([]))),
            bnf_rhs([t(ident), t(colon), nt(type_expr), nt(func_param_list)],
                det_func((pred(Nodes::in, Node::out) is semidet :-
                    Nodes = [ident(Name, _), _, type_(Type),
                        param_list(Params)],
                    Node = param_list([past_param(Name, Type) | Params])
                ))
            )
        ]),
        bnf_rule("maybe using", maybe_using, [
            bnf_rhs([], const(using([]))),
            bnf_rhs([t(using), nt(ident_list), nt(maybe_using)],
                det_func((pred(Nodes::in, Node::out) is semidet :-
                    Nodes = [_, ident_list(Resources), using(UsingB)],
                    UsingA = map((func(N) = past_using(ut_using, N)),
                        Resources),
                    Node = using(UsingA ++ UsingB)
                ))
            )
        ]),

        % TypeExpr := Type
        %           | Type '(' TypeExpr ( , TypeExpr )* ')'
        % Type := ident
        bnf_rule("type expression", type_expr, [
            bnf_rhs([nt(type_), nt(maybe_type_parameters)],
                det_func((pred(Nodes::in, Node::out) is semidet :-
                    Nodes = [ident(TypeName, Context), type_params(Params)],
                    Node = type_(past_type(TypeName, Params, Context))
                ))
            )
        ]),
        bnf_rule("type expression", maybe_type_parameters, [
            bnf_rhs([], const(type_params([]))),
            bnf_rhs([t(l_paren), nt(type_expr), nt(type_parameters),
                    t(r_paren)],
                det_func((pred(Nodes::in, Node::out) is semidet :-
                    Nodes = [_, type_(Type), type_params(Types), _],
                    Node = type_params([Type | Types])
                ))
            )
        ]),
        bnf_rule("type expression", type_parameters, [
            bnf_rhs([], const(type_params([]))),
            bnf_rhs([t(comma), nt(type_expr), nt(type_parameters)],
                det_func((pred(Nodes::in, Node::out) is semidet :-
                    Nodes = [_, type_(Type), type_params(Types)],
                    Node = type_params([Type | Types])
                ))
            )
        ]),
        bnf_rule("type", type_, [
            bnf_rhs([t(ident)], identity)
        ]),

        % Block := '{' Statement* '}'
        bnf_rule("block", block, [
            bnf_rhs([t(l_curly), nt(statement), nt(statements), t(r_curly)],
                det_func((pred(Nodes::in, Node::out) is semidet :-
                    Nodes = [_, stmt(Stmt), block(Stmts), _],
                    Node = block([Stmt | Stmts])
                ))
            )
        ]),
        bnf_rule("block", statements, [
            bnf_rhs([], const(block([]))),
            bnf_rhs([nt(statement), nt(statements)],
                det_func((pred(Nodes::in, Node::out) is semidet :-
                    Nodes = [stmt(Stmt), block(Stmts)],
                    Node = block([Stmt | Stmts])
                ))
            )
        ]),

        % Statement := '!' Statement
        %            | Expr
        bnf_rule("statement", statement, [
            bnf_rhs([t(bang), nt(statement)],
                det_func((pred(Nodes::in, Node::out) is semidet :-
                    Nodes = [_, stmt(Stmt)],
                    Node = stmt(ps_bang_statement(Stmt))
                ))
            ),
            bnf_rhs([nt(expr)],
                det_func((pred(Nodes::in, Node::out) is semidet :-
                    Nodes = [expr(Expr, Context)],
                    Node = stmt(ps_expr_statement(Expr, Context))
                ))
            )
        ]),

        % Expressions may be:
        % A value:
        %   Expr := ident
        % A constant:
        %         | const_str
        % A call:
        %         | Expr '(' Expr ( , Expr )* ')'
        %
        % Due to the syntax of calls this is left recursive and requires
        % more than 1 lookahead.  We have broken expr into two non
        % terminals, the first parses most expressions, the second parses
        % the arguments to a call and may be empty.
        %
        bnf_rule("expression", expr, [
            bnf_rhs([nt(expr_part1), nt(expr_part2)],
                det_func((pred(Nodes::in, Node::out) is semidet :-
                    ( Nodes = [expr(Expr, Context), nil],
                        Node = expr(Expr, Context)
                    ; Nodes = [expr(Expr, Context), arg_list(Exprs)],
                        Node = expr(pe_call(Expr, Exprs), Context)
                    )
                ))
            )
        ]),
        bnf_rule("expression", expr_part1, [
            bnf_rhs([t(ident)],
                det_func((pred(Nodes::in, Node::out) is semidet :-
                    Nodes = [ident(Name, Context)],
                    Node = expr(pe_symbol(symbol(Name)), Context)
                ))
            ),
            bnf_rhs([t(string)],
                det_func((pred(Nodes::in, Node::out) is semidet :-
                    Nodes = [string(String, Context)],
                    Node = expr(pe_const(pc_string(String)), Context)
                ))
            ),
            bnf_rhs([t(number)],
                det_func((pred(Nodes::in, Node::out) is semidet :-
                    Nodes = [number(Num, Context)],
                    Node = expr(pe_const(pc_number(Num)), Context)
                ))
            )
        ]),
        bnf_rule("expression", expr_part2, [
            bnf_rhs([], const(nil)),
            bnf_rhs([t(l_paren), nt(call_arg_list), t(r_paren)],
                identity_nth(2))
        ]),
        bnf_rule("argument list", call_arg_list, [
            bnf_rhs([], const(arg_list([]))),
            bnf_rhs([nt(expr), nt(call_arg_list_cont)],
                det_func((pred(Nodes::in, Node::out) is semidet :-
                    Nodes = [expr(Expr, _), arg_list(Exprs)],
                    Node = arg_list([Expr | Exprs])
                ))
            )
        ]),
        bnf_rule("argument list", call_arg_list_cont, [
            bnf_rhs([], const(arg_list([]))),
            bnf_rhs([t(comma), nt(expr), nt(call_arg_list_cont)],
                det_func((pred(Nodes::in, Node::out) is semidet :-
                    Nodes = [_, expr(Expr, _), arg_list(Exprs)],
                    Node = arg_list([Expr | Exprs])
                ))
            )
        ]),

        % IdentList := ident ( , ident )*
        bnf_rule("identifier list", ident_list, [
            bnf_rhs([t(ident), nt(ident_list_cont)],
                det_func((pred(Nodes::in, Node::out) is semidet :-
                    Nodes = [ident(X, _), ident_list(Xs)],
                    Node = ident_list([X | Xs])
                ))
            )
        ]),
        bnf_rule("identifier list", ident_list_cont, [
            bnf_rhs([], const(ident_list([]))),
            bnf_rhs([t(comma), t(ident), nt(ident_list_cont)],
                det_func((pred(Nodes::in, Node::out) is semidet :-
                    Nodes = [_, ident(X, _), ident_list(Xs)],
                    Node = ident_list([X | Xs])
                ))
            )
        ])
    ]).

:- type pt_node
    --->    module_(plasma_ast)
    ;       module_decl(string)
    ;       toplevel_items(list(past_entry))
    ;       toplevel_item(past_entry)
    ;       param_list(list(past_param))
    ;       type_(past_type)
    ;       type_params(list(past_type))
    ;       using(list(past_using))
    ;       resources(list(string))
    ;       block(list(past_statement))
    ;       stmt(past_statement)
    ;       expr(past_expression, context)
    ;       arg_list(list(past_expression))
    ;       ident(string, context)
    ;       ident_list(list(string))
    ;       number(int, context)
    ;       string(string, context)
    ;       context(context)
    ;       nil.

:- instance token_to_result(token_type, pt_node) where [
        token_to_result(Type, MaybeString, Context) =
            ( Type = ident, MaybeString = yes(String) ->
                ident(String, Context)
            ; Type = string, MaybeString = yes(String) ->
                % TODO: handle escape sequences.
                string(String, Context)
            ; Type = number, MaybeString = yes(String) ->
                number(det_to_int(String), Context)
            ; Type = func_ ->
                context(Context)
            ;
                nil
            )
    ].

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
