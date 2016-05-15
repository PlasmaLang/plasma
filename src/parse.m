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
    ;       type_
    ;       func_
    ;       using
    ;       observing
    ;       as
    ;       return
    ;       ident_lower
    ;       ident_upper
    ;       number
    ;       string
    ;       l_curly
    ;       r_curly
    ;       l_paren
    ;       r_paren
    ;       semicolon
    ;       colon
    ;       d_colon
    ;       comma
    ;       period
    ;       plus
    ;       minus
    ;       star
    ;       slash
    ;       percent
    ;       amp
    ;       bar
    ;       caret
    ;       tilda
    ;       bang
    ;       doublelangle
    ;       doublerangle
    ;       doubleplus
    ;       equals
    ;       arrow
    ;       newline
    ;       comment
    ;       whitespace
    ;       eof.

:- func lexemes = list(lexeme(lex_token(token_type))).

lexemes = [
        ("module"           -> return_simple(module_)),
        ("export"           -> return_simple(export)),
        ("import"           -> return_simple(import)),
        ("type"             -> return_simple(type_)),
        ("func"             -> return_simple(func_)),
        ("using"            -> return_simple(using)),
        ("observing"        -> return_simple(observing)),
        ("as"               -> return_simple(as)),
        ("return"           -> return_simple(return)),
        ("{"                -> return_simple(l_curly)),
        ("}"                -> return_simple(r_curly)),
        ("("                -> return_simple(l_paren)),
        (")"                -> return_simple(r_paren)),
        (";"                -> return_simple(semicolon)),
        (":"                -> return_simple(colon)),
        ("::"               -> return_simple(d_colon)),
        (","                -> return_simple(comma)),
        ("."                -> return_simple(period)),
        ("+"                -> return_simple(plus)),
        ("-"                -> return_simple(minus)),
        ("*"                -> return_simple(star)),
        ("/"                -> return_simple(slash)),
        ("%"                -> return_simple(percent)),
        ("&"                -> return_simple(amp)),
        ("|"                -> return_simple(bar)),
        ("^"                -> return_simple(caret)),
        ("~"                -> return_simple(tilda)),
        ("!"                -> return_simple(bang)),
        (">>"               -> return_simple(doublelangle)),
        ("<<"               -> return_simple(doublerangle)),
        ("++"               -> return_simple(doubleplus)),
        ("="                -> return_simple(equals)),
        ("->"               -> return_simple(arrow)),
        (signed_int         -> return_string(number)),
        (identifier_lower   -> return_string(ident_lower)),
        (identifier_upper   -> return_string(ident_upper)),
        % TODO: escapes
        ("\"" ++ *(anybut("\"")) ++ "\""
                            -> (func(S0) = lex_token(string, yes(S)) :-
                                    between(S0, 1, length(S0) - 1, S))),

        (("#" ++ *(anybut("\n")))
                            -> return_simple(comment)),
        ("\n"               -> return_simple(newline)),
        (any(" \t\v\f")     -> return_simple(whitespace))
    ].

:- func identifier_lower = regexp.

identifier_lower = any("abcdefghijklmnopqrstuvwxyz_") ++ *(ident).

:- func identifier_upper = regexp.

identifier_upper = (any("ABCDEFGHIJKLMNOPQRSTUVWXYZ") or ('_')) ++ *(ident).

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
    ;       import_name_cont
    ;       import_name_cont_2
    ;       import_as

    ;       func_defn
    ;       func_param_list
    ;       func_param_list_cont
    ;       maybe_using

    ;       block
    ;       statements
    ;       statement
    ;       bang_statement_cont

    ;       tuple_expr
    ;       tuple_expr_part2
    ;       expr
    ;       expr_part2
    ;       expr1
    ;       expr1_part2
    ;       expr2
    ;       expr2_part2
    ;       expr3
    ;       expr3_part2
    ;       expr4
    ;       expr4_part2
    ;       expr5
    ;       expr5_part2
    ;       expr6
    ;       expr6_part2
    ;       expr7
    ;       expr8
    ;       expr8_part2

    ;       call_args
    ;       call_arg_list
    ;       call_arg_list_cont

    ;       type_defn
    ;       type_defn_lhs
    ;       maybe_type_defn_lhs_params
    ;       type_defn_body_cont
    ;       type_constructor
    ;       type_constr_maybe_params
    ;       type_constr_params_cont
    ;       type_constr_param
    ;       type_expr
    ;       type_name
    ;       type_var
    ;       type_symbol
    ;       type_symbol_cont
    ;       maybe_type_parameters
    ;       type_parameters

    ;       ident
    ;       ident_list
    ;       ident_list_cont
    ;       lower_ident_list
    ;       lower_ident_list_cont
    ;       ident_dlist
    ;       ident_dlist_cont.

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
            bnf_rhs([t(module_), nt(ident)],
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
            bnf_rhs([nt(func_defn)], identity),
            bnf_rhs([nt(type_defn)], identity)
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
                    Nodes = [ident_list(Names, _)],
                    Node = toplevel_item(past_export(export_some(Names)))
                ))
            )
        ]),

        % ImportDirective := import QualifiedIdent
        %                  | import QualifiedIdent . *
        %                  | import QualifiedIdent as ident
        %
        % To aide parsing without lookahead we also accept, but discard
        % later:
        %                  | import QualifiedIdent . * as ident
        %
        bnf_rule("import directive", import_directive, [
            bnf_rhs([t(import), nt(ident), nt(import_name_cont), nt(import_as)],
                det_func((pred(Nodes::in, Node::out) is semidet :-
                    Nodes = [_, ident(Name, _), import_name(Names), As0],
                    ( if As0 = nil then
                        As = no
                    else
                        As0 = ident(AsName, _),
                        As = yes(AsName)
                    ),
                    Node = toplevel_item(past_import(dot(Name, Names), As))
                ))
            )
        ]),
        bnf_rule("import directive", import_name_cont, [
            bnf_rhs([], const(import_name(nil))),
            bnf_rhs([t(period), nt(import_name_cont_2)],
                identity_nth(2)
            )
        ]),
        bnf_rule("import directive", import_name_cont_2, [
            bnf_rhs([nt(ident), nt(import_name_cont)],
                det_func((pred(Nodes::in, Node::out) is semidet :-
                    Nodes = [ident(Name, _), import_name(Names)],
                    Node = import_name(dot(Name, Names))
                ))
            ),
            bnf_rhs([t(star)],
                const(import_name(star)))
        ]),
        bnf_rule("import directive", import_as, [
            bnf_rhs([], const(nil)),
            bnf_rhs([t(as), nt(ident)], identity_nth(2))
        ]),

        % TypeDefn
        bnf_rule("type definition", type_defn, [
            bnf_rhs([t(type_), nt(type_defn_lhs), t(equals),
                    nt(type_constructor), nt(type_defn_body_cont)],
                det_func((pred(Nodes::in, Node::out) is semidet :-
                    Nodes = [_, type_defn_lhs(Name, Params, Context), _,
                        constructor(Const), constructors(Consts)],
                    Node = toplevel_item(past_type(Name, Params,
                        [Const | Consts], Context))
                ))
            )
        ]),
        bnf_rule("type definition", type_defn_lhs, [
            bnf_rhs([nt(type_name), nt(maybe_type_defn_lhs_params)],
                det_func((pred(Nodes::in, Node::out) is semidet :-
                    Nodes = [ident(Name, Context), ident_list(Args, _)],
                    Node = type_defn_lhs(Name, Args, Context)
                ))
            )
        ]),
        bnf_rule("type definition", maybe_type_defn_lhs_params, [
            bnf_rhs([], const(ident_list([], nil_context))),
            % Note, case is not enforced here.
            bnf_rhs([t(l_paren), nt(lower_ident_list), t(r_paren)],
                identity_nth(2))
        ]),
        bnf_rule("type definition", type_defn_body_cont, [
            bnf_rhs([], const(constructors([]))),
            bnf_rhs([t(bar), nt(type_constructor), nt(type_defn_body_cont)],
                det_func((pred(Nodes::in, Node::out) is semidet :-
                    Nodes = [_, constructor(Const), constructors(Consts)],
                    Node = constructors([Const | Consts])
                ))
            )
        ]),

        bnf_rule("type constroctor", type_constructor, [
            bnf_rhs([t(ident_upper), nt(type_constr_maybe_params)],
                det_func((pred(Nodes::in, Node::out) is semidet :-
                    Nodes = [ident(Name, Context), fields(Fields)],
                    Node = constructor(pat_constructor(Name, Fields,
                        Context))
                ))
            )
        ]),
        bnf_rule("type constructor", type_constr_maybe_params, [
            bnf_rhs([], const(fields([]))),
            bnf_rhs([t(l_paren), nt(type_constr_param),
                    nt(type_constr_params_cont), t(r_paren)],
                det_func((pred(Nodes::in, Node::out) is semidet :-
                    Nodes = [_, field(Field), fields(Fields), _],
                    Node = fields([Field | Fields])
                ))
            )
        ]),
        bnf_rule("type constructor", type_constr_params_cont, [
            bnf_rhs([], const(fields([]))),
            bnf_rhs([t(comma), nt(type_constr_param),
                    nt(type_constr_params_cont)],
                det_func((pred(Nodes::in, Node::out) is semidet :-
                    Nodes = [_, field(Field), fields(Fields)],
                    Node = fields([Field | Fields])
                ))
            )
        ]),
        bnf_rule("type constructor", type_constr_param, [
            bnf_rhs([nt(ident), t(d_colon), nt(type_expr)],
                det_func((pred(Nodes::in, Node::out) is semidet :-
                    Nodes = [ident(Name, Context), _, type_expr(TypeExpr)],
                    Node = field(pat_field(Name, TypeExpr, Context))
                ))
            )
        ]),

        % TypeExpr := Type
        %           | Type '(' TypeExpr ( , TypeExpr )* ')'
        % Type := QualifiedIden
        bnf_rule("type expression", type_expr, [
            bnf_rhs([nt(type_var)],
                det_func((pred(Nodes::in, Node::out) is semidet :-
                    Nodes = [ident(TypeVar, Context)],
                    Node = type_expr(past_type_var(TypeVar, Context))
                ))
            ),
            bnf_rhs([nt(type_symbol), nt(maybe_type_parameters)],
                det_func((pred(Nodes::in, Node::out) is semidet :-
                    Nodes = [ident_list(TypeQName, Context),
                        type_expr_params(Params)],
                    split_last(TypeQName, Qualifiers, Name),
                    Node = type_expr(past_type(Qualifiers, Name, Params,
                        Context))
                ))
            )
        ]),
        bnf_rule("type expression", maybe_type_parameters, [
            bnf_rhs([], const(type_expr_params([]))),
            bnf_rhs([t(l_paren), nt(type_expr), nt(type_parameters),
                    t(r_paren)],
                det_func((pred(Nodes::in, Node::out) is semidet :-
                    Nodes = [_, type_expr(Type), type_expr_params(Types), _],
                    Node = type_expr_params([Type | Types])
                ))
            )
        ]),
        bnf_rule("type expression", type_parameters, [
            bnf_rhs([], const(type_expr_params([]))),
            bnf_rhs([t(comma), nt(type_expr), nt(type_parameters)],
                det_func((pred(Nodes::in, Node::out) is semidet :-
                    Nodes = [_, type_expr(Type), type_expr_params(Types)],
                    Node = type_expr_params([Type | Types])
                ))
            )
        ]),
        bnf_rule("type name", type_name, [
            bnf_rhs([t(ident_upper)], identity)
        ]),
        bnf_rule("type var", type_var, [
            bnf_rhs([t(ident_lower)], identity)
        ]),
        bnf_rule("type symbol", type_symbol, [
            % Note: only enforces case for the first item.
            bnf_rhs([t(ident_upper), nt(ident_dlist_cont)],
                det_func((pred(Nodes::in, Node::out) is semidet :-
                    Nodes = [ident(Head, Context), ident_list(Tail, _)],
                    Node = ident_list([Head | Tail], Context)
                ))
            )
        ]),

        % FuncDefinition := ident '(' ( Param ( , Param )* )? ')' ->
        %                       TypeExpr Using* Block
        % Param := ident : TypeExpr
        % Using := using IdentList
        %        | observing IdentList
        bnf_rule("function definition", func_defn, [
            bnf_rhs([t(func_), nt(ident),
                    t(l_paren), nt(func_param_list), t(r_paren),
                    t(arrow), nt(type_expr), nt(maybe_using), nt(block)],
                det_func((pred(Nodes::in, Node::out) is semidet :-
                    Nodes = [context(Context), ident(Name, _), _,
                        param_list(Params), _, _, type_expr(RetType),
                        using(Using), block(Body)],
                    Node = toplevel_item(past_function(Name, Params,
                        RetType, Using, Body, Context))
                ))
            )
        ]),
        bnf_rule("parameter list", func_param_list, [
            bnf_rhs([], const(param_list([]))),
            bnf_rhs([nt(ident), t(d_colon), nt(type_expr),
                    nt(func_param_list_cont)],
                det_func((pred(Nodes::in, Node::out) is semidet :-
                    Nodes = [ident(Name, _), _, type_expr(Type),
                        param_list(Params)],
                    Node = param_list([past_param(Name, Type) | Params])
                ))
            )
        ]),
        bnf_rule("parameter list", func_param_list_cont, [
            bnf_rhs([], const(param_list([]))),
            bnf_rhs([t(comma), nt(ident), t(d_colon), nt(type_expr),
                    nt(func_param_list_cont)],
                det_func((pred(Nodes::in, Node::out) is semidet :-
                    Nodes = [_, ident(Name, _), _, type_expr(Type),
                        param_list(Params)],
                    Node = param_list([past_param(Name, Type) | Params])
                ))
            )
        ]),
        bnf_rule("maybe using", maybe_using, [
            bnf_rhs([], const(using([]))),
            bnf_rhs([t(using), nt(ident_list), nt(maybe_using)],
                det_func((pred(Nodes::in, Node::out) is semidet :-
                    Nodes = [_, ident_list(Resources, _), using(UsingB)],
                    UsingA = map((func(N) = past_using(ut_using, N)),
                        Resources),
                    Node = using(UsingA ++ UsingB)
                ))
            ),
            bnf_rhs([t(observing), nt(ident_list), nt(maybe_using)],
                det_func((pred(Nodes::in, Node::out) is semidet :-
                    Nodes = [_, ident_list(Resources, _), using(UsingB)],
                    UsingA = map((func(N) = past_using(ut_observing, N)),
                        Resources),
                    Node = using(UsingA ++ UsingB)
                ))
            )
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
            bnf_rhs([t(bang), nt(ident), nt(bang_statement_cont)],
                det_func((pred(Nodes::in, Node::out) is semidet :-
                    Nodes = [_, ident(Ident, Context), bang_stmt(BangCont)],
                    Node = stmt(BangCont(Context, Ident))
                ))
            ),
            bnf_rhs([t(return), nt(tuple_expr)],
                det_func((pred(Nodes::in, Node::out) is semidet :-
                    Nodes = [_, ExprNode],
                    ( ExprNode = expr(Expr, Context),
                        Node = stmt(ps_return_statement([Expr], Context))
                    ; ExprNode = expr_list(Exprs, Context),
                        Node = stmt(ps_return_statement(Exprs, Context))
                    )
                ))
            ),
            bnf_rhs([nt(ident_list), t(equals), nt(tuple_expr)],
                det_func((pred(Nodes::in, Node::out) is semidet :-
                    Nodes = [ident_list(Vars, Context), _, ExprNode],
                    ( ExprNode = expr(Expr, _),
                        Exprs = [Expr]
                    ; ExprNode = expr_list(Exprs, _)
                    ),
                    Node = stmt(ps_asign_statement(Vars, Exprs, Context))
                ))
            )
        ]),
        bnf_rule("statement", bang_statement_cont, [
            bnf_rhs([nt(call_args)],
                det_func((pred(Nodes::in, Node::out) is semidet :-
                    Nodes = [arg_list(Args)],
                    Node = bang_stmt(func(Context, Callee) =
                            ps_bang_call(past_call(symbol(Callee), Args),
                                Context)
                        )
                ))
            ),
            bnf_rhs([t(period), nt(ident_dlist), nt(call_args)],
                det_func((pred(Nodes::in, Node::out) is semidet :-
                    Nodes = [_, ident_list(IdentList, _), arg_list(Args)],
                    Node = bang_stmt((func(Context, IdentHead) = Node0 :-
                            Idents = [IdentHead | IdentList],
                            det_split_last(Idents, Qualifiers, Name),
                            Callee = symbol(Qualifiers, Name),
                            Node0 = ps_bang_call(past_call(Callee, Args),
                                Context)
                        ))
                ))
            ),
            bnf_rhs([t(equals), nt(ident_dlist), nt(call_args)],
                det_func((pred(Nodes::in, Node::out) is semidet :-
                    Nodes = [_, ident_list(CalleeIdents, _), arg_list(Args)],
                    det_split_last(CalleeIdents, Qualifiers, Name),
                    Callee = symbol(Qualifiers, Name),
                    Node = bang_stmt((func(Context, Var) =
                            ps_bang_asign_call([Var], past_call(Callee, Args),
                                Context)
                        ))
                ))
            ),
            bnf_rhs([t(comma), nt(ident_list), t(equals), nt(ident_dlist),
                    nt(call_args)],
                det_func((pred(Nodes::in, Node::out) is semidet :-
                    Nodes = [_, ident_list(Vars, _), _,
                        ident_list(CalleeIdents, _), arg_list(Args)],
                    det_split_last(CalleeIdents, Qualifiers, Name),
                    Callee = symbol(Qualifiers, Name),
                    Node = bang_stmt((func(Context, Var) =
                            ps_bang_asign_call([Var | Vars],
                                past_call(Callee, Args),
                                Context)
                        ))
                ))
            )
        ]),

        % Expressions may be:
        % A value:
        %   Expr := ent
        % A call:
        %         | ident '(' Expr ( , Expr )* ')'
        % A constant:
        %         | const_str
        %
        bnf_rule("expression", tuple_expr, [
            bnf_rhs([nt(expr), nt(tuple_expr_part2)],
                det_func((pred(Nodes::in, Node::out) is semidet :-
                    Nodes = [expr(Expr, C), expr_list(Exprs, _)],
                    ( Exprs = [],
                        Node = expr(Expr, C)
                    ; Exprs = [_ | _],
                        Node = expr_list([Expr | Exprs], C)
                    )
                ))
            )
        ]),
        bnf_rule("expression", tuple_expr_part2, [
            bnf_rhs([], const(expr_list([], nil_context))),
            bnf_rhs([t(comma), nt(expr), nt(tuple_expr_part2)],
                det_func((pred(Nodes::in, Node::out) is semidet :-
                    Nodes = [_, expr(Expr, C), expr_list(Exprs, _)],
                    Node = expr_list([Expr | Exprs], C)
                ))
            )
        ]),
        bnf_rule("expression", expr, [
            bnf_rhs([nt(expr1), nt(expr_part2)], build_expr_part1)
        ]),
        bnf_rule("expression", expr_part2, [
            bnf_rhs([], const(nil)),
            bnf_rhs([t(doubleplus), nt(expr)],
                build_expr_part2(pb_concat))
        ]),
        bnf_rule("expression", expr1, [
            bnf_rhs([nt(expr2), nt(expr1_part2)], build_expr_part1)
        ]),
        bnf_rule("expression", expr1_part2, [
            bnf_rhs([], const(nil)),
            bnf_rhs([t(bar), nt(expr1)], build_expr_part2(pb_or))
        ]),
        bnf_rule("expression", expr2, [
            bnf_rhs([nt(expr3), nt(expr2_part2)], build_expr_part1)
        ]),
        bnf_rule("expression", expr2_part2, [
            bnf_rhs([], const(nil)),
            bnf_rhs([t(caret), nt(expr2)], build_expr_part2(pb_xor))
        ]),
        bnf_rule("expression", expr3, [
            bnf_rhs([nt(expr4), nt(expr3_part2)], build_expr_part1)
        ]),
        bnf_rule("expression", expr3_part2, [
            bnf_rhs([], const(nil)),
            bnf_rhs([t(amp), nt(expr3)], build_expr_part2(pb_and))
        ]),
        bnf_rule("expression", expr4, [
            bnf_rhs([nt(expr5), nt(expr4_part2)], build_expr_part1)
        ]),
        bnf_rule("expression", expr4_part2, [
            bnf_rhs([], const(nil)),
            bnf_rhs([t(doublelangle), nt(expr4)],
                build_expr_part2(pb_lshift)),
            bnf_rhs([t(doublerangle), nt(expr4)],
                build_expr_part2(pb_rshift))
        ]),
        bnf_rule("expression", expr5, [
            bnf_rhs([nt(expr6), nt(expr5_part2)], build_expr_part1)
        ]),
        bnf_rule("expression", expr5_part2, [
            bnf_rhs([], const(nil)),
            bnf_rhs([t(plus), nt(expr5)], build_expr_part2(pb_add)),
            bnf_rhs([t(minus), nt(expr5)], build_expr_part2(pb_sub))
        ]),
        bnf_rule("expression", expr6, [
            bnf_rhs([nt(expr7), nt(expr6_part2)], build_expr_part1)
        ]),
        bnf_rule("expression", expr6_part2, [
            bnf_rhs([], const(nil)),
            bnf_rhs([t(star), nt(expr6)], build_expr_part2(pb_mul)),
            bnf_rhs([t(slash), nt(expr6)], build_expr_part2(pb_div)),
            bnf_rhs([t(percent), nt(expr6)], build_expr_part2(pb_mod))
        ]),
        bnf_rule("expression", expr7, [
            bnf_rhs([t(minus), nt(expr7)],
                det_func((pred(Nodes::in, Node::out) is semidet :-
                    Nodes = [_, expr(Expr, C)],
                    Node = expr(pe_u_op(pu_minus, Expr), C)
                ))
            ),
            bnf_rhs([t(tilda), nt(expr7)],
                det_func((pred(Nodes::in, Node::out) is semidet :-
                    Nodes = [_, expr(Expr, C)],
                    Node = expr(pe_u_op(pu_not, Expr), C)
                ))
            ),
            bnf_rhs([nt(expr8)], identity)
        ]),
        bnf_rule("expression", expr8, [
            bnf_rhs([t(l_paren), nt(expr), t(r_paren)],
                identity_nth(2)),
            bnf_rhs([nt(ident_dlist), nt(expr8_part2)],
                det_func((pred(Nodes::in, Node::out) is semidet :-
                    Nodes = [ident_list(QName, Context), SecondNode],
                    split_last(QName, Qualifiers, Name),
                    Symbol = symbol(Qualifiers, Name),
                    ( SecondNode = nil,
                        Node = expr(pe_symbol(Symbol), Context)
                    ; SecondNode = arg_list(Exprs),
                        Node = expr(pe_call(past_call(Symbol, Exprs)), Context)
                    )
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
        bnf_rule("expression", expr8_part2, [
            bnf_rhs([], const(nil)),
            bnf_rhs([nt(call_args)], identity)
        ]),

        % The call rules are shared by both expression and statement rules.
        bnf_rule("call", call_args, [
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

        bnf_rule("Identifier", ident, [
            bnf_rhs([t(ident_lower)], identity),
            bnf_rhs([t(ident_upper)], identity)
        ]),
        % IdentList := ident ( , ident )*
        bnf_rule("identifier list", ident_list, [
            bnf_rhs([nt(ident), nt(ident_list_cont)],
                det_func((pred(Nodes::in, Node::out) is semidet :-
                    Nodes = [ident(X, C), ident_list(Xs, _)],
                    Node = ident_list([X | Xs], C)
                ))
            )
        ]),
        bnf_rule("identifier list", ident_list_cont, [
            bnf_rhs([], const(ident_list([], nil_context))),
            bnf_rhs([t(comma), nt(ident), nt(ident_list_cont)],
                det_func((pred(Nodes::in, Node::out) is semidet :-
                    Nodes = [_, ident(X, C), ident_list(Xs, _)],
                    Node = ident_list([X | Xs], C)
                ))
            )
        ]),
        bnf_rule("identifier list", lower_ident_list, [
            bnf_rhs([t(ident_lower), nt(lower_ident_list_cont)],
                det_func((pred(Nodes::in, Node::out) is semidet :-
                    Nodes = [ident(X, C), ident_list(Xs, _)],
                    Node = ident_list([X | Xs], C)
                ))
            )
        ]),
        bnf_rule("identifier list", lower_ident_list_cont, [
            bnf_rhs([], const(ident_list([], nil_context))),
            bnf_rhs([t(comma), t(ident_lower), nt(lower_ident_list_cont)],
                det_func((pred(Nodes::in, Node::out) is semidet :-
                    Nodes = [_, ident(X, C), ident_list(Xs, _)],
                    Node = ident_list([X | Xs], C)
                ))
            )
        ]),
        % IdentDList := ident ( . ident )*
        bnf_rule("qualified identifier", ident_dlist, [
            bnf_rhs([nt(ident), nt(ident_dlist_cont)],
                det_func((pred(Nodes::in, Node::out) is semidet :-
                    Nodes = [ident(X, C), ident_list(Xs, _)],
                    Node = ident_list([X | Xs], C)
                ))
            )
        ]),
        bnf_rule("qualified identifier", ident_dlist_cont, [
            bnf_rhs([], const(ident_list([], nil_context))),
            bnf_rhs([t(period), nt(ident), nt(ident_dlist_cont)],
                det_func((pred(Nodes::in, Node::out) is semidet :-
                    Nodes = [_, ident(X, C), ident_list(Xs, _)],
                    Node = ident_list([X | Xs], C)
                ))
            )
        ])
    ]).

:- type pt_node
    --->    module_(plasma_ast)
    ;       module_decl(string)
    ;       toplevel_items(list(past_entry))
    ;       toplevel_item(past_entry)
    ;       import_name(ast.import_name_2)
    ;       param_list(list(past_param))
    ;       type_expr(past_type_expr)
    ;       type_expr_params(list(past_type_expr))
    ;       type_defn_lhs(string, list(string), context)
    ;       constructor(pat_constructor)
    ;       constructors(list(pat_constructor))
    ;       field(pat_field)
    ;       fields(list(pat_field))
    ;       using(list(past_using))
    ;       resources(list(string))
    ;       block(list(past_statement))
    ;       stmt(past_statement)
    ;       bang_stmt(func(context, string) = past_statement)
    ;       expr(past_expression, context)
    ;       expr_part(past_bop, past_expression)
    ;       expr_list(list(past_expression), context)
    ;       op(past_bop)
    ;       arg_list(list(past_expression))
    ;       ident(string, context)
    ;       ident_list(list(string), context)
    ;       number(int, context)
    ;       string(string, context)
    ;       context(context)
    ;       nil.

:- func build_expr_part1(list(pt_node)) = maybe(pt_node).

build_expr_part1(Nodes) = MaybeNode :-
    ( if
        Nodes = [expr(ExprL, C), Part2],
        ( Part2 = nil,
            Node = expr(ExprL, C)
        ; Part2 = expr_part(Op, ExprR),
            Node = expr(pe_b_op(ExprL, Op, ExprR), C)
        )
    then
        MaybeNode = yes(Node)
    else
        MaybeNode = no
    ).

:- func build_expr_part2(past_bop, list(pt_node)) = maybe(pt_node).

build_expr_part2(Op, Nodes) = MaybeNode :-
    ( if
        Nodes = [_, expr(Expr, _)],
        Node = expr_part(Op, Expr)
    then
        MaybeNode = yes(Node)
    else
        MaybeNode = no
    ).

:- instance token_to_result(token_type, pt_node) where [
        token_to_result(Type, MaybeString, Context) =
            (
                ( Type = ident_lower
                ; Type = ident_upper
                ),
                MaybeString = yes(String)
            ->
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
