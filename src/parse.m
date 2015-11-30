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

:- import_module result.

:- import_module ast.

%-----------------------------------------------------------------------%

:- type plasma_parse_error.

:- instance error(plasma_parse_error).

%-----------------------------------------------------------------------%

:- pred parse(string::in, result(plasma_ast, plasma_parse_error)::out,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

:- import_module cord.
:- import_module int.
:- import_module list.
:- import_module maybe.
:- import_module require.
:- import_module unit.

:- import_module lex.
:- import_module parsing.
:- import_module parsing.bnf.
:- import_module parsing.gen.
:- import_module symtab.

:- import_module ast.
:- import_module context.

%-----------------------------------------------------------------------%

:- type plasma_parse_error
    --->    ppe_io_error(string)
    ;       ppe_tokeniser_error(string)
    ;       ppe_parse_unexpected_eof(list(string))
    ;       ppe_parse_unexpected_token(list(string), string)
    ;       ppe_parse_junk_at_end(string).

:- instance error(plasma_parse_error) where [
    func(error_or_warning/1) is ppe_error_or_warning,
    func(to_string/1) is ppe_to_string
].

%-----------------------------------------------------------------------%

parse(Filename, Result, !IO) :-
    io.open_input(Filename, OpenResult, !IO),
    ( OpenResult = ok(File),
        Lexer = lex.init(lexemes, lex.read_from_string, ignore_tokens),
        tokenize(File, Lexer, Filename, 1, [], TokensResult, !IO),
        io.close_input(File, !IO),
        ( TokensResult = ok(Tokens),
            parse_tokens(Tokens, Result)
        ; TokensResult = errors(Errors),
            Result = errors(Errors)
        )
    ; OpenResult = error(IOError),
        Result = return_error(context(Filename, 0),
            ppe_io_error(error_message(IOError)))
    ).

%-----------------------------------------------------------------------%

:- pred tokenize(text_input_stream::in, lexer(token, string)::in,
    string::in, int::in,
    list(token(token_type))::in,
    result(list(token(token_type)), plasma_parse_error)::out,
    io::di, io::uo) is det.

tokenize(File, Lexer, Filename, Line, RevTokens0, MaybeTokens, !IO) :-
    Context = context(Filename, Line),
    io.read_line_as_string(File, ReadResult, !IO),
    ( ReadResult = ok(String0),
        copy(String0 ++ "\n", String),
        LS0 = lex.start(Lexer, String),
        tokenize_line(Context, [], MaybeTokens0, LS0, LS),
        _ = lex.stop(LS),
        ( MaybeTokens0 = ok(NewTokens),
            RevTokens = NewTokens ++ RevTokens0,
            tokenize(File, Lexer, Filename, Line+1, RevTokens, MaybeTokens, !IO)
        ; MaybeTokens0 = errors(_),
            MaybeTokens = MaybeTokens0
        )
    ; ReadResult = eof,
        MaybeTokens = ok(reverse(RevTokens0))
    ; ReadResult = error(IOError),
        MaybeTokens = return_error(Context,
            ppe_io_error(error_message(IOError)))
    ).

:- pred tokenize_line(context::in, list(token(token_type))::in,
    result(list(token(token_type)), plasma_parse_error)::out,
    lexer_state(token, string)::di,
    lexer_state(token, string)::uo) is det.

tokenize_line(Context, RevTokens, MaybeTokens, !LS) :-
    lex.read(MaybeToken, !LS),
    ( MaybeToken = ok(token(Token, MaybeString)),
        TAC = token(Token, MaybeString, Context),
        tokenize_line(Context, [TAC | RevTokens], MaybeTokens, !LS)
    ; MaybeToken = eof,
        MaybeTokens = ok(RevTokens)
    ; MaybeToken = error(Message, _Line),
        MaybeTokens = return_error(Context, ppe_tokeniser_error(Message))
    ).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- type token
    --->    token(token_type, maybe(string)).

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

:- func lexemes = list(lexeme(token)).

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
        (lex.identifier     -> (func(S) = token(ident, yes(S)))),
        % TODO: escapes
        ("\"" ++ *(anybut("\"")) ++ "\""
                            -> (func(S0) = token(string, yes(S)) :-
                                    between(S0, 1, length(S0) - 1, S))),

        (("#" ++ *(anybut("\n")))
                            -> return_simple(comment)),
        ("\n"               -> return_simple(newline)),
        (any(" \t\v\f")     -> return_simple(whitespace))
    ].

:- func return_simple(token_type) = token_creator(token).

return_simple(T) = lex.return(token(T, no)).

:- pred ignore_tokens(token::in) is semidet.

ignore_tokens(token(whitespace, _)).
ignore_tokens(token(newline, _)).
ignore_tokens(token(comment, _)).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

    % Plasma := ModuleDecl Item*
    %
:- pred parse_tokens(list(token(token_type))::in,
    result(plasma_ast, plasma_parse_error)::out) is det.

parse_tokens(!.Tokens, Result) :-
    parsing.parse(make_parser(plasma_bnf), !.Tokens, Result0),
    ( Result0 = ok(PTNode),
        ( PTNode = module_(AST) ->
            Result = ok(AST)
        ;
            unexpected($file, $pred, "Result of parsing isn't a module")
        )
    ; Result0 = errors(Errors0),
        Errors = errors_map(parse_error_to_plasma_parse_error, Errors0),
        Result = errors(Errors)
    ).

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

:- func const(A, B) = A.

const(A, _) = A.

:- func identity(list(T)) = T.

identity(Nodes) =
    ( Nodes = [Node] ->
        Node
    ;
        unexpected($file, $pred, string(Nodes))
    ).

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

:- func ppe_error_or_warning(plasma_parse_error) = error_or_warning.

ppe_error_or_warning(ppe_io_error(_)) = error.
ppe_error_or_warning(ppe_tokeniser_error(_)) = error.
ppe_error_or_warning(ppe_parse_unexpected_eof(_)) = error.
ppe_error_or_warning(ppe_parse_unexpected_token(_, _)) = error.
ppe_error_or_warning(ppe_parse_junk_at_end(_)) = error.

:- func ppe_to_string(plasma_parse_error) = string.

ppe_to_string(ppe_io_error(Message)) = Message.
ppe_to_string(ppe_tokeniser_error(Message)) =
    format("Tokenizer error, %s", [s(Message)]).
ppe_to_string(ppe_parse_unexpected_eof(Expected)) =
    format("Unexpected EOF, expected %s", [s(str_list_or(Expected))]).
ppe_to_string(ppe_parse_unexpected_token(Expected, Got)) =
    format("Parse error, read %s expected %s", [s(Got),
        s(str_list_or(Expected))]).
ppe_to_string(ppe_parse_junk_at_end(Got)) =
    format("Parse error: junk at end of input: %s", [s(Got)]).

:- func str_list_or(list(string)) = string.

str_list_or(Strs) = join_list(", ", Strs).

:- func parse_error_to_plasma_parse_error(parse_error(token_type)) =
    plasma_parse_error.

parse_error_to_plasma_parse_error(pe_unexpected_eof(Tokens)) =
        ppe_parse_unexpected_eof(Strs) :-
    Strs = map(string, Tokens).
parse_error_to_plasma_parse_error(pe_unexpected_token(ExpTokens, GotToken)) =
    ppe_parse_unexpected_token(map(string, ExpTokens), string(GotToken)).
parse_error_to_plasma_parse_error(pe_junk_at_end(Token)) =
    ppe_parse_junk_at_end(string(Token)).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
