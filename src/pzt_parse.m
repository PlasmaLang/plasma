%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module pzt_parse.
%
% Parse the PZ textual representation.
%
% Copyright (C) 2015 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
%-----------------------------------------------------------------------%

:- interface.

%-----------------------------------------------------------------------%

:- import_module io.
:- import_module string.

:- import_module asm_ast.
:- import_module asm_error.
:- import_module result.

:- pred parse(string::in, result(asm, asm_error)::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

:- import_module cord.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module require.

:- import_module context.
:- import_module lex_util.
:- import_module parsing.
:- import_module parsing.bnf.
:- import_module parsing.gen.
:- import_module pz.
:- import_module pz.code.
:- import_module lex.
:- import_module symtab.

%-----------------------------------------------------------------------%

parse(Filename, Result, !IO) :-
    parse_file(Filename, lexemes, ignore_tokens, pz_bnf, Result0, !IO),
    ( Result0 = ok(PZNode),
        ( PZNode = pzt(AST) ->
            Result = ok(AST)
        ;
            unexpected($file, $pred, "Wrong node type")
        )
    ; Result0 = errors(Errors),
        Result = errors(map(
            (func(error(C, E)) = error(C, e_read_src_error(E))), Errors))
    ).

%-----------------------------------------------------------------------%

:- type pzt_token == token(token_basic).

:- type token_basic
    --->    proc
    ;       block
    ;       data
    ;       array
    ;       cjmp
    ;       roll
    ;       pick
    ;       w ; w8 ; w16 ; w32 ; w64 ; w_ptr ; ptr
    ;       open_curly
    ;       close_curly
    ;       open_paren
    ;       close_paren
    ;       dash
    ;       equals
    ;       semicolon
    ;       colon
    ;       comma
    ;       period
    ;       identifier
    ;       number
    ;       comment
    ;       whitespace
    ;       eof.

:- func lexemes = list(lexeme(lex_token(token_basic))).

lexemes = [
        ("proc"             -> return(proc)),
        ("block"            -> return(block)),
        ("data"             -> return(data)),
        ("array"            -> return(array)),
        ("cjmp"             -> return(cjmp)),
        ("roll"             -> return(roll)),
        ("pick"             -> return(pick)),
        ("w"                -> return(w)),
        ("w8"               -> return(w8)),
        ("w16"              -> return(w16)),
        ("w32"              -> return(w32)),
        ("w64"              -> return(w64)),
        ("w_ptr"            -> return(w_ptr)),
        ("ptr"              -> return(ptr)),
        ("{"                -> return(open_curly)),
        ("}"                -> return(close_curly)),
        ("("                -> return(open_paren)),
        (")"                -> return(close_paren)),
        ("-"                -> return(dash)),
        ("="                -> return(equals)),
        (","                -> return(comma)),
        ("."                -> return(period)),
        (";"                -> return(semicolon)),
        (":"                -> return(colon)),
        (lex.identifier     -> return(identifier)),
        (?("-") ++ lex.nat  -> return(number)),
        ("//" ++ (*(anybut("\n")))
                            -> return(comment)),
        (lex.whitespace     -> return(whitespace))
    ].

:- pred ignore_tokens(lex_token(token_basic)::in) is semidet.

ignore_tokens(lex_token(whitespace, _)).
ignore_tokens(lex_token(comment, _)).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- func pz_bnf = bnf(token_basic, non_terminal, pz_node).

pz_bnf = bnf(pzt, eof, [
        bnf_rule("plasma textual bytecode", pzt, [
            bnf_rhs([], const(pzt(asm([])))),
            bnf_rhs([nt(item), nt(pzt)],
                det_func((pred(Nodes::in, Node::out) is semidet :-
                    Nodes = [item(X), pzt(asm(Xs))],
                    Node = pzt(asm([X | Xs]))
                ))
            )
        ]),
        bnf_rule("item", item, [
            bnf_rhs([nt(proc)], identity),
            bnf_rhs([nt(data)], identity)
        ]),

        bnf_rule("proc", proc, [
            bnf_rhs([t(proc), nt(qname), nt(proc_sig), nt(proc_body),
                    t(semicolon)],
                (func(Nodes) =
                    ( if Nodes = [context(Context), symbol(Name), proc_sig(Sig),
                            nil, _]
                    then
                        yes(item(asm_entry(Name, Context,
                            asm_proc_decl(Sig))))
                    else if Nodes = [context(Context), symbol(Name),
                            proc_sig(Sig), blocks(Blocks), _]
                    then
                        yes(item(asm_entry(Name, Context,
                            asm_proc(Sig, Blocks))))
                    else
                        no
                    )
                )
            )
        ]),
        bnf_rule("proc sig", proc_sig, [
            bnf_rhs([t(open_paren), nt(data_width_list), t(dash),
                    nt(data_width_list), t(close_paren)],
                det_func((pred(Nodes::in, Node::out) is semidet :-
                    Nodes = [_, data_width_list(Inputs), _,
                        data_width_list(Outputs), _],
                    Node = proc_sig(pz_signature(Inputs, Outputs))
                ))
            )
        ]),
        bnf_rule("data width list", data_width_list, [
            bnf_rhs([],
                const(data_width_list([]))),
            bnf_rhs([nt(data_width), nt(data_width_list)],
                det_func((pred(Nodes::in, Node::out) is semidet :-
                    Nodes = [data_width(Width), data_width_list(Widths)],
                    Node = data_width_list([Width | Widths])
                ))
            )
        ]),
        bnf_rule("proc body", proc_body, [
            bnf_rhs([],
                const(nil)),
            bnf_rhs([t(open_curly), nt(instrs_or_blocks), t(close_curly)],
                identity_nth(2))
            ]),
        bnf_rule("proc body", instrs_or_blocks, [
            bnf_rhs([nt(instrs)],
                det_func((pred(Nodes::in, Node::out) is semidet :-
                    Nodes = [instrs(Is)],
                    Is = [I | _],
                    Context = I ^ pzti_context,
                    Node = blocks([pzt_block("_", Is, Context)])
                ))
            ),
            bnf_rhs([nt(block), nt(blocks)],
                det_func((pred(Nodes::in, Node::out) is semidet :-
                    Nodes = [block(B), blocks(Bs)],
                    Node = blocks([B | Bs])
                ))
            )
        ]),
        bnf_rule("blocks", blocks, [
            bnf_rhs([],
                const(blocks([]))),
            bnf_rhs([nt(block), nt(blocks)],
                det_func((pred(Nodes::in, Node::out) is semidet :-
                    Nodes = [block(B), blocks(Bs)],
                    Node = blocks([B | Bs])
                ))
            )
        ]),
        bnf_rule("block", block, [
            bnf_rhs([t(block), t(identifier), t(open_curly), nt(instrs),
                    t(close_curly)],
                det_func((pred(Nodes::in, Node::out) is semidet :-
                    Nodes = [context(Context), string(Name, _), _,
                        instrs(Instrs), _],
                    Node = block(pzt_block(Name, Instrs, Context))
                ))
            )
        ]),

        bnf_rule("instructions", instrs, [
            bnf_rhs([],
                const(instrs([]))),
            bnf_rhs([nt(instr), nt(instrs)],
                det_func((pred(Nodes::in, Node::out) is semidet :-
                    Nodes = [instr(I), instrs(Is)],
                    Node = instrs([I | Is])
                ))
            )
        ]),
        bnf_rule("instruction", instr, [
            bnf_rhs([nt(instr_name), nt(instr_width)],
                det_func((pred(Nodes::in, Node::out) is semidet :-
                    Nodes = [instr(pzt_instruction(Instr, _, Context)),
                        Widths0],
                    ( Widths0 = nil,
                        Widths = no
                    ; Widths0 = data_width(Width),
                        Widths = one_width(Width)
                    ; Widths0 = data_width_2(Width1, Width2),
                        Widths = two_widths(Width1, Width2)
                    ),
                    Node = instr(pzt_instruction(Instr, Widths, Context))
                ))
            )
        ]),
        bnf_rule("instruction", instr_name, [
            bnf_rhs([t(identifier)],
                det_func((pred(Nodes::in, Node::out) is semidet :-
                    Nodes = [string(S, C)],
                    Node = instr(pzt_instruction(pzti_word(symbol(S)), no, C))
                ))
            ),
            bnf_rhs([t(number)],
                det_func((pred(Nodes::in, Node::out) is semidet :-
                    Nodes = [num(N, C)],
                    Node = instr(pzt_instruction( pzti_load_immediate(N),
                        no, C))
                ))
            ),
            bnf_rhs([t(cjmp), t(identifier)],
                det_func((pred(Nodes::in, Node::out) is semidet :-
                    Nodes = [context(C), string(Dest, _)],
                    Node = instr(pzt_instruction(pzti_cjmp(Dest), no, C))
                ))
            ),
            bnf_rhs([t(roll), t(number)],
                det_func((pred(Nodes::in, Node::out) is semidet :-
                    Nodes = [context(C), num(N, _)],
                    Node = instr(pzt_instruction(pzti_roll(N), no, C))
                ))
            ),
            bnf_rhs([t(pick), t(number)],
                det_func((pred(Nodes::in, Node::out) is semidet :-
                    Nodes = [context(C), num(N, _)],
                    Node = instr(pzt_instruction(pzti_pick(N), no, C))
                ))
            )
        ]),
        bnf_rule("instruction", instr_width, [
            bnf_rhs([], const(nil)),
            bnf_rhs([t(colon), nt(data_width), nt(instr_width_2)],
                det_func((pred(Nodes::in, Node::out) is semidet :-
                    (
                        Nodes = [_, data_width(Width1), data_width(Width2)],
                        Node = data_width_2(Width1, Width2)
                    ;
                        Nodes = [_, data_width(Width), nil],
                        Node = data_width(Width)
                    )
                ))
            )
        ]),
        bnf_rule("instruction", instr_width_2, [
            bnf_rhs([], const(nil)),
            bnf_rhs([t(colon), nt(data_width)], identity_nth(2))
        ]),

        bnf_rule("data", data, [
            bnf_rhs([t(data), t(identifier), t(equals), nt(data_type),
                    nt(data_value), t(semicolon)],
                det_func((pred(Nodes::in, Node::out) is semidet :-
                    Nodes = [context(Context), string(Name, _), _,
                        data_type(Type), data_value(Value), _],
                    Node = item(asm_entry(symbol(Name), Context,
                        asm_data(Type, Value)))
                ))
            )
        ]),
        bnf_rule("data type", data_type, [
            bnf_rhs([t(array), t(open_paren), nt(data_width),
                    t(close_paren)],
                det_func((pred(Nodes::in, Node::out) is semidet :-
                    Nodes = [_, _, data_width(Width), _],
                    Node = data_type(type_array(Width))
                ))
            )
        ]),
        bnf_rule("data value", data_value, [
            bnf_rhs([t(open_curly), nt(number_list), t(close_curly)],
                det_func((pred(Nodes::in, Node::out) is semidet :-
                    Nodes = [_, num_list(List), _],
                    Node = data_value(pzv_sequence(List))
                ))
            )
        ]),
        bnf_rule("number list", number_list, [
            bnf_rhs([],
                const(num_list([]))),
            bnf_rhs([t(number), nt(number_list)],
                det_func((pred(Nodes::in, Node::out) is semidet :-
                    Nodes = [num(X, _), num_list(Xs)],
                    Node = num_list([X | Xs])
                ))
            )
        ]),

        bnf_rule("data width", data_width, [
            bnf_rhs([t(w)], const(data_width(w_fast))),
            bnf_rhs([t(w8)], const(data_width(w8))),
            bnf_rhs([t(w16)], const(data_width(w16))),
            bnf_rhs([t(w32)], const(data_width(w32))),
            bnf_rhs([t(w64)], const(data_width(w64))),
            bnf_rhs([t(w_ptr)], const(data_width(w_ptr))),
            bnf_rhs([t(ptr)], const(data_width(ptr)))
            ]),

        bnf_rule("qualified name", qname, [
            bnf_rhs([t(identifier), nt(qname_cont)],
                (func(Nodes) =
                    ( if Nodes = [string(Name1, _), string(Name2, _)] then
                        yes(symbol(symbol([Name1], Name2)))
                    else if Nodes = [string(Name, _), nil] then
                        yes(symbol(symbol(Name)))
                    else
                        no
                    )
                ))
            ]),
        bnf_rule("qualified name", qname_cont, [
            bnf_rhs([],
                const(nil)),
            bnf_rhs([t(period), t(identifier)],
                identity_nth(2))
            ])
    ]).

:- type non_terminal
    --->    pzt
    ;       item
    ;       proc
    ;       proc_sig
    ;       proc_body
    ;       instrs_or_blocks
    ;       blocks
    ;       block
    ;       instrs
    ;       instr
    ;       instr_name
    ;       instr_width
    ;       instr_width_2
    ;       data
    ;       data_type
    ;       data_value
    ;       data_width_list
    ;       data_width
    ;       qname
    ;       qname_cont
    ;       number_list.

:- type pz_node
    --->    pzt(asm)
    ;       item(asm_entry)
    ;       proc_sig(pz_signature)
    ;       blocks(list(pzt_block))
    ;       block(pzt_block)
    ;       instrs(list(pzt_instruction))
    ;       instr(pzt_instruction)
    ;       data_type(pz_data_type)
    ;       data_value(pz_data_value)
    ;       data_width_list(list(pz_data_width))
    ;       data_width(pz_data_width)
    ;       data_width_2(pz_data_width, pz_data_width)
    ;       symbol(symbol)
    ;       context(context)
    ;       string(string, context)
    ;       num(int, context)
    ;       num_list(list(int))
    ;       nil.

:- instance token_to_result(token_basic, pz_node) where [
        token_to_result(Terminal, MaybeString, Context) =
            ( if
                ( Terminal = proc
                ; Terminal = data
                ; Terminal = block
                ; Terminal = cjmp
                ; Terminal = roll
                ; Terminal = pick
                )
            then
                context(Context)
            else if
                Terminal = identifier,
                MaybeString = yes(String)
            then
                string(String, Context)
            else if
                Terminal = number,
                MaybeString = yes(String)
            then
                num(det_to_int(String), Context)
            else
                nil
            )
    ].

%-----------------------------------------------------------------------%

:- pred token_is_data_width(token_basic::in, pz_data_width::out) is semidet.

token_is_data_width(w,      w_fast).
token_is_data_width(w8,     w8).
token_is_data_width(w16,    w16).
token_is_data_width(w32,    w32).
token_is_data_width(w64,    w64).
token_is_data_width(w_ptr,  w_ptr).
token_is_data_width(ptr,    ptr).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
