%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module symtab.
%
% Symbol table ADT
%
% Copyright (C) 2015 Paul Bone
% Distributed under the terms of the GPLv2 see ../LICENSE.tools
%
%-----------------------------------------------------------------------%

:- interface.

:- import_module list.
:- import_module set.
:- import_module string.

%-----------------------------------------------------------------------%

    % Symbols are optionally module qualified.  Which symbol names are
    % considered equal depends upon context.
    %
:- type symbol.

:- func symbol(string) = symbol.

:- func symbol(list(string), string) = symbol.

:- pred symbol_parts(symbol, list(string), string).
:- mode symbol_parts(in, out, out) is det.
:- mode symbol_parts(out, in, in) is det.

:- func symbol_to_string(symbol) = string.

:- func symbol_append(symbol, string) = symbol.

%-----------------------------------------------------------------------%

    % True if the symbol has this unqualified name, the symbol may be
    % qualified or unqualified.
    %
:- pred symbol_has_name(symbol::in, string::in) is semidet.

%-----------------------------------------------------------------------%

    % Symbol tables store forward (name->ID) and reverse (ID->name) mappings
    % for symbols.  Symbol tables provide a convenient interface for looking up
    % symbols with various levels of module qualification.  Symbols are
    % expected to be inserted into the table fully qualified, but queried with
    % any amount of qualification.  A query may return multiple results.  For
    % example:
    %
    %   bar.foo         - 1
    %   baz.foo         - 2
    %   baz.troz.foo    - 3
    %   baz.bar.foo     - 4
    %
    % are all in the able.  a query for "foo" will return all four items.  A
    % query for "bar.foo" will return the set of {1, 4}.
    %
:- type symtab(Id).

    % Create an empty symbol table.
    %
:- func init = symtab(Id).

    % Try to insert a new entry.
    %
:- pred insert(symbol::in, Id::in, symtab(Id)::in, symtab(Id)::out)
    is semidet.

%-----------------------------------------------------------------------%

    % Search for an entry by name.  Return the set of entries that match.
    %
:- pred search(symtab(Id)::in, symbol::in, set(Id)::out) is det.

:- pred search_exact(symtab(Id)::in, symbol::in, Id::out) is semidet.

:- pred det_search_exact(symtab(Id)::in, symbol::in, Id::out) is det.

    % Lookup an entry's name by its ID.
    %
:- pred lookup(symtab(Id)::in, Id::in, symbol::out) is det.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module map.
:- import_module require.

%-----------------------------------------------------------------------%

:- type symbol
    --->    unqualified(string)
    ;       qualified(string, symbol).

symbol(Name) = unqualified(Name).

symbol(Qualifiers, Name) = Symbol :-
    symbol_parts(Symbol, Qualifiers, Name).

symbol_parts(unqualified(Name), [], Name).
symbol_parts(qualified(Module, Symbol0), [Module | Modules], Name) :-
    symbol_parts(Symbol0, Modules, Name).

symbol_to_string(Symbol) = String :-
    symbol_parts(Symbol, Quals, Name),
    String = join_list(".", Quals) ++ "." ++ Name.

symbol_append(ModuleSym, Name) = symbol(ModuleParts, Name) :-
    symbol_parts(ModuleSym, ParentModParts, ModuleName),
    ModuleParts = ParentModParts ++ [ModuleName].

%-----------------------------------------------------------------------%

symbol_has_name(Symbol, Name) :-
    symbol_parts(Symbol, _, Name).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- type symtab(Id)
    --->    symtab(
                s_exact_lookup          :: map(symbol, Id),
                s_lookup                :: symbol_tree(Id),
                s_rev_lookup            :: map(Id, symbol)
            ).

% Symbol trees store symbols from their base, then perant then grandperent
% module.  This allows us to lookup the set of symbols for a partially
% qualified name.

:- type symbol_tree(Id) == map(string, symbol_tree_node(Id)).

:- type symbol_tree_node(Id)
    --->    symbol_tree_node(
                stn_branches            :: symbol_tree(Id),
                stn_ids                 :: set(Id)
            ).

%-----------------------------------------------------------------------%

init = symtab(init, init, init).

%-----------------------------------------------------------------------%

insert(Symbol, Id, !Symtab) :-
    det_insert(Symbol, Id, yes, !Symtab).

:- pred det_insert(symbol::in, Id::in, bool::out,
    symtab(Id)::in, symtab(Id)::out) is det.

det_insert(Sym, Id, Success, !Symtab) :-
    symbol_parts(Sym, Modules, Name),
    ExactMap0 = !.Symtab ^ s_exact_lookup,
    ( if map.insert(Sym, Id, ExactMap0, ExactMap) then
        Tree0 = !.Symtab ^ s_lookup,
        RevMap0 = !.Symtab ^ s_rev_lookup,
        Node0 = get_or_create_node(Tree0, Name),
        insert_tree(reverse(Modules), Id, Node0, Node),
        map.set(Name, Node, Tree0, Tree),
        map.det_insert(Id, Sym, RevMap0, RevMap),
        !Symtab ^ s_exact_lookup := ExactMap,
        !Symtab ^ s_lookup := Tree,
        !Symtab ^ s_rev_lookup := RevMap,
        Success = yes
    else
        Success = no
    ).

:- pred insert_tree(list(string)::in, Id::in,
    symbol_tree_node(Id)::in, symbol_tree_node(Id)::out) is det.

insert_tree(Modules0, Id, !Tree) :-
    !Tree ^ stn_ids := insert(!.Tree ^ stn_ids, Id),
    ( Modules0 = []
    ; Modules0 = [Module | Modules],
        Node0 = get_or_create_node(!.Tree ^ stn_branches, Module),
        insert_tree(Modules, Id, Node0, Node),
        set(Module, Node, !.Tree ^ stn_branches, Branches),
        !Tree ^ stn_branches := Branches
    ).

:- func get_or_create_node(symbol_tree(Id), string) = symbol_tree_node(Id).

get_or_create_node(Tree, Name) = Node :-
    ( search(Tree, Name, NodePrime) ->
        Node = NodePrime
    ;
        Node = symbol_tree_node(init, init)
    ).

%-----------------------------------------------------------------------%

search(Symtab, Sym, Ids) :-
    symbol_parts(Sym, Modules, Name),
    ( search(Symtab ^ s_lookup, Name, Node) ->
        search_tree(Node, Modules, Ids)
    ;
        Ids = set.init
    ).

search_exact(Symtab, Sym, Id) :-
    search(Symtab ^ s_exact_lookup, Sym, Id).

det_search_exact(Symtab, Sym, Id) :-
    ( if search_exact(Symtab, Sym, IdPrime) then
        Id = IdPrime
    else
        unexpected($file, $pred, "Entry not found or ambigious")
    ).

:- pred search_tree(symbol_tree_node(Id)::in, list(string)::in, set(Id)::out)
    is det.

search_tree(Tree, [], Tree ^ stn_ids).
search_tree(Tree, [Module | Modules], Ids) :-
    ( search(Tree ^ stn_branches, Module, Node) ->
        search_tree(Node, Modules, Ids)
    ;
        Ids = set.init
    ).

lookup(Symtab, Id, Sym) :-
    map.lookup(Symtab ^ s_rev_lookup, Id, Sym).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
