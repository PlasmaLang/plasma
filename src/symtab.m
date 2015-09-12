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

:- import_module maybe.
:- import_module string.

%-----------------------------------------------------------------------%

:- type symbol.

:- func symbol(string) = symbol.

:- func symbol(string, string) = symbol.

:- pred symbol_names(symbol::in, maybe(string)::out, string::out) is det.

:- func symbol_to_string(symbol) = string.

%-----------------------------------------------------------------------%

    % True if the symbol has this unqualified name, the symbol may be
    % qualified or unqualified.
    %
:- pred symbol_has_name(symbol::in, string::in) is semidet.

%-----------------------------------------------------------------------%

:- type symtab(V).

%-----------------------------------------------------------------------%

    % Create an empty symbol table.
    %
:- func init = symtab(V).

    % Try to insert a new entry, fail if it is not found.
    %
:- pred insert(symbol::in, V::in, symtab(V)::in, symtab(V)::out)
    is semidet.

    % Try to insert a new entry, throw an exception if it is not found.
    %
:- pred det_insert(symbol::in, V::in, symtab(V)::in, symtab(V)::out)
    is det.

%-----------------------------------------------------------------------%

    % Search for an entry, fail if it was not found.
    %
:- pred search(symtab(V)::in, symbol::in, V::out) is semidet.

    % Search for an entry, throw an exception if it was not found.
    %
:- pred lookup(symtab(V)::in, symbol::in, V::out) is det.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module map.
:- import_module require.

%-----------------------------------------------------------------------%

:- type symbol
    --->    unqualified(string)
    ;       qualified(string, string).

symbol(Name) = unqualified(Name).

symbol(Module, Name) = qualified(Module, Name).

symbol_names(unqualified(Name), no, Name).
symbol_names(qualified(Module, Name), yes(Module), Name).

symbol_to_string(unqualified(String)) = String.
symbol_to_string(qualified(Module, String)) = Module ++ "." ++ String.

%-----------------------------------------------------------------------%

symbol_has_name(unqualified(Name), Name).
symbol_has_name(qualified(_, Name), Name).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- type symtab(V)
    --->    symtab(
                s_qualified_symbols     :: map(string, symtab_node(V)),
                s_local_symbols         :: map(string, V)
            ).

:- type symtab_node(V) == map(string, V).

%-----------------------------------------------------------------------%

init = symtab(init, init).

%-----------------------------------------------------------------------%

insert(unqualified(Name), V, !Symtab) :-
    insert(Name, V, !.Symtab ^ s_local_symbols, Map),
    !Symtab ^ s_local_symbols := Map.
insert(qualified(Module, Name), V, !Symtab) :-
    ( search(!.Symtab ^ s_qualified_symbols, Module, ModuleSyms0) ->
        insert(Name, V, ModuleSyms0, ModuleSyms)
    ;
        ModuleSyms = singleton(Name, V)
    ),
    set(Module, ModuleSyms, !.Symtab ^ s_qualified_symbols, QualSyms),
    !Symtab ^ s_qualified_symbols := QualSyms.

det_insert(K, V, !Symtab) :-
    ( insert(K, V, !Symtab) ->
        true
    ;
        unexpected($file, $pred, "Symbol already present")
    ).

%-----------------------------------------------------------------------%

search(Symtab, unqualified(Name), V) :-
    ( search(Symtab ^ s_local_symbols, Name, VPrime) ->
        V = VPrime
    ;
        search(Symtab ^ s_qualified_symbols, "builtin", BuiltinSyms),
        search(BuiltinSyms, Name, V)
    ).
search(Symtab, qualified(Module, Name), V) :-
    search(Symtab ^ s_qualified_symbols, Module, ModuleSyms),
    search(ModuleSyms, Name, V).

lookup(Symtab, Sym, V) :-
    ( search(Symtab, Sym, VPrime) ->
        V = VPrime
    ;
        unexpected($file, $pred,
            format("Symbol not found (%s)", [s(string(Sym))]))
    ).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
