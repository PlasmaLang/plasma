%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module symtab.
%
% Copyright (C) 2015 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
% Symbol table ADT
%
%-----------------------------------------------------------------------%

:- interface.

:- import_module list.
:- import_module string.

%-----------------------------------------------------------------------%

    % Symbols are optionally module qualified.
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
    % for symbols.  Symbols are always fully qualified, since they can be
    % important into the environment this is not burdensome on developers.
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

    % Search for an entry by name.
    %
:- pred search(symtab(Id)::in, symbol::in, Id::out) is semidet.

:- pred lookup(symtab(Id)::in, symbol::in, Id::out) is det.

    % Lookup an entry's name by its ID.
    %
:- pred lookup_rev(symtab(Id)::in, Id::in, symbol::out) is det.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module map.
:- import_module require.
:- import_module set.

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
    ( Quals = [_ | _],
        String = join_list(".", Quals) ++ "." ++ Name
    ; Quals = [],
        String = Name
    ).

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
                s_fwd_lookup            :: map(symbol, Id),
                s_rev_lookup            :: map(Id, symbol)
            ).

%-----------------------------------------------------------------------%

init = symtab(init, init).

%-----------------------------------------------------------------------%

insert(Symbol, Id, !Symtab) :-
    map.insert(Symbol, Id, !.Symtab ^ s_fwd_lookup, Fwd),
    map.insert(Id, Symbol, !.Symtab ^ s_rev_lookup, Rev),
    !:Symtab = symtab(Fwd, Rev).

%-----------------------------------------------------------------------%

search(Symtab, Sym, Id) :-
    map.search(Symtab ^ s_fwd_lookup, Sym, Id).

lookup(Symtab, Sym, Id) :-
    map.lookup(Symtab ^ s_fwd_lookup, Sym, Id).

lookup_rev(Symtab, Id, Sym) :-
    map.lookup(Symtab ^ s_rev_lookup, Id, Sym).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
