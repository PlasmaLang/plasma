%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module q_name.
%
% Copyright (C) 2015-2016 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
% Qualified name ADT
%
%-----------------------------------------------------------------------%

:- interface.

:- import_module list.
:- import_module string.

%-----------------------------------------------------------------------%

    % Qualified name.
    %
:- type q_name.

:- func q_name(string) = q_name.

:- func q_name(list(string), string) = q_name.

:- pred q_name_parts(q_name, list(string), string).
:- mode q_name_parts(in, out, out) is det.
:- mode q_name_parts(out, in, in) is det.

:- func q_name_to_string(q_name) = string.

:- func q_name_append(q_name, string) = q_name.

%-----------------------------------------------------------------------%

    % True if the q_name has this unqualified name, the q_name may be
    % qualified or unqualified.
    %
:- pred q_name_has_name(q_name::in, string::in) is semidet.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

%-----------------------------------------------------------------------%

:- type q_name
    --->    unqualified(string)
    ;       qualified(string, q_name).

q_name(Name) = unqualified(Name).

q_name(Qualifiers, Name) = QName :-
    q_name_parts(QName, Qualifiers, Name).

q_name_parts(unqualified(Name), [], Name).
q_name_parts(qualified(Module, QName0), [Module | Modules], Name) :-
    q_name_parts(QName0, Modules, Name).

q_name_to_string(QName) = String :-
    q_name_parts(QName, Quals, Name),
    ( Quals = [_ | _],
        String = join_list(".", Quals) ++ "." ++ Name
    ; Quals = [],
        String = Name
    ).

q_name_append(ModuleSym, Name) = q_name(ModuleParts, Name) :-
    q_name_parts(ModuleSym, ParentModParts, ModuleName),
    ModuleParts = ParentModParts ++ [ModuleName].

%-----------------------------------------------------------------------%

q_name_has_name(QName, Name) :-
    q_name_parts(QName, _, Name).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
