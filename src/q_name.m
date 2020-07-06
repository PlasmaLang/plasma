%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module q_name.
%
% Copyright (C) 2015-2016, 2019-2020 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
% Qualified name ADT
%
%-----------------------------------------------------------------------%

:- interface.

:- import_module list.
:- import_module maybe.

%-----------------------------------------------------------------------%

    % Qualified name.
    %
:- type q_name.

:- func q_name(nq_name) = q_name.
:- func q_name_single(string) = q_name.

:- pragma obsolete(q_name/2).
:- func q_name(list(string), string) = q_name.

:- func q_name_from_dotted_string(string) = q_name.
:- func q_name_from_list(list(string)) = q_name.

:- func q_name_to_string(q_name) = string.

:- pred q_name_parts(q_name, maybe(q_name), nq_name).
:- mode q_name_parts(in, out, out) is det.

:- func q_name_append_str(q_name, string) = q_name.

:- pred q_name_append(q_name, nq_name, q_name).
:- mode q_name_append(in, in, out) is det.
:- mode q_name_append(in, out, in) is semidet.

:- func q_name_append(q_name, nq_name) = q_name.

:- func q_name_unqual(q_name) = string.

%-----------------------------------------------------------------------%

    % Non-qualified name.
    %
    % This is an abstract type over a string, but it ensures that the string
    % is a legal identifier.
    %
:- type nq_name.

:- func nq_name_det(string) = nq_name.

:- func nq_name_from_string(string) = maybe_error(nq_name).

:- func nq_name_to_string(nq_name) = string.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

:- import_module require.
:- import_module string.

%-----------------------------------------------------------------------%

:- type q_name
    --->    unqualified(nq_name)
    ;       qualified(nq_name, q_name).

q_name(Name) = unqualified(Name).
q_name_single(Name) = unqualified(nq_name(Name)).

q_name(Qualifiers, Name) = QName :-
    q_name_break(QName, map(nq_name_det, Qualifiers), nq_name_det(Name)).

q_name_from_dotted_string(Dotted) =
    q_name_from_list(split_at_char('.', Dotted)).

q_name_from_list(List) = QName :-
    det_split_last(List, Qualifiers, Name),
    q_name_break(QName, map(nq_name_det, Qualifiers), nq_name_det(Name)).

q_name_to_string(QName) = String :-
    q_name_break(QName, Quals, Name),
    ( Quals = [_ | _],
        String = join_list(".", map(nq_name_to_string, Quals)) ++ "." ++
            nq_name_to_string(Name)
    ; Quals = [],
        String = nq_name_to_string(Name)
    ).

q_name_parts(QName, MaybeModule, Symbol) :-
    q_name_break(QName, ModuleParts, Symbol),
    ( if split_last(ModuleParts, ModuleHead, ModuleTail) then
        q_name_break(Module, ModuleHead, ModuleTail),
        MaybeModule = yes(Module)
    else
        MaybeModule = no
    ).

q_name_append_str(ModuleSym, Name) = QName :-
    q_name_append(ModuleSym, nq_name_det(Name), QName).

q_name_append(A, B, R) :-
    q_name_break(A, AMods, AName),
    Mods = AMods ++ [AName],
    q_name_break(R, Mods, B).

q_name_append(A, B) = R :-
    q_name_append(A, B, R).

q_name_unqual(unqualified(S)) = nq_name_to_string(S).
q_name_unqual(qualified(_, QName)) = q_name_unqual(QName).

%-----------------------------------------------------------------------%

    % Break up a q_name into parts.
    %
:- pred q_name_break(q_name, list(nq_name), nq_name).
:- mode q_name_break(in, out, out) is det.
:- mode q_name_break(out, in, in) is det.

q_name_break(unqualified(Name), [], Name).
q_name_break(qualified(Module, QName0), [Module | Modules], Name) :-
    q_name_break(QName0, Modules, Name).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- type nq_name
    --->    nq_name(string).

nq_name_det(String) = Name :-
    Check = nq_name_from_string(String),
    ( Check = ok(Name)
    ; Check = error(Error),
        unexpected($file, $pred, Error)
    ).

nq_name_from_string(String) = MaybeName :-
    ( if not is_all_alnum_or_underscore(String) then
        MaybeName = error("Illegal identifier")
    else if length(String) = 0 then
        MaybeName = error("Empty identifier")
    else
        MaybeName = ok(nq_name(String))
    ).

nq_name_to_string(nq_name(String)) = String.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
