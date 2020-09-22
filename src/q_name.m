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

:- import_module util.
:- import_module util.pretty.

%-----------------------------------------------------------------------%

    % Qualified name.
    %
:- type q_name.

:- func q_name(nq_name) = q_name.
:- func q_name_single(string) = q_name.

:- func q_name_from_dotted_string(string) = q_name.

    % Throws an exception if the strings can't be made into nq_names.
    %
:- func q_name_from_strings(list(string)) = q_name.

    % This helps the parser avoid an inefficiency, the first argument is for
    % the module parts and the second for the symbol itself.
    %
:- func q_name_from_strings_2(list(string), string) = q_name.

:- func q_name_to_string(q_name) = string.

:- pred q_name_parts(q_name, maybe(q_name), nq_name).
:- mode q_name_parts(in, out, out) is det.

    % True of the qualified name is just an occurance of a simple name,
    % eg: it could be a variable name.
    %
:- pred q_name_is_single(q_name::in, string::out) is semidet.

    % Throws an exception if the string can't be made into nq_names.
    %
:- func q_name_append_str(q_name, string) = q_name.

:- pred q_name_append(q_name, nq_name, q_name).
:- mode q_name_append(in, in, out) is det.
:- mode q_name_append(in, out, in) is semidet.

:- func q_name_append(q_name, nq_name) = q_name.

:- func q_name_unqual(q_name) = nq_name.

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

:- func name_pretty(q_name) = pretty.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

:- import_module require.
:- import_module string.

%-----------------------------------------------------------------------%

:- type q_name
    --->    unqualified(nq_name)
    ;       qualified(q_name, nq_name).

q_name(Name) = unqualified(Name).
q_name_single(Name) = unqualified(nq_name(Name)).

q_name_from_dotted_string(Dotted) =
    q_name_from_list(map(nq_name_det, split_at_char('.', Dotted))).

q_name_from_strings(Strings) = q_name_from_list(map(nq_name_det, Strings)).

q_name_from_strings_2(Module, Symbol) =
    q_name_from_list_2(map(nq_name_det, Module), nq_name_det(Symbol)).

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
    ( ModuleParts = [],
        MaybeModule = no
    ; ModuleParts = [_ | _],
        MaybeModule = yes(q_name_from_list(ModuleParts))
    ).

q_name_is_single(QName, nq_name_to_string(NQName)) :-
    q_name_break(QName, [], NQName).

q_name_append_str(ModuleSym, Name) = QName :-
    q_name_append(ModuleSym, nq_name_det(Name), QName).

q_name_append(A, B, R) :-
    q_name_break(A, AMods, AName),
    Mods = q_name_from_list_2(AMods, AName),
    R = qualified(Mods, B).

q_name_append(A, B) = R :-
    q_name_append(A, B, R).

q_name_unqual(unqualified(NQName)) = NQName.
q_name_unqual(qualified(_, NQName)) = NQName.

%-----------------------------------------------------------------------%

:- func q_name_from_list(list(nq_name)) = q_name.

q_name_from_list(List) = QName :-
    det_split_last(List, Qualifiers, Name),
    QName = q_name_from_list_2(Qualifiers, Name).

:- func q_name_from_list_2(list(nq_name), nq_name) = q_name.

q_name_from_list_2([], Name) = unqualified(Name).
q_name_from_list_2(Quals@[_ | _], Name) =
    qualified(q_name_from_list(Quals), Name).

    % Break up a q_name into parts.
    %
:- pred q_name_break(q_name::in, list(nq_name)::out, nq_name::out) is det.

q_name_break(unqualified(Name), [], Name).
q_name_break(qualified(Modules0, Name), Modules, Name) :-
    Modules = reverse(q_name_break_2(Modules0)).

:- func q_name_break_2(q_name) = list(nq_name).

q_name_break_2(unqualified(Name)) = [Name].
q_name_break_2(qualified(Module, Name)) = [Name | q_name_break_2(Module)].

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

name_pretty(Name) = p_str(q_name_to_string(Name)).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
