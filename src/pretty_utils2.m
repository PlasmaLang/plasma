%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module pretty_utils2.
%
% Pretty printer utils 2.
%
% Copyright (C) 2019-2020 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
%-----------------------------------------------------------------------%
:- interface.

:- import_module cord.
:- import_module int.
:- import_module list.
:- import_module string.

%-----------------------------------------------------------------------%

:- type pretty
    --->    p_unit(cord(string))
    ;       p_group(list(pretty))
    ;       p_spc
    ;       p_nl_hard
    ;       p_nl_soft
    ;       p_tabstop.

:- func p_str(string) = pretty.
:- func p_cord(cord(string)) = pretty.

    % p_parens(OuterOpen,  InnerOpen,
    %          OuterClose, InnerClose, Delimiter, Items) = Pretty.
    %
    % The items are delimited by delimiter and then made a group for
    % indentation, The InnerOpen/Close are inside the group and the
    % OuterOpen/Close are outside it.
    %
:- func p_parens(list(pretty), list(pretty),
                 list(pretty), list(pretty),
                 list(pretty), list(pretty)) =
    list(pretty).

:- func pretty(int, list(pretty)) = cord(string).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
:- implementation.

:- import_module pretty_utils.
:- import_module require.
:- import_module util.

%-----------------------------------------------------------------------%

p_str(String) = p_unit(singleton(String)).
p_cord(Cord) = p_unit(Cord).

%-----------------------------------------------------------------------%

p_parens(OuterLeft, InnerLeft, OuterRight, InnerRight, D, Items) =
    OuterLeft ++
    [p_group(InnerLeft ++ list_join(D, Items) ++ InnerRight)] ++
    OuterRight.

%-----------------------------------------------------------------------%

%
% The pretty printer is implemented in three stages:
%
% + The first is the "client" of this API that turns something like Plasma's
%   core representation the pretty printer expression defined in this
%   module's interface.  One example of this is core.pretty.
%
% + The second is in pretty_to_pis.  It turns this nested expression into a
%   flat sequence of "print instructions".  It is not stateful and will
%   allow for some memorisation/caching.
%
% + The third turns this series of instructions into the final cord(string).
%   it is in pis_to_cord and is stateful.
%
% The main state we're concerned with is the current indentation level and
% column number, these change depending on indentation choices.
%

pretty(Indent, Pretties) = Cord :-
    Instrs = map(pretty_to_pis, Pretties),
    pis_to_cord(condense(Instrs), empty, Cord, Indent, _, [Indent], _,
        Indent, _).

:- type print_instr
    --->    pi_cord(cord(string))
    ;       pi_nl
    ;       pi_indent
    ;       pi_indent(int)
    ;       pi_indent_pop
    ;       pi_delay(list(pretty)).

%-----------------------------------------------------------------------%

:- func pretty_to_pis(pretty) = list(print_instr).

pretty_to_pis(p_unit(Cord)) = [pi_cord(Cord)].
pretty_to_pis(p_group(Pretties)) = Out :-
    ( if
        Pretties = [p_group(InnerPretties)]
    then
        Out = pretty_to_pis(p_group(InnerPretties))
    else if
        all [P] (
            member(P, Pretties) =>
            not ( P = p_nl_hard
                ; P = p_nl_soft
                )
        )
    then
        % Don't add an indent if there's no linebreaks in this group.
        Out = condense(map(pretty_to_pis, Pretties))
    else
        find_indent(Pretties, 0, Indent0),
        ( Indent0 = indent_default,
            Indent = pi_indent
        ; Indent0 = indent_rel(Rel),
            Indent = pi_indent(Rel)
        ),
        Out = [Indent, pi_delay(Pretties), pi_indent_pop]
    ).
pretty_to_pis(p_spc) = [pi_cord(singleton(" "))].
pretty_to_pis(p_nl_hard) = [pi_nl].
pretty_to_pis(p_nl_soft) = [pi_nl].
pretty_to_pis(p_tabstop) = [].

:- type indent
    --->    indent_default
    ;       indent_rel(int).

:- pred find_indent(list(pretty)::in, int::in, indent::out) is det.

find_indent([], _, indent_default).
find_indent([P | Ps], Acc, Indent) :-
    ( P = p_unit(Cord),
        find_indent(Ps, Acc + cord_string_len(Cord), Indent)
    ; P = p_spc,
        find_indent(Ps, Acc + 1, Indent)
    ;
        ( P = p_nl_hard
        ; P = p_nl_soft
        ),
        Indent = indent_default,
        ( if
            all [T] (
                member(T, Ps) => not T = p_tabstop
            )
        then
            true
        else
            unexpected($file, $pred, "Break followed by tabstop")
        )
    ; P = p_tabstop,
        Indent = indent_rel(Acc),
        ( if
            some [B] (
                member(B, Ps), ( B = p_nl_hard ; B = p_nl_soft )
            )
        then
            true
        else
            unexpected($file, $pred, "tabstop not followed by newline")
        )
    ; P = p_group(Pretties),
        FoundBreak = single_line_len(Pretties, 0),
        ( FoundBreak = found_break,
            % If there was an (honored) break in the inner group the
            % outer group has a fixed indent of "offset"
            Indent = indent_default
        ; FoundBreak = single_line(Len),
            % But if the inner group had no breaks then the search for the
            % outer group's tabstop continues.
            find_indent(Ps, Acc + Len, Indent)
        )
    ).

:- type single_line_len
    --->    found_break
            % When there was no break this returns the total length.
    ;       single_line(int).

:- func single_line_len(list(pretty), int) = single_line_len.

single_line_len([], Acc) = single_line(Acc).
single_line_len([P | Ps], Acc) = FoundBreak :-
    ( P = p_unit(Cord),
        FoundBreak = single_line_len(Ps, Acc + cord_string_len(Cord))
    ; P = p_spc,
        FoundBreak = single_line_len(Ps, Acc + 1)
    ; P = p_nl_hard,
        FoundBreak = found_break
    ; P = p_nl_soft,
        % TODO: Maybe we can handle this better.
        FoundBreak = found_break
    ; P = p_tabstop,
        FoundBreak = single_line_len(Ps, Acc)
    ; P = p_group(Pretties),
        FoundBreak = single_line_len(Pretties ++ Ps, Acc)
    ).

%-----------------------------------------------------------------------%

:- pred pis_to_cord(list(print_instr)::in,
    cord(string)::in, cord(string)::out, int::in, int::out,
    list(int)::in, list(int)::out, int::in, int::out) is det.

pis_to_cord([], !Cord, !Indent, !IndentStack, !Pos).
pis_to_cord([Pi | Pis], !Cord, !Indent, !IndentStack, !Pos) :-
    ( Pi = pi_cord(New),
        !:Cord = !.Cord ++ New,
        !:Pos = !.Pos + cord_string_len(New)
    ; Pi = pi_nl,
        !:Cord = !.Cord ++ line(!.Indent),
        !:Pos = !.Indent
    ;
        ( Pi = pi_indent,
            NewIndent = !.Indent + unit
        ; Pi = pi_indent(Rel0),
            NewIndent = !.Pos + Rel0
        ),
        push(!.Indent, !IndentStack),
        !:Indent = NewIndent
    ; Pi = pi_indent_pop,
        pop(!:Indent, !IndentStack)
    ; Pi = pi_delay(Pretties),
        Instrs = map(pretty_to_pis, Pretties),
        pis_to_cord(condense(Instrs), !Cord, !Indent, !IndentStack, !Pos)
    ),
    pis_to_cord(Pis, !Cord, !Indent, !IndentStack, !Pos).

%-----------------------------------------------------------------------%

:- func cord_string_len(cord(string)) = int.

cord_string_len(Cord) =
    foldl(func(S, L) = length(S) + L, Cord, 0).

:- pred push(T::in, list(T)::in, list(T)::out) is det.

push(X, Xs, [X | Xs]).

:- pred pop(T::out, list(T)::in, list(T)::out) is det.

pop(X, [X | Xs], Xs).
pop(_, [], _) :-
    unexpected($file, $pred, "Cannot pop empty stack").

    % Default indentation amount.
    %
:- func unit = int.
unit = 2.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
