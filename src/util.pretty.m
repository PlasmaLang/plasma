%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module util.pretty.
%
% Pretty printer utils.
%
% Copyright (C) 2019-2020 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
%-----------------------------------------------------------------------%
:- interface.

:- import_module cord.
:- import_module list.

%-----------------------------------------------------------------------%

:- type pretty
    --->    p_unit(cord(string))
    ;       p_group(pretty_group_type, list(pretty))
    ;       p_group_curly(
                pgc_first_line  :: list(pretty),
                pgc_open        :: cord(string),
                pgc_inside      :: list(pretty),
                pgc_close       :: cord(string)
            )
    ;       p_comment(
                pc_comment_begin    :: cord(string),
                pc_inside           :: list(pretty)
            )
    ;       p_spc
    ;       p_empty
    ;       p_nl_hard
    ;       p_nl_soft
    ;       p_nl_double % A hard break that adds an extra newline.
    ;       p_tabstop.

:- type pretty_group_type
    --->    g_expr
    ;       g_list.

:- func p_str(string) = pretty.
:- func p_cord(cord(string)) = pretty.
:- func p_quote(string, pretty) = pretty.

    % The first type of pretty group is for expressions. It's what's been used
    % so far to format an expression like:
    %
    %     let x = a +
    %               b
    %         y = foo()
    %         z
    %
:- func p_expr(list(pretty)) = pretty.

    % The second type is for lists, subsequent items arn't indented by a default
    % amount, they always start at the beginning, it can look like
    %
    %     let x = [1,
    %              2,
    %              3]
    %
    % (the group is within the list).
    %
:- func p_list(list(pretty)) = pretty.

:- type options
    --->    options(
                o_max_line      :: int,
                o_indent        :: int
            ).

:- func default_indent = int.

:- func max_line = int.

:- func default_options = options.

    % pretty(Options, CurIndent, Pretty) = Cord
    %
:- func pretty(options, int, list(pretty)) = cord(string).

    % These do the same as the pretty function above, except they use
    % default options.  They're useful for prettifying something with
    % defautl options to print it while throwing an exception.
:- func pretty(list(pretty)) = cord(string).

:- func pretty_str(list(pretty)) = string.

%-----------------------------------------------------------------------%

    % pretty_callish(Prefix, Args),
    % pretty_callish(Prefix, Args, Postfix),
    %
:- func pretty_callish(pretty, list(pretty)) = pretty.

:- func pretty_optional_args(pretty, list(pretty)) = pretty.

:- func pretty_seperated(list(pretty), list(pretty)) = list(pretty).

    % A shorthand for pretty_seperated([p_str(", "), p_nl_soft], X)
    %
:- func pretty_comma_seperated(list(pretty)) = list(pretty).

    % maybe_pretty_args_maybe_prefix(Prefix, Items) = Pretty.
    %
    % Print a list of items with a prefix (if there are any items) and
    % parens if there are more than one item.
    %
    % [] -> ""
    % [X] -> Prefix ++ X
    % Xs -> pretty_callish(Prefix, Xs)
    %
:- func maybe_pretty_args_maybe_prefix(list(pretty), list(pretty)) =
    pretty.

:- func pretty_string(cord(string)) = string.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
:- implementation.

:- import_module int.
:- import_module maybe.
:- import_module require.
:- import_module string.

:- import_module util.mercury.
:- import_module util.string.

%-----------------------------------------------------------------------%

p_str(String) = p_unit(singleton(String)).
p_cord(Cord) = p_unit(Cord).
p_quote(Q, P) = p_group(g_list, [p_str(Q), P, p_str(Q)]).

p_expr(Pretties) = p_group(g_expr, Pretties).

p_list(Pretties) = p_group(g_list, Pretties).

%-----------------------------------------------------------------------%

default_indent = 2.

max_line = 80.

default_options = options(max_line, default_indent).

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
%   it is in pis_to_output and is stateful.
%
% The main state we're concerned with is the current indentation level and
% column number, these change depending on indentation choices.
%

pretty(Opts, Indent0, Pretties) = Cord :-
    ( if
        Pretties = [Pretty],
        ( Pretty = p_group(_, _)
        ; Pretty = p_group_curly(_, _, _, _)
        )
    then
        DoIndent = no_indent
    else
        DoIndent = may_indent
    ),
    Indent = indent(Indent0, duplicate_char(' ', Indent0)),
    pretty_to_cord_retry(Opts, DoIndent, Indent, g_expr, Pretties, _DidBreak,
        empty_output, Output, Indent0, _),
    Cord = output_to_cord(Output).

pretty(Pretties) = pretty(default_options, 0, Pretties).

pretty_str(Pretties) = append_list(list(pretty(Pretties))).

:- type print_instr
    --->    pi_cord(cord(string))
    ;       pi_nl
            % Start a comment without a newline
    ;       pi_start_comment
    ;       pi_nested(pretty_group_type, list(pretty))
            % Nested pretties with open and close (oc) print instructions
    ;       pi_nested_oc(
                pinoc_first_line    :: list(print_instr),
                pinoc_open          :: list(print_instr),
                pinoc_body          :: list(pretty),
                pinoc_close         :: list(print_instr)
            )
    ;       pi_custom_indent(string, list(print_instr)).

%-----------------------------------------------------------------------%

:- type break
    --->    no_break
    ;       break.

:- func pretty_to_pis(break, pretty) = list(print_instr).

pretty_to_pis(_,     p_unit(Cord)) = [pi_cord(Cord)].
pretty_to_pis(Break, p_group(Type, Pretties)) = Out :-
    ( if
        Pretties = [p_group(TypeInner, InnerPretties)]
    then
        Out = pretty_to_pis(Break, p_group(TypeInner, InnerPretties))
    else if
        Pretties = [G @ p_group_curly(_, _, _, _)]
    then
        Out = pretty_to_pis(Break, G)
    else if
        all [P] (
            member(P, Pretties) =>
            not ( P = p_nl_hard
                ; P = p_nl_soft
                )
        )
    then
        % Don't add an indent if there's no linebreaks in this group.
        Out = condense(map(pretty_to_pis(Break), Pretties))
    else
        Out = [pi_nested(Type, Pretties)]
    ).
pretty_to_pis(Break, p_group_curly(First0, Open, Body, Close)) = Out :-
    ( if any_true(unify(p_nl_soft), Body) then
        unexpected($file, $pred, "Soft linebreak in curly group")
    else
        First = pretty_to_pis(Break, p_expr(First0)),
        Out = [pi_nested_oc(First, [pi_cord(Open)],
            Body, [pi_cord(Close)])]
    ).
pretty_to_pis(Break,    p_comment(Begin, Body)) =
    [pi_custom_indent(
        pretty_string(Begin),
        condense(map(pretty_to_pis(Break), Body)))].
pretty_to_pis(_,        p_spc) = [pi_cord(singleton(" "))].
pretty_to_pis(_,        p_empty) = [].
pretty_to_pis(_,        p_nl_hard) = [pi_nl].
pretty_to_pis(_,        p_nl_double) = [pi_nl, pi_nl].
pretty_to_pis(break,    p_nl_soft) = [pi_nl].
pretty_to_pis(no_break, p_nl_soft) = [].
pretty_to_pis(_,        p_tabstop) = [].

:- type indent_diff
    --->    id_default
    ;       id_rel(int).

%-----------------------------------------------------------------------%

:- type retry_or_commit
            % A choicepoint further up the calltree needs retrying so that
            % it can enable soft breaks.
    --->    fail_if_overrun

            % Our caller has enabled soft breaks, we may retry if we need to
            % do the same.
    ;       may_break_lines.

    % Whether or not a newline was "printed".
    %
:- type did_break
    --->    did_break
    ;       did_not_break.

:- type indent
    --->    indent(
                i_pos       :: int,
                i_string    :: string
            ).

:- pred pis_to_output(options::in, retry_or_commit::in, indent::in,
    list(print_instr)::in, output_builder::in,
    maybe(output_builder)::out, did_break::out, int::in, int::out) is det.

pis_to_output(_, _, _, [], Output, yes(Output), did_not_break, !Pos).
pis_to_output(Opts, RoC, Indent, [Pi | Pis], !.Output, MaybeOutput, DidBreak,
        !Pos) :-
    ( Pi = pi_cord(New),
        !:Pos = !.Pos + cord_string_len(New),
        ( if
            !.Pos > Opts ^ o_max_line,
            % We only fail here if our caller is prepared to handle it.
            RoC = fail_if_overrun
        then
            MaybeOutput = no,
            DidBreak = did_not_break
        else
            output_add_new(New, !Output),
            pis_to_output(Opts, RoC, Indent, Pis, !.Output, MaybeOutput,
                DidBreak, !Pos)
        )
    ;
        ( Pi = pi_nl,
            output_newline(Indent ^ i_string, !Output),
            DidBreak = did_break
        ; Pi = pi_start_comment,
            output_start_comment(Indent ^ i_string, DidBreak, !Output)
        ),
        !:Pos = Indent ^ i_pos,
        pis_to_output(Opts, RoC, Indent, Pis, !.Output, MaybeOutput, _, !Pos)
    ; Pi = pi_nested(Type, Pretties),
        chain_op([
                pis_to_output_nested(Opts, RoC, Type, Indent, may_indent,
                    Pretties),
                pis_to_output(Opts, RoC, Indent, Pis)
            ], !.Output, MaybeOutput, DidBreak, !Pos)
    ; Pi = pi_nested_oc(First, Open, Nested, Close),
        PosOpen = !.Pos,
        chain_op([
            (pred(O::in, MO::out, DB::out, Pos0::in, Pos::out) is det :-
                pis_to_output(Opts, RoC, Indent, First, O,
                    MaybeFirst, FirstDidBreak, Pos0, Pos1),
                ( MaybeFirst = no,
                    MO = no,
                    DB = FirstDidBreak,
                    Pos = Pos1
                ; MaybeFirst = yes(FirstOutput),
                    ( FirstDidBreak = did_break,
                        MaybeNL = pi_nl
                    ; FirstDidBreak = did_not_break,
                        MaybeNL = pi_cord(singleton(" "))
                    ),
                    pis_to_output(Opts, RoC, Indent, [MaybeNL] ++ Open,
                        FirstOutput, MO, OpenDidBreak, Pos1, Pos),
                    DB = did_break_combine(FirstDidBreak, OpenDidBreak)
                )
            ),
            (pred(O0::in, MO::out, DB::out, _::in, Pos::out) is det :-
                ( if PosOpen > Indent ^ i_pos then
                    NewLevel = PosOpen + Opts ^ o_indent
                else
                    NewLevel = Indent ^ i_pos + Opts ^ o_indent
                ),
                move_indent(NewLevel, Indent, SubIndent),

                output_newline(SubIndent ^ i_string, O0, O),
                Pos1 = SubIndent ^ i_pos,
                pis_to_output_nested(Opts, RoC, g_expr, SubIndent, no_indent,
                    Nested, O, MO, DB, Pos1, Pos)
            ),
            pis_to_output(Opts, RoC, Indent, [pi_nl] ++ Close),
            pis_to_output(Opts, RoC, Indent, Pis)
          ], !.Output, MaybeOutput, DidBreak, !Pos)
    ; Pi = pi_custom_indent(Begin, PisIndent),
        IndentCustom = indent(Indent ^ i_pos + length(Begin),
            Indent ^ i_string ++ Begin),
        chain_op([
                pis_to_output(Opts, RoC, IndentCustom,
                    [pi_start_comment] ++ PisIndent),
                pis_to_output(Opts, RoC, Indent, Pis)
            ], !.Output, MaybeOutput, DidBreak, !Pos)
    ).

:- pred pis_to_output_nested(options::in, retry_or_commit::in,
    pretty_group_type::in, indent::in, may_indent::in, list(pretty)::in,
    output_builder::in, maybe(output_builder)::out, did_break::out,
    int::in, int::out) is det.

pis_to_output_nested(Opts, RoC, Type, Indent0, MayIndent, Pretties, !.Output,
        MaybeOutput, DidBreak, !Pos) :-
    ( RoC = may_break_lines,
        pretty_to_cord_retry(Opts, MayIndent, Indent0, Type, Pretties,
            DidBreak, !Output, !Pos),
        MaybeOutput = yes(!.Output)
    ; RoC = fail_if_overrun,
        ( MayIndent = may_indent,
            find_and_add_indent(Opts, no_break, Type, Pretties, !.Pos,
                Indent0, Indent)
        ; MayIndent = no_indent,
            Indent = Indent0
        ),
        InstrsBreak = map(pretty_to_pis(no_break), Pretties),
        pis_to_output(Opts, RoC, Indent, condense(InstrsBreak),
            !.Output, MaybeOutput, DidBreak, !Pos)
    ).

:- type may_indent
    --->    may_indent
    ;       no_indent.

:- pred pretty_to_cord_retry(options::in, may_indent::in, indent::in,
    pretty_group_type::in, list(pretty)::in, did_break::out,
    output_builder::in, output_builder::out, int::in, int::out) is det.

pretty_to_cord_retry(Opts, MayIndent, Indent0, Type, Pretties, DidBreak,
        !Output, !Pos) :-
    ( MayIndent = may_indent,
        find_and_add_indent(Opts, no_break, Type, Pretties, !.Pos, Indent0,
            IndentA)
    ; MayIndent = no_indent,
        IndentA = Indent0
    ),
    InstrsNoBreak = map(pretty_to_pis(no_break), Pretties),
    % Without breaking on soft breaks, can we format all this code without
    % overrunning a line?
    pis_to_output(Opts, fail_if_overrun, IndentA, condense(InstrsNoBreak),
        !.Output, MaybeOutput0, DidBreakA, !.Pos, PosNoBreak),
    ( MaybeOutput0 = yes(!:Output),
        !:Pos = PosNoBreak,
        DidBreak = DidBreakA
    ; MaybeOutput0 = no,
        % We can't so retry with soft breaks.
        ( MayIndent = may_indent,
            find_and_add_indent(Opts, no_break, Type, Pretties, !.Pos,
                Indent0, IndentB)
        ; MayIndent = no_indent,
            IndentB = Indent0
        ),
        InstrsBreak = map(pretty_to_pis(break), Pretties),
        pis_to_output(Opts, may_break_lines, IndentB, condense(InstrsBreak),
            !.Output, MaybeOutput1, DidBreakB, !Pos),
        DidBreak = DidBreakB,
        ( MaybeOutput1 = no,
            unexpected($file, $pred, "Fallback failed")
        ; MaybeOutput1 = yes(!:Output)
        )
    ).

:- func did_break_combine(did_break, did_break) = did_break.

did_break_combine(did_not_break, did_not_break) = did_not_break.
did_break_combine(did_not_break, did_break)     = did_break.
did_break_combine(did_break,     _)             = did_break.

%-----------------------------------------------------------------------%

:- pred find_and_add_indent(options::in, break::in,
    pretty_group_type::in, list(pretty)::in, int::in,
    indent::in, indent::out) is det.

find_and_add_indent(Opts, Break, g_expr, Pretties, Pos, !Indent) :-
    find_indent(Break, Pretties, 0, FoundIndent),
    ( FoundIndent = id_default,
        ( if !.Indent ^ i_pos + Opts ^ o_indent > Pos then
            NewLevel = !.Indent ^ i_pos + Opts ^ o_indent
        else
            NewLevel = Pos + Opts ^ o_indent
        )
    ; FoundIndent = id_rel(Rel),
        NewLevel = Pos + Rel
    ),
    move_indent(NewLevel, !Indent).
find_and_add_indent(_,    _,     g_list, _,        Pos, !Indent) :-
    ( if !.Indent ^ i_pos =< Pos then
        move_indent(Pos, !Indent)
    else
        true
    ).

:- pred find_indent(break::in, list(pretty)::in, int::in, indent_diff::out)
    is det.

find_indent(_,     [],       _,   id_default).
find_indent(Break, [P | Ps], Acc, Indent) :-
    ( P = p_unit(Cord),
        find_indent(Break, Ps, Acc + cord_string_len(Cord), Indent)
    ; P = p_spc,
        find_indent(Break, Ps, Acc + 1, Indent)
    ; P = p_empty,
        find_indent(Break, Ps, Acc, Indent)
    ;
        ( P = p_nl_hard
        ; P = p_nl_double
        ; P = p_nl_soft
        ),
        Indent = id_default,
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
        Indent = id_rel(Acc),
        ( if
            some [B] (
                member(B, Ps), ( B = p_nl_hard ; B = p_nl_soft )
            )
        then
            true
        else
            unexpected($file, $pred, "tabstop not followed by newline")
        )
    ; P = p_group(Type, Pretties),
        FoundBreak = single_line_len(Break, Pretties, 0),
        ( FoundBreak = found_break,
            % If there was an (honored) break in the inner group the
            % outer group has a fixed indent of "offset"
            ( Type = g_expr,
                Indent = id_default
            ; Type = g_list,
                Indent = id_rel(0)
            )
        ; FoundBreak = single_line(Len),
            % But if the inner group had no breaks then the search for the
            % outer group's tabstop continues.
            % We can pass Break directly since if we didn't break outside
            % the group then the group is very likely to have the same break
            % when it set-out. (Not a guarantee but a good guess).
            find_indent(Break, Ps, Acc + Len, Indent)
        )
    ; P = p_group_curly(_, _, _, _),
        % We always use the default indents for curly groups.
        Indent = id_default
    ; P = p_comment(Begin, Body),
        find_indent(Break, Body, Acc + cord_string_len(Begin), Indent)
    ).

:- type single_line_len
    --->    found_break
            % When there was no break this returns the total length.
    ;       single_line(int).

:- func single_line_len(break, list(pretty), int) = single_line_len.

single_line_len(_, [], Acc) = single_line(Acc).
single_line_len(Break, [P | Ps], Acc) = FoundBreak :-
    ( P = p_unit(Cord),
        FoundBreak = single_line_len(Break, Ps, Acc + cord_string_len(Cord))
    ; P = p_spc,
        FoundBreak = single_line_len(Break, Ps, Acc + 1)
    ; P = p_empty,
        FoundBreak = single_line_len(Break, Ps, Acc)
    ;
        ( P = p_nl_hard
        ; P = p_nl_double
        ),
        FoundBreak = found_break
    ; P = p_nl_soft,
        ( Break = break,
            FoundBreak = found_break
        ; Break = no_break,
            FoundBreak = single_line_len(Break, Ps, Acc + 1)
        )
    ; P = p_tabstop,
        FoundBreak = single_line_len(Break, Ps, Acc)
    ; P = p_group(_, Pretties),
        FoundBreak = single_line_len(Break, Pretties ++ Ps, Acc)
    ; P = p_group_curly(_, _, _, _),
        unexpected($file, $pred, "I don't think this makes sense")
    ; P = p_comment(Begin, Body),
        FoundBreak = single_line_len(Break, Body, Acc +
            cord_string_len(Begin))
    ).

%-----------------------------------------------------------------------%

    % chain_op(Ops, Input, MaybeOutput, DidBreak, !B),
    %
    % Perform each operation in Ops (so long as they return yes(_), passing
    % the output of each to the input of the next, and threading the states
    % !A and !B.
    %
:- pred chain_op(
    list(pred(A, maybe(A), did_break, C, C)),
    A, maybe(A), did_break, C, C).
:- mode chain_op(
    in(list(pred(in, out, out, in, out) is det)),
    in, out, out, in, out) is det.

chain_op([], Cord, yes(Cord), did_not_break, !Pos).
chain_op([Op | Ops], Cord0, MaybeCord, DidBreak, !Pos) :-
    Op(Cord0, MaybeCord0, DidBreakA, !Pos),
    ( MaybeCord0 = no,
        MaybeCord = no,
        DidBreak = DidBreakA
    ; MaybeCord0 = yes(Cord),
        chain_op(Ops, Cord, MaybeCord, DidBreakB, !Pos),
        DidBreak = did_break_combine(DidBreakA, DidBreakB)
    ).

%-----------------------------------------------------------------------%

:- pred move_indent(int::in, indent::in, indent::out) is det.

move_indent(NewLevel, Indent0, Indent) :-
    RelLevel = NewLevel - Indent0 ^ i_pos,
    String = Indent0 ^ i_string ++ duplicate_char(' ', RelLevel),
    Indent = indent(NewLevel, String).

%-----------------------------------------------------------------------%

    % Use this type to build the output
    %
:- type output_builder
    --->    output(
                output      :: cord(string),
                last_line   :: cord(string)
            ).

:- func empty_output = output_builder.

empty_output = output(empty, empty).

:- func output_to_cord(output_builder) = cord(string).

output_to_cord(!.Output) = Cord :-
    output_end_line(!Output),
    Cord = !.Output ^ output.

:- pred output_end_line(output_builder::in, output_builder::out) is det.

output_end_line(!Output) :-
    ( if is_empty(!.Output ^ output) then
        Prev = init
    else
        Prev = !.Output ^ output ++ singleton("\n")
    ),
    LastLine = trim_line(!.Output ^ last_line),
    !Output ^ output := Prev ++ singleton(LastLine),
    !Output ^ last_line := init.

:- pred output_add_new(cord(string)::in,
    output_builder::in, output_builder::out) is det.

output_add_new(New, !Output) :-
    !Output ^ last_line := !.Output ^ last_line ++ New.

:- pred output_newline(string::in,
    output_builder::in, output_builder::out) is det.

output_newline(Indent, !Output) :-
    output_end_line(!Output),
    !Output ^ last_line := singleton(Indent).

:- pred output_start_comment(string::in, did_break::out,
    output_builder::in, output_builder::out) is det.

output_start_comment(Indent, DidBreak, !Output) :-
    ( if is_output_line_empty(!.Output) then
        % Reset the indent without a newline.
        !Output ^ last_line := singleton(Indent),
        DidBreak = did_break
    else
        % Do a normal newline
        output_newline(Indent, !Output),
        DidBreak = did_not_break
    ).

:- pred is_output_line_empty(output_builder::in) is semidet.

is_output_line_empty(Output) :-
    LastLine = trim_line(Output ^ last_line),
    LastLine = "".

:- func trim_line(cord(string)) = string.

trim_line(Line) = rstrip(append_list(list(Line))).

%-----------------------------------------------------------------------%

:- func cord_string_len(cord(string)) = int.

cord_string_len(Cord) =
    foldl(func(S, L) = length(S) + L, Cord, 0).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

pretty_callish(Prefix, Args) = Pretty :-
    ( if
        % If the prefix is sagnificant and either...
        is_sagnificant(Prefix),
        (
            % there's only one argument being formatted or...
            Args = [_]
        ;
            % at least one argument is sagnificant...
            all [Arg] (
                member(Arg, Args),
                is_sagnificant(Arg)
            )
        )
    then
        % then add a break after the opening paren.
        MaybeBreak = [p_nl_soft]
    else
        MaybeBreak = []
    ),
    Pretty = p_expr([Prefix, p_str("(")] ++ MaybeBreak ++
        [p_list(pretty_comma_seperated(Args)),
        p_str(")")]).

:- pred is_sagnificant(pretty::in) is semidet.

is_sagnificant(Pretty) :-
    SLLen = single_line_len(no_break, [Pretty], 0),
    ( SLLen = found_break
    ; SLLen = single_line(Len),
        Len > default_indent*3
    ).

pretty_optional_args(Prefix, []) = p_expr([Prefix]).
pretty_optional_args(Prefix, Args@[_ | _]) = pretty_callish(Prefix, Args).

pretty_seperated(Sep, Items) = list_join(Sep, Items).

pretty_comma_seperated(Items) =
    pretty_seperated([p_str(", "), p_nl_soft], Items).

maybe_pretty_args_maybe_prefix(_, []) = p_empty.
maybe_pretty_args_maybe_prefix(Prefix, [X]) = p_expr(Prefix ++ [X]).
maybe_pretty_args_maybe_prefix(Prefix, Xs@[_, _ | _]) =
    pretty_callish(p_expr(Prefix), Xs).

%-----------------------------------------------------------------------%

pretty_string(Cord) = append_list(list(Cord)).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
