%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module pz.pretty.
%
% PZ pretty printer
%
% Copyright (C) 2015 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
%-----------------------------------------------------------------------%
:- interface.

:- import_module cord.
:- import_module string.

:- func pz_pretty(pz) = cord(string).

%-----------------------------------------------------------------------%
:- implementation.

:- import_module require.

:- import_module symtab.

pz_pretty(PZ) = condense(DataPretty) ++ nl ++ condense(ProcsPretty) :-
    DataPretty = from_list(map(data_pretty(PZ), pz_get_data_items(PZ))),
    ProcsPretty = from_list(map(proc_pretty(PZ), pz_get_procs(PZ))).

%-----------------------------------------------------------------------%

:- func data_pretty(pz, pair(pzd_id, pz_data)) = cord(string).

data_pretty(PZ, DID - pz_data(Type, Data)) = String :-
    DIDNum = pzd_id_get_num(PZ, DID),
    DeclStr = format("data d%d = ", [i(DIDNum)]),

    TypeStr = data_type_pretty(Type),

    DataStr = data_value_pretty(PZ, Data),

    String = singleton(DeclStr) ++ TypeStr ++ spc ++ DataStr ++ semicolon ++ nl.

:- func data_type_pretty(pz_data_type) = cord(string).

data_type_pretty(type_basic(Width)) = width_pretty(Width).
data_type_pretty(type_array(Width)) = cons("array(",
    snoc(width_pretty(Width), ")")).
data_type_pretty(type_struct(_)) = sorry($file, $pred, "struct").

:- func width_pretty(pz_data_width) = cord(string).

width_pretty(w8) = singleton("w8").
width_pretty(w16) = singleton("w16").
width_pretty(w32) = singleton("w32").
width_pretty(w64) = singleton("w64").
width_pretty(w_fast) = singleton("w_fast").
width_pretty(w_ptr) = singleton("w_ptr").
width_pretty(ptr) = singleton("ptr").

:- func data_value_pretty(pz, pz_data_value) = cord(string).

data_value_pretty(_, pzv_num(Num)) = singleton(string(Num)).
data_value_pretty(_, pzv_sequence(Nums)) =
    singleton("{ ") ++ singleton(join_list(" ", map(string, Nums))) ++
    singleton(" }").
data_value_pretty(PZ, pzv_data(DID)) =
    singleton(format("d%i", [i(pzd_id_get_num(PZ, DID))])).

%-----------------------------------------------------------------------%

:- func proc_pretty(pz, pair(pzp_id, pz_proc)) = cord(string).

proc_pretty(PZ, PID - Proc) = String :-
    Name = format("%s_%d",
        [s(symbol_to_string(Proc ^ pzp_name)), i(pzp_id_get_num(PZ, PID))]),
    Inputs = Proc ^ pzp_signature ^ pzs_before,
    Outputs = Proc ^ pzp_signature ^ pzs_after,
    ParamsStr = join(" ", cord_list_to_cord(map(width_pretty, Inputs))) ++
        singleton(" - ") ++
        join(" ", cord_list_to_cord(map(width_pretty, Outputs))),

    DeclStr = singleton("proc ") ++ singleton(Name) ++ singleton(" (") ++
        ParamsStr ++ singleton(")"),

    MaybeBlocks = Proc ^ pzp_blocks,
    ( MaybeBlocks = yes(Blocks),
        ( Blocks = [],
            unexpected($file, $pred, "no blocks")
        ; Blocks = [Block],
            BlocksStr = pretty_block(PZ, Block)
        ; Blocks = [_, _ | _],
            map_foldl(pretty_block_with_name(PZ), Blocks, BlocksStr0, 0, _),
            BlocksStr = cord_list_to_cord(BlocksStr0)
        ),
        BodyStr = singleton(" {\n") ++ BlocksStr ++ singleton("}")
    ; MaybeBlocks = no,
        BodyStr = init
    ),

    String = DeclStr ++ BodyStr ++ semicolon ++ nl ++ nl.

:- pred pretty_block_with_name(pz::in, pz_block::in, cord(string)::out,
    int::in, int::out) is det.

pretty_block_with_name(PZ, pz_block(Instrs), String, !Num) :-
    String = indent(1) ++ singleton(format("block_%d {\n", [i(!.Num)])) ++
        pretty_instrs(PZ, 2, Instrs) ++
        indent(1) ++ singleton("}\n"),
    !:Num = !.Num + 1.

:- func pretty_block(pz, pz_block) = cord(string).

pretty_block(PZ, pz_block(Instrs)) = pretty_instrs(PZ, 1, Instrs).

:- func pretty_instrs(pz, int, list(pz_instr)) = cord(string).

pretty_instrs(_, _, []) = init.
pretty_instrs(PZ, Indent, [Instr | Instrs]) =
    indent(Indent) ++ pretty_instr(PZ, Instr) ++ nl ++
        pretty_instrs(PZ, Indent, Instrs).

:- func pretty_instr(pz, pz_instr) = cord(string).

pretty_instr(PZ, Instr) = String :-
    ( Instr = pzi_load_immediate(Width, Value),
        (
            ( Value = immediate8(_)
            ; Value = immediate16(_)
            ; Value = immediate32(_)
            ; Value = immediate64(_, _)
            ),
            (
                ( Value = immediate8(Num)
                ; Value = immediate16(Num)
                ; Value = immediate32(Num)
                ),
                NumStr = singleton(string(Num))
            ; Value = immediate64(High, Low),
                NumStr = singleton(format("%d<<32+%d", [i(High),i(Low)]))
            ),
            String = NumStr ++ singleton(":") ++ operand_width_pretty(Width)
        ; Value = immediate_data(DID),
            String = singleton(format("d%d", [i(pzd_id_get_num(PZ, DID))]))
        ; Value = immediate_code(PID),
            String = singleton(format("proc_%d",
                [i(pzp_id_get_num(PZ, PID))]))
        ; Value = immediate_label(Num),
            String = singleton(format("label_%d", [i(Num)]))
        )
    ;
        ( Instr = pzi_ze(Width1, Width2),
            Name = "ze"
        ; Instr = pzi_se(Width1, Width2),
            Name = "se"
        ; Instr = pzi_trunc(Width1, Width2),
            Name = "trunc"
        ),
        String = singleton(Name) ++ colon ++
            operand_width_pretty(Width1) ++ comma ++
            operand_width_pretty(Width2)
    ;
        ( Instr = pzi_add(Width),
            Name = "add"
        ; Instr = pzi_sub(Width),
            Name = "sub"
        ; Instr = pzi_mul(Width),
            Name = "mul"
        ; Instr = pzi_div(Width),
            Name = "div"
        ; Instr = pzi_lt_u(Width),
            Name = "lt_u"
        ; Instr = pzi_lt_s(Width),
            Name = "lt_s"
        ; Instr = pzi_gt_u(Width),
            Name = "gt_u"
        ; Instr = pzi_gt_s(Width),
            Name = "gt_s"
        ; Instr = pzi_cjmp(Dest, Width),
            Name = format("cjmp(%d)", [i(Dest)])
        ),
        String = singleton(Name) ++ colon ++
            operand_width_pretty(Width)
    ;
        ( Instr = pzi_dup,
            Name = "dup"
        ; Instr = pzi_drop,
            Name = "drop"
        ; Instr = pzi_swap,
            Name = "swap"
        ; Instr = pzi_call(PID),
            Name = format("proc_%d", [i(pzp_id_get_num(PZ, PID))])
        ; Instr = pzi_ret,
            Name = "ret"
        ),
        String = singleton(Name)
    ;
        ( Instr = pzi_roll(N),
            Name = "roll "
        ; Instr = pzi_pick(N),
            Name = "pick "
        ),
        String = singleton(Name) ++ singleton(string(N))
    ).

:- func operand_width_pretty(pzf_operand_width) = cord(string).

operand_width_pretty(_) = init.

%-----------------------------------------------------------------------%

:- func join(T, cord(T)) = cord(T).

join(Join, Cord0) = Cord :-
    ( if
        head_tail(Cord0, Head, Tail),
        not is_empty(Tail)
    then
        Cord = cons(Head, cons(Join, join(Join, Tail)))
    else
        Cord = Cord0
    ).

:- func nl = cord(string).
nl = singleton("\n").

:- func spc = cord(string).
spc = singleton(" ").

:- func semicolon = cord(string).
semicolon = singleton(";").

:- func colon = cord(string).
colon = singleton(":").

:- func comma = cord(string).
comma = singleton(",").

:- func indent(int) = cord(string).
indent(N) =
    ( if N = 0 then
        init
    else
        singleton("    ") ++ indent(N-1)
    ).

