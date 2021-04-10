%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module pz.pretty.
%
% PZ pretty printer
%
% Copyright (C) 2015-2021 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
%-----------------------------------------------------------------------%
:- interface.

:- import_module cord.
:- import_module string.

:- func pz_pretty(pz) = cord(string).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
:- implementation.

:- import_module int.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module uint32.

:- import_module context.
:- import_module pz.code.
:- import_module util.
:- import_module util.pretty_old.
:- import_module q_name.

%-----------------------------------------------------------------------%

pz_pretty(PZ) =
        condense(ModuleDeclsPretty) ++ nl ++
        condense(ImportsPretty) ++ nl ++
        condense(StructsPretty) ++ nl ++
        condense(DataPretty) ++ nl ++
        condense(ProcsPretty) ++ nl ++
        condense(ClosuresPretty) ++ nl :-
    ModuleDeclsPretty = from_list(map(module_decl_pretty,
        pz_get_module_names(PZ))),
    ImportsPretty = from_list(map(import_pretty, pz_get_imports(PZ))),
    StructsPretty = from_list(map(struct_pretty, pz_get_structs(PZ))),
    DataPretty = from_list(map(data_pretty, pz_get_data_items(PZ))),
    ProcsPretty = from_list(map(proc_pretty(PZ), pz_get_procs(PZ))),
    ClosuresPretty = from_list(map(closure_pretty(PZ),
        pz_get_closures(PZ))).

:- func module_decl_pretty(q_name) = cord(string).

module_decl_pretty(Name) =
    cord.from_list(["module ", q_name_to_string(Name)]) ++ nl.

%-----------------------------------------------------------------------%

:- func import_pretty(pair(pzi_id, q_name)) = cord(string).

import_pretty(IID - Name) =
    from_list(["import ", string(pzi_id_get_num(IID)), " ",
        q_name_to_string(Name), ";\n"]).

%-----------------------------------------------------------------------%

:- func struct_pretty(pair(pzs_id, pz_named_struct)) = cord(string).

struct_pretty(SID - pz_named_struct(Name, pz_struct(Fields))) = String :-
    SIDNum = pzs_id_get_num(SID),

    String = from_list(["struct ", Name, "_", string(SIDNum), " = { "]) ++
        join(comma ++ spc, map(width_pretty, Fields)) ++ singleton(" }\n").

%-----------------------------------------------------------------------%

:- func data_pretty(pair(pzd_id, pz_data)) = cord(string).

data_pretty(DID - pz_data(Type, Values)) = String :-
    DIDNum = pzd_id_get_num(DID),
    DeclStr = format("data d%d = ", [i(cast_to_int(DIDNum))]),

    TypeStr = data_type_pretty(Type),

    DataStr = singleton("{ ") ++ join(spc,
            map(data_value_pretty, Values)) ++
        singleton(" }"),

    String = singleton(DeclStr) ++ TypeStr ++ spc ++ DataStr ++ semicolon ++ nl.

:- func data_type_pretty(pz_data_type) = cord(string).

data_type_pretty(type_array(Width, _)) = cons("array(",
    snoc(width_pretty(Width), ")")).
data_type_pretty(type_struct(StructId)) = singleton(StructName) :-
    StructName = format("struct_%d",
        [i(cast_to_int(pzs_id_get_num(StructId)))]).

:- func data_value_pretty(pz_data_value) = cord(string).

data_value_pretty(pzv_num(Num)) =
    singleton(string(Num)).
data_value_pretty(Value) =
        singleton(format("%s%i", [s(Label), i(cast_to_int(IdNum))])) :-
    ( Value = pzv_data(DID),
        Label = "d",
        IdNum = pzd_id_get_num(DID)
    ; Value = pzv_import(IID),
        Label = "i",
        IdNum = pzi_id_get_num(IID)
    ; Value = pzv_closure(CID),
        Label = "c",
        IdNum = pzc_id_get_num(CID)
    ).

%-----------------------------------------------------------------------%

:- func proc_pretty(pz, pair(pzp_id, pz_proc)) = cord(string).

proc_pretty(PZ, PID - Proc) = String :-
    Name = pretty_proc_name(PID, Proc),
    Inputs = Proc ^ pzp_signature ^ pzs_before,
    Outputs = Proc ^ pzp_signature ^ pzs_after,
    ParamsStr = join(spc, map(width_pretty, Inputs)) ++
        singleton(" - ") ++
        join(spc, map(width_pretty, Outputs)),

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

:- func pretty_proc_name(pzp_id, pz_proc) = string.

pretty_proc_name(PID, Proc) =
    format("%s_%d", [s(q_name_to_string(Proc ^ pzp_name)),
        i(cast_to_int(pzp_id_get_num(PID)))]).

:- pred pretty_block_with_name(pz::in, pz_block::in, cord(string)::out,
    int::in, int::out) is det.

pretty_block_with_name(PZ, pz_block(Instrs), String, !Num) :-
    String = indent(2) ++ singleton(format("block b%d {\n", [i(!.Num)])) ++
        pretty_instrs(PZ, 4, Instrs) ++
        indent(2) ++ singleton("}\n"),
    !:Num = !.Num + 1.

:- func pretty_block(pz, pz_block) = cord(string).

pretty_block(PZ, pz_block(Instrs)) = pretty_instrs(PZ, 2, Instrs).

:- func pretty_instrs(pz, int, list(pz_instr_obj)) = cord(string).

pretty_instrs(_, _, []) = init.
pretty_instrs(PZ, Indent, [Instr | Instrs]) =
    indent(Indent) ++ pretty_instr_obj(PZ, Instr) ++ nl ++
        pretty_instrs(PZ, Indent, Instrs).

:- func pretty_instr_obj(pz, pz_instr_obj) = cord(string).

pretty_instr_obj(PZ, pzio_instr(Instr)) = pretty_instr(PZ, Instr).
pretty_instr_obj(_, pzio_context(PZContext)) = Pretty :-
    ( PZContext = pz_context(Context, _),
        Pretty = comment ++ singleton(context_string(Context))
    ; PZContext = pz_context_short(Line),
        Pretty = comment ++ singleton(":" ++ string(Line))
    ; PZContext = pz_nil_context,
        Pretty = empty
    ).
pretty_instr_obj(_, pzio_comment(Comment)) =
    comment ++ singleton(Comment).

:- func pretty_instr(pz, pz_instr) = cord(string).

pretty_instr(PZ, Instr) = String :-
    ( Instr = pzi_load_immediate(Width, Value),
        (
            ( Value = im_i8(Num),
                NumStr = string(Num)
            ; Value = im_u8(Num),
                NumStr = string(Num)
            ; Value = im_i16(Num),
                NumStr = string(Num)
            ; Value = im_u16(Num),
                NumStr = string(Num)
            ; Value = im_i32(Num),
                NumStr = string(Num)
            ; Value = im_u32(Num),
                NumStr = string(Num)
            ; Value = im_i64(Num),
                NumStr = string(Num)
            ; Value = im_u64(Num),
                NumStr = string(Num)
            ),
            String = singleton(NumStr) ++ colon ++ width_pretty(Width)
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
            width_pretty(Width1) ++ comma ++ width_pretty(Width2)
    ;
        ( Instr = pzi_add(Width),
            Name = "add"
        ; Instr = pzi_sub(Width),
            Name = "sub"
        ; Instr = pzi_mul(Width),
            Name = "mul"
        ; Instr = pzi_div(Width),
            Name = "div"
        ; Instr = pzi_mod(Width),
            Name = "mod"
        ; Instr = pzi_lshift(Width),
            Name = "lshift"
        ; Instr = pzi_rshift(Width),
            Name = "rshift"
        ; Instr = pzi_and(Width),
            Name = "and"
        ; Instr = pzi_or(Width),
            Name = "or"
        ; Instr = pzi_xor(Width),
            Name = "xor"
        ; Instr = pzi_lt_u(Width),
            Name = "lt_u"
        ; Instr = pzi_lt_s(Width),
            Name = "lt_s"
        ; Instr = pzi_gt_u(Width),
            Name = "gt_u"
        ; Instr = pzi_gt_s(Width),
            Name = "gt_s"
        ; Instr = pzi_eq(Width),
            Name = "eq"
        ; Instr = pzi_not(Width),
            Name = "not"
        ; Instr = pzi_cjmp(Dest, Width),
            Name = format("cjmp b%d", [i(cast_to_int(Dest))])
        ),
        String = singleton(Name) ++ colon ++ width_pretty(Width)
    ;
        ( Instr = pzi_call(Callee),
            InstrName = "call"
        ; Instr = pzi_tcall(Callee),
            InstrName = "tcall"
        ),
        ( Callee = pzc_closure(CID),
            CalleeName = format("closure_%d", [i(
                cast_to_int(pzc_id_get_num(CID)))])
        ;
            ( Callee = pzc_import(IID),
                CalleeSym = pz_lookup_import(PZ, IID)
            ; Callee = pzc_proc_opt(PID),
                CalleeSym = pz_lookup_proc(PZ, PID) ^ pzp_name
            ),
            CalleeName = q_name_to_string(CalleeSym)
        ),
        String = singleton(InstrName) ++ spc ++ singleton(CalleeName)
    ;
        ( Instr = pzi_drop,
            Name = "drop"
        ; Instr = pzi_call_ind,
            Name = "call_ind"
        ; Instr = pzi_tcall_ind,
            Name = "tcall_ind"
        ; Instr = pzi_jmp(Dest),
            Name = format("jmp %d", [i(cast_to_int(Dest))])
        ; Instr = pzi_ret,
            Name = "ret"
        ; Instr = pzi_get_env,
            Name = "get_env"
        ),
        String = singleton(Name)
    ;
        ( Instr = pzi_roll(N),
            Name = "roll "
        ; Instr = pzi_pick(N),
            Name = "pick "
        ),
        String = singleton(Name) ++ singleton(string(N))
    ; Instr = pzi_alloc(Struct),
        String = singleton(format("alloc struct_%d",
            [i(cast_to_int(pzs_id_get_num(Struct)))]))
    ; Instr = pzi_make_closure(Proc),
        String = singleton(format("make_closure_%d",
            [i(cast_to_int(pzp_id_get_num(Proc)))]))
    ;
        ( Instr = pzi_load(Struct, Field, Width),
            Name = "load"
        ; Instr = pzi_store(Struct, Field, Width),
            Name = "store"
        ),
        String = singleton(Name) ++ colon ++ width_pretty(Width) ++ spc ++
            singleton(string(pzs_id_get_num(Struct))) ++ spc ++
            singleton(string(Field))
    ; Instr = pzi_load_named(ImportId, Width),
        String = singleton("load_named") ++ colon ++ width_pretty(Width) ++
            spc ++ singleton("import_") ++
            singleton(string(pzi_id_get_num(ImportId)))
    ).

%-----------------------------------------------------------------------%

:- func closure_pretty(pz, pair(pzc_id, pz_closure)) = cord(string).

closure_pretty(PZ, Id - pz_closure(ProcId, DataId)) =
        CloPretty ++ EntryPretty :-
    Proc = pz_lookup_proc(PZ, ProcId),
    ProcName = pretty_proc_name(ProcId, Proc),
    DataName = format("d%d", [i(cast_to_int(pzd_id_get_num(DataId)))]),
    CloPretty = ExportPretty ++ from_list(
        ["closure ", Name, " = ", ProcName, " ", DataName, ";\n"]),
    ( if
        find_first_match((pred((_ - EId)::in) is semidet :-
                Id = EId
            ), pz_get_exports(PZ), Export),
        Export = ExportName0 - Id
    then
        ExportPretty = singleton("export "),
        Name = q_name_to_string(ExportName0)
    else
        ExportPretty = init,
        Name = format("clo_%d", [i(cast_to_int(pzc_id_get_num(Id)))])
    ),
    ( if
        ( if
            pz_get_maybe_entry_closure(PZ) = yes(Entry),
            Id = Entry ^ pz_ep_closure
        then
            Type = "default "
        else if
            member(Entry, pz_get_entry_candidates(PZ)),
            Id = Entry ^ pz_ep_closure
        then
            Type = "candidate "
        else
            false
        )
    then
        EntryPretty = from_list(["entry ", Type, Name, ";\n"])
    else
        EntryPretty = init
    ).

%-----------------------------------------------------------------------%

:- func width_pretty(pz_width) = cord(string).

width_pretty(Width) = singleton(width_pretty_str(Width)).

:- func width_pretty_str(pz_width) = string.

width_pretty_str(pzw_8)    = "w8".
width_pretty_str(pzw_16)   = "w16".
width_pretty_str(pzw_32)   = "w32".
width_pretty_str(pzw_64)   = "w64".
% TODO: check that these match what the parser expects, standardize on some
% names for these throughout the system.
width_pretty_str(pzw_fast) = "w".
width_pretty_str(pzw_ptr)  = "ptr".

:- func comment = cord(string).

comment = singleton("// ").

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
