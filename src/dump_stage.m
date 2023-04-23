%-----------------------------------------------------------------------%
% Dump stages utility
% vim: ts=4 sw=4 et
%
% Copyright (C) Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
% A utility predicate to dump intermediate compiler stages.
%
%-----------------------------------------------------------------------%
:- module dump_stage.
%-----------------------------------------------------------------------%

:- interface.

:- import_module cord.
:- import_module io.

:- import_module options.
:- import_module q_name.

%-----------------------------------------------------------------------%

:- pred maybe_dump_stage(general_options, q_name, string,
    func(D) = cord(string), D, io, io).
:- mode maybe_dump_stage(in, in, in,
    func(in) = (out) is det, in, di, uo) is det.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module string.

%-----------------------------------------------------------------------%

maybe_dump_stage(GeneralOpts, ModuleName, Stage, Format, Data, !IO) :-
    DumpStages = GeneralOpts ^ go_dump_stages,
    ( DumpStages = dump_stages,
        dump_stage(GeneralOpts, Stage, ModuleName,
            append_list(list(Format(Data))), !IO)
    ; DumpStages = dont_dump_stages
    ).

:- pred dump_stage(general_options::in, string::in, q_name::in, string::in,
    io::di, io::uo) is det.

dump_stage(GeneralOpts, Name, ModuleName, Dump, !IO) :-
    Filename = format("%s/%s.plasma-dump_%s",
        [s(GeneralOpts ^ go_dir), s(q_name_to_string(ModuleName)), s(Name)]),
    io.open_output(Filename, OpenRes, !IO),
    ( OpenRes = ok(Stream),
        io.write_string(Stream, Dump, !IO),
        io.close_output(Stream, !IO)
    ; OpenRes = error(Error),
        format(io.stderr_stream, "%s: %s\n",
            [s(Filename), s(error_message(Error))], !IO),
        io.set_exit_status(1, !IO)
    ).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
