%-----------------------------------------------------------------------%
% Write a Plasma interface file
% vim: ts=4 sw=4 et
%
% Copyright (C) Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
% This module provides the code for writing out an interface file.
%
%-----------------------------------------------------------------------%
:- module write_interface.
%-----------------------------------------------------------------------%

:- interface.

:- import_module io.
:- import_module maybe.
:- import_module string.

:- import_module core.

:- pred write_interface(string::in, core::in, maybe_error::out,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
:- implementation.

:- import_module cord.
:- import_module list.
:- import_module pair.

:- import_module common_types.
:- import_module core.function.
:- import_module core.pretty.
:- import_module core.types.
:- import_module core.resource.
:- import_module q_name.
:- import_module util.
:- import_module util.my_io.
:- import_module util.pretty.

%-----------------------------------------------------------------------%

write_interface(Filename, Core, Result, !IO) :-
    PrettyStr = pretty_str([pretty_interface(Core)]),
    write_temp_and_move(open_output, close_output,
        (pred(File::in, ok::out, IO0::di, IO::uo) is det :-
            io.write_string(File, PrettyStr, IO0, IO)
        ), Filename, Result, !IO).

:- func pretty_interface(core) = pretty.

pretty_interface(Core) = Pretty :-
    ModuleName = q_name_to_string(module_name(Core)),
    ExportedResources = core_all_exported_resources(Core),
    ExportedTypes = core_all_exported_types(Core),
    ExportedFuncs = core_all_exported_functions(Core),
    Pretty = p_list([
        p_str("// Plasma interface file"), p_nl_hard,
        p_str("module"), p_spc, p_str(ModuleName), p_nl_double] ++
        condense(map(pretty_resource_interface(Core), ExportedResources)) ++
        condense(map(pretty_type_interface(Core), ExportedTypes)) ++
        condense(map(pretty_func_interface(Core), ExportedFuncs))).

:- func pretty_resource_interface(core, pair(resource_id, resource)) =
    list(pretty).

pretty_resource_interface(Core, _ - R) =
    [resource_decl_pretty(Core, R), p_nl_double].

:- func pretty_type_interface(core, pair(type_id, user_type)) = list(pretty).

pretty_type_interface(Core, _ - Type) = Pretty :-
    Pretty = [type_decl_pretty(Core, Type), p_nl_double].

:- func pretty_func_interface(core, pair(func_id, function)) = list(pretty).

pretty_func_interface(Core, _ - Func) = Pretty :-
    Pretty = [p_expr(func_decl_pretty(Core, Func)), p_nl_double].

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
