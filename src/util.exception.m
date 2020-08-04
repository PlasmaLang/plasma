%-----------------------------------------------------------------------%
% Exception utility code
% vim: ts=4 sw=4 et
%
% Copyright (C) 2015-2020 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
%-----------------------------------------------------------------------%
:- module util.exception.

:- interface.

:- import_module io.
:- import_module maybe.
:- import_module context.

%-----------------------------------------------------------------------%

    % This exception and its routines are temporary, they should be used for
    % code that finds a compilation error, but error handling is not
    % properly setup in that area of the compiler.  This helps by making
    % these errors a little more friendly, and by allowing us to search the
    % source code for these locations when we eventually add error handling.
    %
:- type compile_error_exception
    --->    compile_error_exception(string, string, maybe(context), string).

:- pred compile_error(string::in, string::in, string::in) is erroneous.

:- pred compile_error(string::in, string::in, context::in, string::in)
    is erroneous.

    % This is an alternative to the sorry/1 predicate in the Mercury
    % standard library.  This predicate uses a dedicated exception type and
    % is caught explicitly by plasmac's main/2 predicate.
    %
:- type unimplemented_exception
    --->    unimplemented_exception(string, string, string).

:- pred sorry(string::in, string::in, string::in) is erroneous.

:- func sorry(string, string, string) = T.
:- mode sorry(in, in, in) = (out) is erroneous.

    % Like sorry except that these exceptions are used for things we think
    % are unlikely.  Like trying to roll more than 256 items on the PZ
    % stack.  If they happen to real people then we'll try to address them
    % and can probably do something about them.
    %
:- type design_limitation_exception
    --->    design_limitation_exception(string, string, string).

:- pred limitation(string::in, string::in, string::in) is erroneous.

% TODO: add "unexpected" exception.

:- type tool
    --->    plzc
    ;       plzasm.

:- type had_errors
    --->    had_errors
    ;       did_not_have_errors.

:- pred run_and_catch(pred(io, io), tool, had_errors, io, io).
:- mode run_and_catch(pred(di, uo) is det, in, out, di, uo) is cc_multi.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
:- implementation.

:- import_module exception.
:- import_module list.
:- import_module pair.
:- import_module string.

%-----------------------------------------------------------------------%

compile_error(File, Pred, Message) :-
    throw(compile_error_exception(File, Pred, no, Message)).

compile_error(File, Pred, Context, Message) :-
    throw(compile_error_exception(File, Pred, yes(Context), Message)).

sorry(File, Pred, Message) :-
    throw(unimplemented_exception(File, Pred, Message)).
sorry(File, Pred, Message) = _ :-
    util.exception.sorry(File, Pred, Message).

limitation(File, Pred, Message) :-
    throw(design_limitation_exception(File, Pred, Message)).

%-----------------------------------------------------------------------%

run_and_catch(Run, Tool, HadErrors, !IO) :-
    ( try [io(!IO)] (
        Run(!IO)
    ) then
        HadErrors = did_not_have_errors
    catch compile_error_exception(File, Pred, MbCtx, Msg) ->
        HadErrors = had_errors,
        Description =
"A compilation error occured and this error is not handled gracefully\n" ++
"by the " ++ tool_name(Tool) ++ ". Sorry.",
        ShortName = tool_short_name(Tool),
        ( MbCtx = yes(Ctx),
            print_exception(Description,
                ["Message"                  - Msg,
                 "Context"                  - context_string(Ctx),
                 (ShortName ++ " location") - Pred,
                 (ShortName ++ " file")     - File],
                !IO)
        ; MbCtx = no,
            print_exception(Description,
                ["Message"                  - Msg,
                 (ShortName ++ " location") - Pred,
                 (ShortName ++ " file")     - File],
                !IO)
        )
    catch unimplemented_exception(File, Pred, Feature) ->
        HadErrors = had_errors,
        print_exception(
"A feature required by your program is currently unimplemented,\n" ++
"however this is something we hope to implement in the future. Sorry\n",
            ["Feature"  - Feature,
             "Location" - Pred,
             "File"     - File],
            !IO)
    catch design_limitation_exception(File, Pred, Message) ->
        HadErrors = had_errors,
        print_exception(
"This program pushes Plasma beyond what it is designed to do. If this\n" ++
"happens on real programs (not a stress test) please contact us and\n" ++
"we'll do what we can to fix it.",
        ["Message"  - Message,
         "Location" - Pred,
         "File"     - File],
        !IO)
    catch software_error(Message) ->
        HadErrors = had_errors,
        print_exception(
"The " ++ tool_name(Tool) ++
    " has crashed due to a bug (an assertion failure or\n" ++
"unhandled state). Please make a bug report. Sorry.",
            ["Message" - Message], !IO)
    ).

:- pred print_exception(string::in, list(pair(string, string))::in,
    io::di, io::uo) is det.

print_exception(Message, Fields, !IO) :-
    write_string(stderr_stream, Message, !IO),
    io.nl(!IO),
    foldl(exit_exception_field, Fields, !IO).

:- pred exit_exception_field(pair(string, string)::in, io::di, io::uo)
    is det.

exit_exception_field(Name - Value, !IO) :-
    write_string(pad_right(Name ++ ": ", ' ', 20), !IO),
    write_string(Value, !IO),
    nl(!IO).

:- func tool_name(tool) = string.

tool_name(plzc) = "Plasma compiler".
tool_name(plzasm) = "Plasma bytecode assembler".

:- func tool_short_name(tool) = string.

tool_short_name(T) = string(T).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
