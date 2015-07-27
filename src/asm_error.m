%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module asm_error.
%
% Error type for assembler.
%
% Copyright (C) 2015 Paul Bone
% Distributed under the terms of the GPLv2 see ../LICENSE.tools
%
%-----------------------------------------------------------------------%

:- interface.

:- import_module string.

:- import_module result.

%-----------------------------------------------------------------------%

:- type asm_error
    --->    e_io_error(string)
    ;       e_tokeniser_error(string)
    ;       e_parse_error(
                epe_expecting   :: string,
                epe_got         :: string
            )
    ;       e_parse_error_eof(
                epee_expecting  :: string
            )
    ;       e_parse_error_other(
                epeo_message    :: string
            )
    ;       e_name_already_defined(string).

:- instance error(asm_error).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

:- import_module list.

%-----------------------------------------------------------------------%

:- instance error(asm_error) where [
        func(to_string/1) is asme_to_string,
        func(error_or_warning/1) is asme_error_or_warning
    ].

:- func asme_to_string(asm_error) = string.

asme_to_string(e_io_error(Message)) =
    format("IO Error, %s", [s(Message)]).
asme_to_string(e_tokeniser_error(Message)) =
    format("Tokeniser error, %s", [s(Message)]).
asme_to_string(e_parse_error(Expecting, Got)) =
    format("Expected %s, read %s during parsing.", [s(Expecting), s(Got)]).
asme_to_string(e_parse_error_eof(Expecting)) =
    format("Unexpected EOF while expecting %s", [s(Expecting)]).
asme_to_string(e_parse_error_other(Message)) =
    format("Parse error %s", [s(Message)]).
asme_to_string(e_name_already_defined(Name)) =
    format("\"%s\" is already defined", [s(Name)]).

:- func asme_error_or_warning(asm_error) = error_or_warning.

asme_error_or_warning(_) = error.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
