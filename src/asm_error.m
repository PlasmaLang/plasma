%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module asm_error.
%
% Error type for assembler.
%
% Copyright (C) 2015 Paul Bone
% All rights reserved
%
%-----------------------------------------------------------------------%

:- interface.

%-----------------------------------------------------------------------%

:- type asm_error
    --->    e_io_error(string)
    ;       e_tokeniser_error(string)
    ;       e_parse_error(
                epe_expecting   :: string,
                epe_got         :: string
            )
    ;       e_name_already_defined(string).

