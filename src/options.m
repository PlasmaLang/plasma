%-----------------------------------------------------------------------%
% Plasma compiler options
% vim: ts=4 sw=4 et
%
% Copyright (C) 2015-2016 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
% The options structure for the Plasma compiler.
%
%-----------------------------------------------------------------------%
:- module options.
%-----------------------------------------------------------------------%

:- interface.

:- import_module string.

%-----------------------------------------------------------------------%

:- type compile_options
    --->    compile_options(
                % The directory of the input file.
                co_dir              :: string,
                co_input_file       :: string,
                co_output_file      :: string,
                co_dump_stages      :: dump_stages
            ).

:- type dump_stages
    --->    dump_stages
    ;       dont_dump_stages.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
