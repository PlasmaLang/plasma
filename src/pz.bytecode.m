%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module pz.bytecode.
%
% Common code for reading or writing PZ bytecode.
%
% Copyright (C) 2015 Paul Bone
% Distributed under the terms of the GPLv2 see ../LICENSE.tools
%
%-----------------------------------------------------------------------%

:- interface.

:- import_module int.
:- import_module string.

%-----------------------------------------------------------------------%

:- type plz0_tag
    --->    pt_magic.

:- pred plz0_tag_id(plz0_tag, int).
:- mode plz0_tag_id(in, out) is det.
% :- mode plz0_tag_id(out, in) is semidet.

:- func plz0_id_string = string.

:- func plz0_version = int.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

:- import_module list.

:- pragma foreign_decl("C",
"
#include ""pz_format.h""
").

plz0_tag_id(pt_magic,       magic_num).

:- func magic_num = int.

:- pragma foreign_proc("C",
    magic_num = (X::out),
    [will_not_call_mercury, thread_safe, promise_pure],
    "X = PZ_MAGIC_TAG;").

%-----------------------------------------------------------------------%

plz0_id_string =
    format("%s version %d", [s(id_string_part), i(plz0_version)]).

:- func id_string_part = string.

:- pragma foreign_proc("C",
    id_string_part = (X::out),
    [will_not_call_mercury, thread_safe, promise_pure],
    "
    /*
     * Cast away the const qualifier, Mercury won't modify this string
     * because it does not have a unique mode.
     */
    X = (char*)PZ_MAGIC_STRING_PART;
    ").

%-----------------------------------------------------------------------%

:- pragma foreign_proc("C",
    plz0_version = (X::out),
    [will_not_call_mercury, thread_safe, promise_pure],
    "X = PZ_FORMAT_VERSION;").

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
