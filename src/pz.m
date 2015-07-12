
:- module pz.

:- interface.

:- import_module io.
:- import_module string.

:- type pz.

:- pred read_pz(string::in, pz::out, io::di, io::uo) is det.

:- implementation.

:- type pz
    ---> pz.

read_pz(_, pz, !IO).

