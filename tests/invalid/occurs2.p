# vim: ft=plasma
# This is free and unencumbered software released into the public domain.
# See ../LICENSE.unlicense

module Occurs2

type Occurs(x) = Occurs ( v : x )

func occurs2(a : Occurs(o)) -> o {
    return a
}

