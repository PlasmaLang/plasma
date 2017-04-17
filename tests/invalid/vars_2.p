# vim: ft=plasma
# This is free and unencumbered software released into the public domain.
# See ../LICENSE.unlicense

# Hello module declaration, this gives the name of the module.
module Vars_2

# An export list each symbol named by an export list is exported from the
# module.
export foo

func foo(a : Int, a : Int) -> Int {
    return a + a
}

