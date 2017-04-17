# vim: ft=plasma
# This is free and unencumbered software released into the public domain.
# See ../LICENSE.unlicense

module Vars_3

# An export list each symbol named by an export list is exported from the
# module.
export foo

func foo(a : Int, b : Int) -> Int {
    b = a * 3
    return b
}

