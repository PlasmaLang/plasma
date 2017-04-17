# vim: ft=plasma
# This is free and unencumbered software released into the public domain.
# See ../LICENSE.unlicense

module Vars_4

# An export list each symbol named by an export list is exported from the
# module.
export foo

func foo(a : Int, b : Int) -> Int {
    match (a) {
        1 -> { return 1 }
        b -> { return b*3 }
    }
}

