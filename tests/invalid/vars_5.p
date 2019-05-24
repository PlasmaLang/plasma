/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module Vars_5

export foo

func foo(a : Int, b : Int) -> Int {
    match (a) {
        1 -> { return 1 }
        b -> { return b*3 }
    }
}

