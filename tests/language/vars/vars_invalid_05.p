/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module VarsInvalid_05

export 
func foo(a : Int, b : Int) -> Int {
    match (a) {
        1 -> { return 1 }
        var b -> { return b*3 }
    }
}

