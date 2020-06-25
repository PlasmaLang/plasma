/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module Vars_06

export
func main() uses IO -> Int {
    _ = foo(1)

    // It is an error to read from _.
    print!(int_to_string(_))

    return 0
}

func foo(n : Int) -> Int { return n + 3 }

