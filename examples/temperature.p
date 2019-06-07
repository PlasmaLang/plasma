/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module Temperature

export main

import io

func main() uses IO -> Int {
    run!(26)
    run!(38)
    run!(0)
    run!(100)
    run!(-40)
    return 0
}

func run(c : Int) uses IO {
    var f = c_to_f(c)
    print!(int_to_string(c) ++ "c is " ++ int_to_string(f) ++ "f\n")
}

func c_to_f(c : Int) -> Int {
    return c * 9 / 5 + 32
}

