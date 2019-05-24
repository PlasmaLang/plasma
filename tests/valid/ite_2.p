/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module Ite_2

export main

import io

func main() uses IO -> Int {
    print!("fib(16) = " ++ int_to_string(fib(16)) ++ "\n")
    return 0
}

func fib(n : Int) -> Int {
    // Parens are optional
    if n == 0 {
        return 1
    } else if (n == 1) {
        return 1
    } else {
        return fib(n-1) + fib(n-2)
    }
}

