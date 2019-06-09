/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module Closure_1

export main

import io

func main() uses IO -> Int {
    var greeting = "Hello "

    // The compiler crashs when we forget the return type for the closure.
    func hi(name : String) {
        return greeting ++ name ++ "\n"
    }

    print!(hi("Paul"))

    return 0
}

