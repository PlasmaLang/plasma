/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module Closure

export main

import io

func main() uses IO -> Int {
    var greeting = "Hello "
    func hi(name : String) -> String {
        return greeting ++ name ++ "\n"
    }

    print!(hi("Paul"))

    return 0
}

