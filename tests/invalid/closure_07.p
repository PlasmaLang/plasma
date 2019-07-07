/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module Closure_07

export main

import io

func main() uses IO -> Int {
    var greeting = "Hello "

    var msg = "quack!"

    // The compiler crashs when we forget the return type for the closure.
    func hi(name : String) -> String {
        msg= greeting ++ name ++ "\n"
        return msg
    }

    print!(hi("Paul"))

    // msg wont be available here
    print!(msg)

    return 0
}
