/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module Closure_08

import io

export
func main() uses IO -> Int {
    var salutation = "G'day"

    func greet(name : String) uses IO {
        print!(salutation ++ " " ++ name ++ "\n")
    }

    greet("Paul")

    return 0
}

