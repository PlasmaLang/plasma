/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module Closure_10

import io

export
func main() uses IO -> Int {
    var salutation = "G'day"

    func greet(name : String) -> Int {
        print!(salutation ++ " " ++ name ++ "\n")
        return 3
    }

    _ = greet("Paul")

    return 0
}

