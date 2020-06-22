/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module Closure_02

import io

export
func main() uses IO -> Int {
    var greeting = "Hello "

    func hi(name : String) -> String {
        var msg = greeting ++ name ++ "\n"
        return msg
    }

    // We should be able to use this variable here, the above one isn't in
    // scope.
    var msg = hi("Paul")
    print!(msg)

    return 0
}

