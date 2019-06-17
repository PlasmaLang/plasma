/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module Closure_05

export main

import io

func make_closure(drink : String) -> func() uses IO {
    func sing(n : Int) -> String {
        if (n == 0) {
            return "No more " ++ drink ++ "\n"
        } else {
            return int_to_string(n) ++ " bottles of " ++ drink ++ "...\n" ++
                sing(n - 1)
        }
    }

    func doit() uses IO {
        print!(sing(7))
    }

    return doit
}

func main() uses IO -> Int {
    var my_closure = make_closure("wine")

    my_closure!()

    return 0
}

