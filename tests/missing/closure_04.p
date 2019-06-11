/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module Closure_04

export main

import io

func main() uses IO -> Int {
    var salutation = "G'day"

    func greet(name : String, opening : String) uses IO {
        print!(salutation ++ " " ++ name ++ " " ++ opening ++ "\n")
    }

    var opening = "How's it goin?"

    func greet2(name : String) uses IO {
        greet!(name, opening)
    }

    greet2!("Paul")
    greet2!("James")

    return 0
}

