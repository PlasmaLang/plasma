/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module ClosureInfer1

export
func main() uses IO -> Int {
    var salutation = "G'day"

    func greet(name : String, opening : String) uses IO {
        print!(salutation ++ " " ++ name ++ " " ++ opening ++ "\n")
    }

    var opening = "How's it goin?"

    // Because closures are typechecked before their containing functions,
    // not enough type information is passed into this closure from the
    // outside and it has an ambigious type.
    func greet2(name : String) uses IO {
        greet!(name, opening)
    }

    greet2!("Paul")
    greet2!("James")

    return 0
}

