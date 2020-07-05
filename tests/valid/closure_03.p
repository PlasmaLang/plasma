/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module Closure_03

export
func main() uses IO -> Int {
    var salutation = "G'day"

    func greet(name : String) uses IO {
        print!(salutation ++ " " ++ name)
        // Try to trick the compiler with two bang statements inside the one
        // closure which is itself a single statement.
        print!("\n")
    }

    greet!("Paul")
    greet!("James")

    return 0
}

