/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module Closure_05

// Wrap this in a function to help the typechecker.
func lines() -> Int {
    return 4
}

entrypoint
func main() uses IO -> Int {

    func phrase1(drink : String) -> String {
        return "No more " ++ drink ++ "\n"
    }
    func make_closure(drink : String) -> func() uses IO {
        func sing(n : Int) -> String {
            if (n == 0) {
                return phrase1(drink)
            } else {
                return int_to_string(n) ++ " bottles of " ++ drink ++ "...\n" ++
                    sing(n - 1)
            }
        }

        func doit() uses IO {
            print!(sing(lines()))
        }

        return doit
    }

    var my_closure = make_closure("wine")

    my_closure!()

    return 0
}

