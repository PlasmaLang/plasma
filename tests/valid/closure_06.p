/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module Closure_06

func phrase1(drink : String) -> String {
    return "No more " ++ drink ++ "\n"
}

func make_closure(drink : String) -> func(Int) uses IO {
    func sing(n : Int) uses IO {
        if (n == 0) {
            // The compiler will generate a call that does not set the
            // environment.
            print!(phrase1(drink))
        } else {
            print!(int_to_string(n) ++ " bottles of " ++ drink ++
                "...\n")
            sing!(n - 1)
        }
    }

    return sing 
}

entrypoint
func main() uses IO -> Int {

    var my_closure = make_closure("wine")

    my_closure!(4)

    return 0
}

