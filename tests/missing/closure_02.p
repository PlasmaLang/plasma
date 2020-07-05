/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module Closure_02

func make_is_odd() -> func(Int) -> Bool {
    func is_odd(n : Int) -> Bool {
        if (n == 0) {
            return False
        } else {
            return is_even(n-1)
        }
    }
    func is_even(n : Int) -> Bool {
        if (n == 0) {
            return True
        } else {
            return is_odd(n-1)
        }
    }

    return is_odd
}

export
func main() uses IO -> Int {
    func odd_or_even(n : Int) -> String {
        var is_odd = make_is_odd()
        if (is_odd(n)) {
            return "odd"
        } else {
            return "even"
        }
    }

    var n = 23
    print!(int_to_string(n) ++ " is " ++ odd_or_even(n))

    return 0
}

