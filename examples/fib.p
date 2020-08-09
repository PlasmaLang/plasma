/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

/*
 * This example shows conditional statements and expressions, using
 * if-then-elses and pattern matches.
 */

module Fib

export
func main() uses IO -> Int {
    var n = 16 

    var n_str = int_to_string(n)
    func label(m : Int) -> String {
        return "fib" ++ int_to_string(m) ++ "(" ++ n_str ++ ") = "
    }

    print!(label(1) ++ int_to_string(fib1(n)) ++ "\n")
    print!(label(2) ++ int_to_string(fib2(n)) ++ "\n")
    print!(label(3) ++ int_to_string(fib3(n)) ++ "\n")
    print!(label(4) ++ int_to_string(fib4(n)) ++ "\n")
    print!(label(5) ++ int_to_string(fib5(n)) ++ "\n")
    print!(label(6) ++ int_to_string(fib6(n)) ++ "\n")
    return 0
}

func fib1(n : Int) -> Int {
    if (n <= 1) {
        return 1
    } else {
        return fib1(n-1) + fib1(n-2)
    }
}

// Or branches can set a variable:
func fib2(n : Int) -> Int {
    var r
    if (n <= 1) {
        r = 1
    } else {
        r = fib2(n-1) + fib2(n-2)
    }
    return r
}

// Or if-then-else can be an expression:
func fib3(n : Int) -> Int {
    return if (n <= 1) then 1 else fib3(n-1) + fib3(n-2)
}

// Or, using pattern matching:
func fib4(n : Int) -> Int {
    match (n) {
        0 -> {
            return 1
        }
        1 -> {
            return 1
        }
        // Any symbols here must be constructor symbols or free variables.
        var m -> {
            return fib4(m-1) + fib4(m-2)
        }
    }
}

// Or, using pattern matching that sets a value:
func fib5(n : Int) -> Int {
    var r
    match (n) {
        0 -> {
            r = 1
        }
        1 -> {
            r = 1
        }
        // Any symbols here must be constructor symbols or free variables.
        var m -> {
            r = fib5(m-1) + fib5(m-2)
        }
    }

    return r
}

// Or, pattern matching can be an expression.
func fib6(n : Int) -> Int {
    return match (n) {
        0 -> 1
        1 -> 1
        // Any symbols here must be constructor symbols or free variables.
        var m -> fib6(m-1) + fib6(m-2)
    }
}

// // Pattern matching can also include guards.
// func fib7(n : Int) -> Int {
//     return match (n) {
//         m | m < 2     -> 1
//         m | otherwise -> fib7(m-1) + fib7(m-2)
//     }
// }

