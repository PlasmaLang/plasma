/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module Module_02

// The import declaration works, it causes the interface file to be read.
import Module_02a

export
func is_odd(n : Int) -> Bool {
    if (n == 0) {
        return False
    } else {
        return Module_02a.is_even(n - 1)
    }
}

entrypoint
func main() uses IO -> Int {
    print!("is_odd(0) = " ++ bool_to_string(is_odd(0)) ++ "\n")
    print!("is_odd(1) = " ++ bool_to_string(is_odd(1)) ++ "\n")
    print!("is_odd(2) = " ++ bool_to_string(is_odd(2)) ++ "\n")
    print!("is_odd(34) = " ++ bool_to_string(is_odd(34)) ++ "\n")
    print!("is_odd(35) = " ++ bool_to_string(is_odd(35)) ++ "\n")
    return 0
}

