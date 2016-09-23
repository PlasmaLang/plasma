# vim: ft=plasma
# This is free and unencumbered software released into the public domain.
# See ../LICENSE.unlicense

module Match_1 

export main

import io

func main() -> Int using IO {
    print!("fib(16) = " ++ int_to_string(fib(16)) ++ "\n")
    return 0
}

# Or, using pattern matching:
func fib(n :: Int) -> Int {
    match (n) {
        0 -> {
            return 1
        }
        1 -> {
            return 1
        }
        # Any symbols here must be constructor symbols or free variables.
        m -> {
            return fib(m-1) + fib(m-2)
        }
    }
}

