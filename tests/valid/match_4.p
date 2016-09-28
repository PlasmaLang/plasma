# vim: ft=plasma
# This is free and unencumbered software released into the public domain.
# See ../LICENSE.unlicense

module Match_4

export main

import io

func main() -> Int using IO {
    print!("fib(16) = " ++ int_to_string(fib(16)) ++ "\n")
    return 0
}

func fib(n :: Int) -> Int {
    match (n) {
        0 -> {
            r = 1
        }
        1 -> {
            m = "fish" 
            r = 1
        }
        m -> {
            r = fib(m-1) + fib(m-2)
        }
    }

    return r
}

