# vim: ft=plasma
# This is free and unencumbered software released into the public domain.
# See ../LICENSE.unlicense

module Arity_HO_2

export main

import io

func main() uses IO -> Int {
    f = fst(pm, 3)
    
    return 0
}

func fst(f : func(Int) -> (Int, Int), input : Int) -> Int {
    # Incorrect arity in call to higher-order function.
    a = f(input)
    return a
}

# A function that returns two things.
func pm(x : Int) -> (Int, Int) {
    if (x < 0) {
        x_abs = x * -1
    } else {
        x_abs = x
    }
    return x_abs, x_abs * -1
}

