/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module Vars_12

export main

import io

func main() uses IO -> Int {
    var x = 3
    match (x) {
        3 -> {
            var z = 4
        }
        yy -> {
            var z = yy * 26
        }
    }

    print!(int_to_string(z))
    return 0
}

