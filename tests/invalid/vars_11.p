/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module Vars_11

export main

import io

func main() uses IO -> Int {
    var x = 3
    var y
    match (x) {
        3 -> {
            var z = 4
        }
        var yy -> {
            y = yy * 26
        }
    }

    print!(int_to_string(z))
    return 0
}

