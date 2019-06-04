/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module Vars_10 

export main

import io

func main() uses IO -> Int {
    var x = 3
    var y
    if (x == 4) {
        y = 2
    } else {
        var yy = "f"
    }

    y = 3

    print!(int_to_string(y))
    return 0
}

