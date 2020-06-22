/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module Match_2

import io

export
func main() uses IO -> Int {
    var x = 3
    var y
    match (x) {
        3 -> {
            y = 2
            return 4
        }
        var yy -> {
            y = yy * 26
        }
    }

    print!(int_to_string(y))
    return 0
}

