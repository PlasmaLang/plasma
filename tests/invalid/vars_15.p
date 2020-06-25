/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module Vars_15

export
func main() uses IO -> Int {
    var x
    var y = 2
    if (3 > y) {
        x = "Greater than"
    } else {
        x = -1
    }
    return 0
}

