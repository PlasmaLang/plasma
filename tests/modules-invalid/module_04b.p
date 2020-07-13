/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module Module_04b

import Module_04import

export
func main() uses IO -> Int {
    // Mismatched number of parameters
    _ = Module_04import.someInt()

    return 0
}

