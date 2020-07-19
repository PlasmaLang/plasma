/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module Module_04a

import Module_04import

export
func main() uses IO -> Int {
    // Mismatched type in imported function's use.
    _ = Module_04import.someInt(3)

    return 0
}

