/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module Module_04c

import Module_04import

export
func main() uses IO -> Int {
    // Mismatched arity
    Module_04import.someInt("Boo")

    return 0
}

