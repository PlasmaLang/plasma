/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module Builtin_01

entrypoint
func main() uses IO -> Int {
    print!(string_concat("abc", "def"))

    return 0
}

