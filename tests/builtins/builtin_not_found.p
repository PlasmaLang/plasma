/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module BuiltinNotFound

entrypoint
func main() uses IO -> Int {
    // string_concat is a builtin, but it's not imported so this should
    // generate a compiler error.
    print!(string_concat("abc", "def"))

    return 0
}

