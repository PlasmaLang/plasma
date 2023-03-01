/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module VarsInvalid_14

export
func main() uses IO -> Int {
    var x
    func foo() uses IO {
        print!(int_to_string(x))
    }
    return 0
}

