/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module Module_04d

import Module_04import

export
func main() uses IO -> Int {
    _ = test()

    return 0
}

func test() -> Int {
    // Mismatched resources
    Module_04import.someAction!()

    return 1
}


