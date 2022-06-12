/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module Module_06

import Module_06a

export
func main() uses IO -> Int {
    // The foo type is abstractly-exported, we should not be able to access
    // the constructor.
    var f = Module_06a.Foo(3)
    Module_06a.Foo(var n) = f

    return 0
}

