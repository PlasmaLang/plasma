/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module Module_07

import Module_07a

export
func main() uses IO -> Int {
    var f = Module_07a.makeFoo(28)
    print!("foo is: " ++ Module_07a.fooStr(f) ++ "\n")

    return 0
}

