/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module Module_06

import Module_06a

export
func main() uses IO -> Int {
    var s = test1!()
    print!(s ++ "\n")
    return 0
}

func test1() uses Module_06a.Foo -> String {
    Module_06a.troz!()
    return zort!()
}

resource Zort from Module_06a.Bar

func zort() uses Zort -> String {
    return "zort"
}

