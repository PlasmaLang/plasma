/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module OpaqueResource2

import OpaqueResource as OR

entrypoint
func test() uses IO -> Int {
    var s = test2!("Bob")
    print!(s ++ "\n")

    return 0
}

func test2(s : String) uses OR.Res1 -> String {
    // Calling test3 directly is illegal. although Res2 comes from Res1 this
    // module doesn't know that because Res2 is opaque.
    return test3!(s)
}

func test3(s : String) uses OR.Res2 -> String {
    return "Hi " ++ s ++ "."
}

