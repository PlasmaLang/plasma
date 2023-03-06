/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module OpaqueResource1

import OpaqueResource as OR

entrypoint
func test() uses IO -> Int {
    var s = test2!("Bob")
    print!(s ++ "\n")

    return 0
}

func test2(s : String) uses OR.Res1 -> String {
    // The only way to get Res2 is by calling into the module that can
    // access it.
    return OR.do_with_res!(test3, s)
}

func test3(s : String) uses OR.Res2 -> String {
    return "Hi " ++ s ++ "."
}

